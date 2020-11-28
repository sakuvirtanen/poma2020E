library(shiny)
library(shinyWidgets)
library(shinyFiles)
library(htmlTable)
library(fs)
library(shinydashboard)
library(ggplot2)
library(readxl)
library(dplyr)
library(reshape2)
library(DT)
library(kableExtra)
source("scripts.R")

ui <- fluidPage(style="background-color:#FFFFFF;",
  setBackgroundImage(
    src = ""
  ),
  includeCSS("styles.css"),
  
  titlePanel(
    h1("Portfolio Management Tool")
  ),
  
  
  sidebarLayout(
    sidebarPanel(style="background-color:#DDDDDD;",
      
      fluidRow(
        column(12,h2("Select Tickers"),style="background-color:#DDDDDD;",
          
          fluidRow(
        
            column(12,shinyFilesButton("stockFile","File select", "Select your stock data file", multiple=FALSE, viewtype="detail")),
            
            column(12,textOutput("stockFilePath")),
            
            column(12,searchInput(inputId="tickerSearch",
                        label="Search stock data by ticker",
                        placeholder = "",
                        btnSearch = icon("search"), 
                        btnReset = icon("remove"),
                        width = "100%")),
            
            column(12,uiOutput("tickerSearchRes")),
            
            column(12,searchInput(inputId="isinSearch",
                                  label="Search bonds by ISIN",
                                  placeholder = "",
                                  btnSearch = icon("search"), 
                                  btnReset = icon("remove"),
                                  width = "100%")),
            
            column(12,uiOutput("isinSearchRes")),
          )
        )
      ),
      
      
      fluidRow(
        
        column(12,h2("Asset Allocation"),style="background-color:#DDDDDD;",
          fluidRow(
            
            column(12,numericInput(inputId = "notional",
                                  label = "Notional portfolio value at beginning:",
                                  value = 1000000,min = 1,max = 2000000)),
          
            column(width=6,numericInput(inputId = "stockweight",
                                  label = "Stocks (%):",
                                  value = 100,min = 0,max = 100)),
            
            column(width=6,numericInput(inputId = "bondweight",label = "Bonds (%):",
                                  value = 0,min = 0,max = 100)),
            
          )
        )
        

      ),
      
      fluidRow(
        column(12,h2("Simulation specs"),style="background-color:#DDDDDD;",
                      
          fluidRow(
            
            column(6,sliderInput(inputId = "months",
                   label = "Number of months to simulate:",
                   value = 10,min = 1,max = 60)),
            
            column(6,sliderInput("slide","Number of simulations",
                                  min=0,max=1000,value=20)),
    
      
            column(12,dateRangeInput(inputId = "dates",
                     "Choose the start and end date",
                     start = "2018-10-01",
                     end = "2019-10-01",)),
          )
        )
      ), 
      
      numericInput(inputId = "var",
                   label = "Value at risk:",
                   value = 5,min = 0,max = 10),
      
      actionButton("button",label="Run simulation")
      
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel('Portfolio',
          tabsetPanel(
            tabPanel("Stocks",dataTableOutput("stockTable"),icon=icon("suitcase")),
            tabPanel("Bonds", dataTableOutput("bondTable"), icon=icon("suitcase"))
            # tabPanel("Assets",uiOutput("portfolioStocks")),
            #tabPanel("Bonds",dataTableOutput("dataTest"),actionButton("deleteLastStock","Delete last"),icon=icon("suitcase"))
          )
        ),
        tabPanel('Simulation results',
          fluidRow(
              column(6,tabsetPanel(
                    tabPanel("Price path",plotOutput("pricepath"),icon = icon("chart-line")),
                    tabPanel("Return distribution",plotOutput("tuottojakauma1"),icon = icon("bar-chart-o")),
                    tabPanel("Market cap distribution",plotOutput("tuottojakauma2"),icon = icon("bar-chart-o"))
              )),
           

              column(6,tabsetPanel(
                    tabPanel("Statistics",tableOutput("dist_stats"))
              ))
              
          )
          )
        )
      )
    )
    
  
)

server <- function(input,output,session) {
  
  # Give running directory as only volume accessible for the file chooser.
  volumes <- c(wd = ".")
  # Server side function for rendering the file chooser. Note that this takes the additional session variable given
  # to the server function
  shinyFileChoose(input, "stockFile", roots = volumes, filetypes=c("", "xlsx", "xls"), session = session)
  
  simResults <- reactiveValues()
  stockPortfolio <- reactiveValues()
  portfoliodf <- reactiveValues(stockData = data.frame(
    Ticker = c(),
    Weight = c(),
    Delete= c(),
    stringsAsFactors = FALSE), bondData = data.frame(
      ISIN = c(),
      Weight = c(),
      Delete = c(),
      stringsAsFactors = FALSE
    ))
  
  #  Create a list for storing the event handlers for individual search result buttons
  tickObsList <- list()
  # Event handler for ticker search events
  stockSearchButtons <- eventReactive(input$tickerSearch_search,
  {
    # Search by grepping elements from the first row of the stock data file
    # The first row must contain the tickers for individual securities in the file.
    resultButtons <-  grep(input$tickerSearch,names(read_excel(parseFilePaths(
      volumes,input$stockFile)[["datapath"]], sheet="Returns")),ignore.case=TRUE,value=TRUE)
    
    # Limit the maximum number of buttons rendered to a sensible number.
    # This feature now requires pagination, this needs to be added.
    renderCount <- pmin(6,length(resultButtons))
    
    # Iterate through resultButtons to create fluidRow() elements with
    # actionButton()'s in them for each search result.
    resultButtons <- lapply(resultButtons[1:renderCount], function(i)
    {
      btName <- toString(i)
      # print(btName)
      if (is.null(tickObsList[[btName]])) {
        tickObsList[[btName]] <<- observeEvent(input[[btName]], {
          portfoliodf$stockData <- rbind(portfoliodf$stockData,data.frame(
            Ticker = btName,
            Weight = 1,
            Delete = shinyInput(actionButton, nrow(portfoliodf$stockData)+1, 'button_', label = "Delete", onclick = 'Shiny.onInputChange(\"stockDel_button\",  this.id)' ),
            row.names = nrow(portfoliodf$stockData)+1
          ))
          # print(stockPortfolio$selected[btName])
          # print(stockPortfolio$selected[btName])
          # print(btName)
          # print(resultButtons)
        },autoDestroy = TRUE)
        
      }
      fluidRow(
        column(12,actionButton(btName,btName))
        
      )
    })
  }
  )
  
  #  Create a list for storing the event handlers for individual search result buttons
  isinObsList <- list()
  # Event handler for ticker search events
  bondSearchButtons <- eventReactive(input$isinSearch_search, {
    # Search by grepping elements from the first row of the stock data file
    # The first row must contain the tickers for individual securities in the file.
    resultButtons <-  grep(input$isinSearch,unique(read_excel("data/finnish_corporate_bonds.xlsx", sheet="Sheet1")[["ISIN"]]),ignore.case=TRUE,value=TRUE)
    
    # Limit the maximum number of buttons rendered to a sensible number.
    # This feature now requires pagination, this needs to be added.
    renderCount <- pmin(6,length(resultButtons))
    
    # Iterate through resultButtons to create fluidRow() elements with
    # actionButton()'s in them for each search result.
    resultButtons <- lapply(resultButtons[1:renderCount], function(i)
    {
      btName <- toString(i)
      # print(btName)
      if (is.null(isinObsList[[btName]])) {
        isinObsList[[btName]] <<- observeEvent(input[[btName]], {
          portfoliodf$bondData <- rbind(portfoliodf$bondData, data.frame(
            ISIN = btName,
            Weight = 1,
            Delete = shinyInput(actionButton, nrow(portfoliodf$bondData)+1, 'button_', label = "Delete", onclick = 'Shiny.onInputChange(\"bondDel_button\",  this.id)' ),
            row.names = nrow(portfoliodf$bondData)+1
          ))
          print(portfoliodf$bondData)
          # print(stockPortfolio$selected[btName])
          # print(stockPortfolio$selected[btName])
          # print(btName)
          # print(resultButtons)
        },autoDestroy = TRUE)
        
      }
      fluidRow(
        actionButton(btName,btName)
        
      )
    })
  }
  )


  pricePaths <- eventReactive(simResults$stockOnly, {
    
    selectedTickers <- c()

    for (s in stockPortfolio$tickers) {
      selectedTickers <- c(selectedTickers,s[1])
    }
    
    # Margin adjustment
    par(mar = 5*c(1,1,1,1))
    
    # Set time frame:
    T = 0:input$months
    # Set y axis min and max from simulated results:
    y_scale = c(min(simResults$stockOnly[,input$months])*input$notional,max(simResults$stockOnly[,input$months])*input$notional)
    # Axis labels:
    y_lab = "Market Cap"
    x_lab = "Months from beginning"
    
    # Set title and subtitle:
    title_ = "Price path simulation"
    sub_ = paste(selectedTickers, collapse = ", ")
    
    # Merge title and subtitle:
    full_title = paste(title_, "\n",sub_)

    
    stock_df = as.matrix(simResults$stockOnly)
    stock_df = as.data.frame(t(stock_df))
    # print(length(stock_df))
    
    #means = colMeans(simResults$stockOnly)*input$notional
    
    stock_df$Month = seq(1,input$months+1)
    #stock_df$Means = cMeans(simResults$stockOnly) #*input$notional
    # print(stock_df)
    
    d = stock_df
    
    
    #print(length(means))
    d <- melt(d, id.vars="Month")
    #print(d)
    # Everything on the same plot
    print(stock_df)
    ggplot(d, aes(Month,value, col=variable)) + 
      geom_line() + 
      ggtitle("Simulated Price Paths") +
      theme_light() +
      theme(legend.title = element_blank()) +
      theme(legend.position = "none")
      
      #geom_line(aes(x=Month,y=Means))
    
    # First path plot:
    #plot(T, simResults$stockOnly[1,]*input$notional, main = full_title, ylim = y_scale, type = "l", xlab = x_lab, ylab = y_lab)
    
    # Plot rest with lines:
    #for (i in seq(1,input$slide,1)) {
    #  lines(T, simResults$stockOnly[i,]*input$notional)
    #}
    
    # Find mean path:
    #means = colMeans(simResults$stockOnly)
    
    # Plot mean path:
    #lines(T,means*input$notional, col = "#FF0000")
  })
  
  return_histogram <- eventReactive(simResults$withBonds,{
    histData <- simResults$withBonds
    Scaled_return = histData[,input$months+1]/histData[,1]*100-100
    VaR_q = quantile(Scaled_return, probs = c(input$var/100))*input$notional/100
    subtitle = paste(input$slide, " simulations, ", input$months, " steps" , ", VaR ", input$var/100,"%:" , signif(VaR_q, digits = 3))
    f = data.frame(Scaled_return)
    ggplot(f,aes(x=Scaled_return)) + geom_histogram()
    #hist(Scaled_return, main = input$tickers, sub = subtitle, xlab = "Cumulative return (%)", xlim = c(-100,200), breaks = 15)
  })

  marketcap_histogram <- eventReactive(input$button, {
    histData <- simResults$withBonds
    Scaled_return = histData[,input$months+1]/histData[,1]*100-100
    # Normalized values at end of period:
    Scaled = histData[,input$months+1]
    Cap = Scaled * input$notional
    VaR_q = quantile(Scaled_return, probs = c(input$var/100))*input$notional/100
    subtitle = paste(input$slide, " simulations, ", input$months, " steps" , ", VaR ", input$var/100,"%:" , signif(VaR_q, digits = 3))
    f = data.frame(Cap)
    ggplot(f,aes(x=Cap)) + geom_histogram()
    #hist(Cap, main = input$tickers, xlab = "Market value (Eur)", xlim = c(0,3*input$notional), breaks = 15)
  })
  
  
  observeEvent(input$button, {
    selectedTickers <- c()
    selectedWeights <- c()
    
    for (s in stockPortfolio$tickers) {
      selectedTickers <- c(selectedTickers,s[1])
    }
    
    for (s in stockPortfolio$weights) {
      selectedWeights <- c(selectedWeights,s[1])
    }
    
    # print(length(selectedTickers))
    simResults$stockOnly <- Simulate_Stocks(portfoliodf$stockData[["Ticker"]], format(as.Date(input$dates[1]), "%Y-%m"), format(as.Date(input$dates[2]), "%Y-%m"), input$months, input$slide, matrix(portfoliodf$stockData[["Weight"]], nrow = 1),input$notional,input$stockweight,input$bondweight)
  })
  
  observeEvent(simResults$stockOnly, {
    # print("A stock simulation just got created! Now invoking bond simulations")
    datecheck <- checkDates(input$dates[1],input$dates[2],portfoliodf$bondData[["ISIN"]])
    
    print(datecheck$possible)
    
    if (datecheck["possible"] == TRUE) {
      print(portfoliodf$bondData[["ISIN"]])
    simResults$withBonds <- Simulate_Bonds(portfoliodf$bondData[["ISIN"]],format(as.Date(input$dates[1]), "%Y-%m"), format(as.Date(input$dates[2]), "%Y-%m"), input$months, input$slide, matrix(portfoliodf$bondData[["Weight"]], nrow = 1), simResults$stockOnly, input$stockweight, input$bondweight)
    } else {
      print("No data available for selected ISINs in date range. Bond simulation skipped")
    }
  })
  
  library(moments)
  
  maketable <- eventReactive(simResults$withBonds,{
    Scaled_return = simResults$withBonds[,input$months+1]/simResults$withBonds[,1]*100-100
    VaR_q = quantile(Scaled_return, probs = c(input$var/100))*input$notional/100
    kurt = kurtosis(Scaled_return)
    skew = skewness(Scaled_return)
    result <- data.frame(
                  Statistic = c('Value-at-Risk','Skew','Kurtosis'),
                  Value = c(-VaR_q,skew,kurt)
              )
  })
  
  # This is a helper function found on SO, seeing if it helps with generating deletion buttons for stocks in portfolio
  shinyInput <- function(FUN, len, id, ...) {
    # inputs <- character(1)
    inputs <- as.character(FUN(paste0(id, len), ...))
    inputs
  }
  
  selectedData <- eventReactive(stockPortfolio$tickers, {
    
    selectedTickers <- c()
    selectedWeights <- c()
      
    for (s in stockPortfolio$tickers) {
      selectedTickers <- c(selectedTickers,s[1])
    }
    
    for (s in stockPortfolio$weights) {
      selectedWeights <- c(selectedWeights,s[1])
    }
    
    # print(selectedTickers)
    # print(selectedWeights)
    
    # tickers = unlist(strsplit(input$tickers,","))
    # print(stockPortfolio$weights)
    f = cbind(selectedTickers,selectedWeights,shinyInput(actionButton, length(selectedWeights), 'button_', label = "Delete", onclick = 'Shiny.onInputChange(\"delete_button\",  this.id)' ))
    
    f <- data.frame(
      Tickers = selectedTickers,
      Weights = selectedWeights,
      Actions = shinyInput(actionButton, length(selectedWeights), 'button_', label = "Delete", onclick = 'Shiny.onInputChange(\"select_button\",  this.id)' ),
      stringsAsFactors = FALSE
    )
    
    f <- datatable(f)

  })
  
  
  observeEvent(input$stockDel_button, {
    selectedRow <- as.numeric(strsplit(input$stockDel_button, "_")[[1]][2])
    portfoliodf$stockData <- portfoliodf$stockData[-c(selectedRow),]
  })
  
  observeEvent(input$bondDel_button, {
    selectedRow <- as.numeric(strsplit(input$bondDel_button, "_")[[1]][2])
    portfoliodf$bondData <- portfoliodf$bondData[-c(selectedRow),]
  })

  # This function outputs the text display of current stock file
  output$stockFilePath <- renderText({
    if (is.integer(input$file)) {
      cat("Select stock file")
    } else {
      parseFilePaths(volumes,input$stockFile)[["datapath"]]
    }
  })
  
  output$tickerSearchRes <- renderUI({stockSearchButtons()})
  output$isinSearchRes <- renderUI({bondSearchButtons()})
  output$pricepath <- renderPlot({pricePaths()})
  output$tuottojakauma1 <- renderPlot({return_histogram()})
  output$tuottojakauma2 <- renderPlot({marketcap_histogram()})
  output$filetable <- renderDataTable({selectedData()},server = FALSE, escape = FALSE, selection = 'none')
  output$stockTable <- DT::renderDataTable(
    portfoliodf$stockData, server = FALSE, escape = FALSE, selection = 'none'
  )
  output$bondTable <- DT::renderDataTable(
    portfoliodf$bondData, server = FALSE, escape = FALSE, selection = 'none'
  )
  output$dist_stats <- renderTable({maketable()})
}

shinyApp(ui,server)

