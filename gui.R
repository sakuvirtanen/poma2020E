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
            #verbatimTextOutput("tickerSearchRes"),
            
            column(12,uiOutput("tickerSearchRes")),
            
            column(12,textInput(inputId = "tickers",
                      label = "Give tickers:",
                      value = "NOKIA.HE")),
            
            column(12,textInput(inputId = "isins",
                      label = "Give ISINs",
                      value = "XS1333685409")),
            
            
            column(12,actionButton("pf_button",label="Add to portfolio"))
            
          )
        )
      ),
      
      
      fluidRow(
        
        column(12,h2("Asset Allocation"),style="background-color:#DDDDDD;",
          fluidRow(
          
            column(width=4,numericInput(inputId = "stockweight",
                                  label = "Stocks (%):",
                                  value = 100,
                                  min = 0,
                                  max = 100)),
            
            column(width=4,numericInput(inputId = "bondweight",label = "Corporate bonds (%):",
                                  value = 0,min = 0,max = 100)),
            
            column(width=4,numericInput(inputId = "govbondweight",label = "Government bonds (%):",
                                        value = 0,min = 0,max = 100))
          )
        ),
        column(12,textOutput("weight_check"))
        

      ),
      
      fluidRow(
        column(12,h2("Simulation specs"),style="background-color:#DDDDDD;",
                      
          fluidRow(
            
            column(6,numericInput(inputId = "months",
                   label = "Number of months to simulate:",
                   value = 10,
                   min = 1,
                   max = 60)),
      
            column(6,numericInput(inputId = "notional",
                   label = "Notional portfolio value at beginning:",
                   value = 1000000,
                   min = 1,
                   max = 2000000)),
      
      
            column(12,dateRangeInput(inputId = "dates",
                     "Choose the start and end date",
                     start = "2018-10-01",
                     end = "2019-10-01",)),
      
            column(12,sliderInput("slide","Number of simulations",
                  min=0,max=1000,value=20)),
          )
        )
      ), 
      
      numericInput(inputId = "var",
                   label = "Value at risk:",
                   value = 0.05,
                   min = 0,
                   max = 1),
      
      actionButton("button",label="Run simulation"),
      actionButton("button",label="Download simulated data")
      
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Assets",dataTableOutput("filetable"),actionButton("deleteLastStock","Delete last")),
        # tabPanel("Assets",uiOutput("portfolioStocks")),
        tabPanel("Price path",plotOutput("pricepath"),icon = icon("chart-line")),
        tabPanel("Return distribution",plotOutput("tuottojakauma1"),icon = icon("bar-chart-o")),
        tabPanel("Market cap distribution",plotOutput("tuottojakauma2"),icon = icon("bar-chart-o")),
        tabPanel("Key information",tableOutput("stats"),icon = icon("info")),
        tabPanel("Efficient Frontier",plotOutput("efficientfrontier"))
      )
    ),
    
  )
  
)

server <- function(input,output,session) {
  
  # Find out some volumes on the local filesystem
  volumes <- c(wd = ".")
  # Server side function for rendering the file chooser. Note that this takes the additional session variable given
  # to the server function
  shinyFileChoose(input, "stockFile", roots = volumes, filetypes=c("", "xlsx", "xls"), session = session)
  
  simResults <- reactiveValues()
  stockPortfolio <- reactiveValues()
  
  #  Create a list for storing the event handlers for individual search result buttons
  obsList <- list()
  
  # Event handler for ticker search events
  searchResButtons <- eventReactive(input$tickerSearch_search,
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
      if (is.null(obsList[[btName]])) {
        obsList[[btName]] <<- observeEvent(input[[btName]], {
          stockPortfolio$tickers[btName] <- btName
          stockPortfolio$weights[btName] <- 1
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
  
  # selectedPortfolioConstructor <- eventReactive(stockPortfolio$weights, {
  #   selectedTickers <- c()
  #   selectedWeights <- c()
  #   
  #   for (s in stockPortfolio$tickers) {
  #     selectedTickers <- c(selectedTickers,s[1])
  #   }
  #   
  #   for (s in stockPortfolio$weights) {
  #     selectedWeights <- c(selectedWeights,s[1])
  #   }
  #   
  #   print("Weights changed")
  #   print("Current tickers:")
  #   print(selectedTickers)
  #   
  #   rows <- selectedTickers
  #   
  #   for (i in 1:length(selectedTickers)) {
  #     rows[i] <- fluidRow(
  #       p(selectedTickers[i]),
  #       p(selectedWeights[i])
  #     )
  #   }
  #   
  # })
  
  
  
  observeEvent(stockPortfolio, {
    print("stockPortfolio changed")
  })
  
  tickerList <- eventReactive(input$button,{
    input$tickers
  })
  
  observeEvent(input$deleteLastStock, {
    selectedTickers <- c()
    selectedWeights <- c()
    
    for (s in stockPortfolio$tickers) {
      selectedTickers <- c(selectedTickers,s[1])
    }
    
    for (s in stockPortfolio$weights) {
      selectedWeights <- c(selectedWeights,s[1])
    }
    
    delName = selectedTickers[length(selectedTickers)]
    
    tickList <- stockPortfolio$tickers
    tickList <- tickList[-length(tickList)]
    
    weightList <- stockPortfolio$weights
    weightList <- weightList[-length(weightList)]
    
    stockPortfolio$tickers <- tickList
    stockPortfolio$weights <- weightList
    # print(stockPortfolio$tickers[delName])
  })
  
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
    
    stock_df$Month = seq(1,input$months+1)
    # print(stock_df)
    
    d = stock_df
    
    d <- melt(d, id.vars="Month")
    #print(d)
    # Everything on the same plot
    ggplot(d, aes(Month,value, col=variable)) + 
      geom_line() + 
      ggtitle("Simulated Price Paths") +
      theme_light() +
      theme(legend.title = element_blank()) +
      theme(legend.position = "none")
      #geom_line(aes(x=Month,y=colMeans(stock_df)*input$notional))
    
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
  
  return_histogram <- eventReactive(input$button,{
    histData <- simResults$withBonds
    Scaled_return = histData[,input$months+1]/histData[,1]*100-100
    VaR_q = quantile(Scaled_return, probs = c(input$var))*input$notional/100
    subtitle = paste(input$slide, " simulations, ", input$months, " steps" , ", VaR ", input$var,"%:" , signif(VaR_q, digits = 3))
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
    VaR_q = quantile(Scaled_return, probs = c(input$var))*input$notional/100
    subtitle = paste(input$slide, " simulations, ", input$months, " steps" , ", VaR ", input$var,"%:" , signif(VaR_q, digits = 3))
    f = data.frame(Cap)
    ggplot(f,aes(x=Scaled_return)) + geom_histogram()
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
    simResults$stockOnly <- Simulate_Stocks(selectedTickers, format(as.Date(input$dates[1]), "%Y-%m"), format(as.Date(input$dates[2]), "%Y-%m"), input$months, input$slide, matrix(selectedWeights, nrow = 1),input$notional,input$stockweight,input$bondweight)
  })
  
  observeEvent(simResults$stockOnly, {
    # print("A stock simulation just got created! Now invoking bond simulations")
    simResults$withBonds <- Simulate_Bonds(unlist(strsplit(input$isins,",")),format(as.Date(input$dates[1]), "%Y-%m"), format(as.Date(input$dates[2]), "%Y-%m"), input$months, input$slide, matrix(rep(1,length(unlist(strsplit(input$isins,",")))), nrow = 1), simResults$stockOnly, input$stockweight, input$bondweight)
  })
  
  
  maketable <- eventReactive(input$button,{
    result <- table(c("Value-at-Risk","b",),c(1,2))
  })
  
  # This is a helper function found on SO, seeing if it helps with generating deletion buttons for stocks in portfolio
  shinyInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
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
    # print(f)
    # print(length(f))
    f <- datatable(f)
    
    
    
    # Create the table (using table from htmlTables doc as example)
    #HTML(
    #  htmlTable(matrix(paste("Content", LETTERS[1:16]), 
    #                   ncol=4, byrow = TRUE),
    #            header =  paste(c("1st", "2nd",
    #                              "3rd", "4th"), "header"),
                #rnames = paste(c("1st", "2nd",
                #                 "3rd", "4th"), "row"),
    #            rgroup = c("Stocks",
    #                       "Corporate Bonds"),
     #           n.rgroup = c(2,2),
      #          cgroup = c("Cgroup 1", "Cgroup 2&dagger;"),
       #         n.cgroup = c(2,2), 
      #          caption="Portfolio Assets",
      #          tfoot="&dagger; A table footer commment") 
    #)
    
  })
  
  
  frontier_react <- eventReactive(input$button,{
    plot(c(1,2,3),c(3,5,8),main="Efficient Frontier",
         xlab="Standard deviation",ylab="Excess returns")
  })
  
  # This function outputs the text display of current stock file
  output$stockFilePath <- renderText({
    if (is.integer(input$file)) {
      cat("Select stock file")
    } else {
      parseFilePaths(volumes,input$stockFile)[["datapath"]]
    }
  })
  
  output$tickerSearchRes <- renderUI({searchResButtons()})
  output$portfolioStocks <- renderUI({selectedPortfolioConstructor()})
  output$text <- renderText({tickerList()})
  output$pricepath <- renderPlot({pricePaths()})
  output$tuottojakauma1 <- renderPlot({return_histogram()})
  output$tuottojakauma2 <- renderPlot({marketcap_histogram()})
  output$filetable <- renderDataTable({selectedData()},server = FALSE, escape = FALSE, selection = 'none')
  output$stats <- renderTable({maketable()})
  
  output$efficientfrontier <- renderPlot({frontier_react()})
  

}

shinyApp(ui,server)

