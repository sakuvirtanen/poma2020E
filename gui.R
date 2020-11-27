library(shiny)
library(shinyWidgets)
library(shinyFiles)
library(fs)
library(readxl)
# source("osakesimulaatio_funktiomuodossa.R")
# source("hintapolkufunktio.R")
source("scripts.R")

ui <- fluidPage(
  setBackgroundImage(
    src = ""
  ),
  includeCSS("styles.css"),
  
  titlePanel(
    h1("Portfolio Management Tool")
  ),
  
  
  sidebarLayout(
    sidebarPanel(
      
      fluidRow(
        
        column(12,h2("Select Tickers"),style="background-color:#ffff99;",
          
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
          )
        )
      ),
      
      
      fluidRow(
        
        column(12,h2("Asset Allocation"),style="background-color:#ccff99;",
          fluidRow(
          
            column(width=4,numericInput(inputId = "stockweight",
                                  label = "Stocks (%):",
                                  value = 100,
                                  min = 0,
                                  max = 100)),
            
            column(width=4,numericInput(inputId = "bondweight",
                                  label = "Corporate bonds (%):",
                                  value = 0,
                                  min = 0,
                                  max = 100)),
            
            column(width=4,numericInput(inputId = "govbondweight",
                                        label = "Government bonds (%):",
                                        value = 0,
                                        min = 0,
                                        max = 100))
          )
        ),
        column(12,textOutput("weight_check"))
        

      ),
      
      fluidRow(
        column(12,h2("Simulation specs"),style="background-color:#ffff99;",
                      
          fluidRow(
            
            column(6,numericInput(inputId = "months",
                   label = "Number of months to simulate:",
                   value = 5,
                   min = 1,
                   max = 60)),
      
            column(6,numericInput(inputId = "notional",
                   label = "Notional portfolio value at beginning:",
                   value = 1000000,
                   min = 1,
                   max = 2000000)),
      
      
            column(12,dateRangeInput(inputId = "dates",
                     "Choose the start and end date",
                     start = "2016-07-01",
                     end = "2018-08-01",)),
      
            column(12,sliderInput("slide","Number of simulations",
                  min=0,max=1000,value=10)),
          )
        )
      ), 
      
      numericInput(inputId = "var",
                   label = "Value at risk:",
                   value = 0.05,
                   min = 0,
                   max = 1),
      
      actionButton("button",label="Run simulation"),
      
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Price path",plotOutput("pricepath")),
        tabPanel("Return distribution",plotOutput("tuottojakauma1")),
        tabPanel("Market cap distribution",plotOutput("tuottojakauma2")),
        tabPanel("Key information",tableOutput("stats")),
        tabPanel("Efficient Frontier",plotOutput("efficientfrontier"))
      )
    )
    
  )
  
)

server <- function(input,output,session) {
  
  # Find out some volumes on the local filesystem
  volumes <- c(wd = ".")
  # Server side function for rendering the file chooser. Note that this takes the additional session variable given
  # to the server function
  shinyFileChoose(input, "stockFile", roots = volumes, filetypes=c("", "xlsx", "xls"), session = session)
  
  simResults <- reactiveValues()
  
  
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
          updateTextInput(session, "tickers", value=paste0(input$tickers,",",btName))
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
  
  text_reactive <- eventReactive(input$button,{
    input$tickers
  })
  
  # path_reactive <- eventReactive(input$button,{
  #   tickerlist <- unlist(strsplit(input$tickers,","))
  #   pricePathFunction(tickerlist,
  #                     c("2018-10-01","2020-10-01"),
  #                     input$slide,
  #                     1000000,
  #                     input$months,parseFilePaths(volumes,input$stockFile)[["datapath"]])
  # })
  # 
  
  pricePaths <- eventReactive(simResults$stockOnly, {
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
    sub_ = paste(unlist(strsplit(input$tickers,",")), collapse = ", ")
    
    # Merge title and subtitle:
    full_title = paste(title_, "\n",sub_)
    
    # First path plot:
    plot(T, simResults$stockOnly[1,]*input$notional, main = full_title, ylim = y_scale, type = "l", xlab = x_lab, ylab = y_lab)
    
    # Plot rest with lines:
    for (i in seq(1,input$slide,1)) {
      lines(T, simResults$stockOnly[i,]*input$notional)
    }
    
    # Find mean path:
    means = colMeans(simResults$stockOnly)
    # Plot mean path:
    lines(T,means*input$notional, col = "#FF0000")
  })
  
  return_histogram <- eventReactive(input$button,{
    histData <- simResults$stockOnly
    #print(histData)
    Scaled_return = histData[,input$months+1]/histData[,1]*100-100
    #print(Scaled_return)
    VaR_q = quantile(Scaled_return, probs = c(input$var))*input$notional/100
    subtitle = paste(input$slide, " simulations, ", input$months, " steps" , ", VaR ", input$var,"%:" , signif(VaR_q, digits = 3))
    hist(Scaled_return, main = input$tickers, sub = subtitle, xlab = "Cumulative return (%)", xlim = c(-100,200), breaks = 15)
  })

  marketcap_histogram <- eventReactive(input$button, {
    histData <- simResults$stockOnly
    #Scaled_return = histData[,input$months+1]/histData[,1]*100-100
    # Normalized values at end of period:
    Scaled = histData[,input$months+1]
    Cap = Scaled * input$notional
    #print(Cap)
    #VaR_q = quantile(Scaled_return, probs = c(input$var))*input$notional/100
    #subtitle = paste(input$slide, " simulations, ", input$months, " steps" , ", VaR ", input$var,"%:" , signif(VaR_q, digits = 3))
    hist(Cap, main = input$tickers, xlab = "Market value (Eur)", xlim = c(0,3*input$notional), breaks = 15)
  })
  
  
  observeEvent(input$button, {
    simResults$stockOnly <- Simulate_Stocks(unlist(strsplit(input$tickers,",")), format(as.Date(input$dates[1]), "%Y-%m"), format(as.Date(input$dates[2]), "%Y-%m"), input$months, input$slide, matrix(rep(1,length(unlist(strsplit(input$tickers,",")))), nrow = 1),input$notional,input$stockweight,input$bondweight)
  })
  
  # observeEvent(simResults$stockOnly, {
  #   print("A stock simulation just got created! Now invoking bond simulations")
  #   simResults$withBonds <- Simulate_Bonds(unlist(strsplit(input$isins,",")),format(as.Date(input$dates[1]), "%Y-%m"), format(as.Date(input$dates[2]), "%Y-%m"), input$months, input$slide, matrix(rep(1,length(unlist(strsplit(input$isins,",")))), nrow = 1), simResults$stockOnly, input$stockweight, input$bondweight)
  # })
  
  maketable <- eventReactive(input$button,{
    result <- table(c("Value-at-Risk","b"),c(1,2))
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
  output$text <- renderText({text_reactive()})
  output$pricepath <- renderPlot({pricePaths()})
  output$tuottojakauma1 <- renderPlot({return_histogram()})
  output$tuottojakauma2 <- renderPlot({marketcap_histogram()})
  
  output$stats <- renderTable({maketable()})
  
  output$efficientfrontier <- renderPlot({frontier_react()})
  

}

shinyApp(ui,server)

