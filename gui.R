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
      
      shinyFilesButton("stockFile","File select", "Select your stock data file", multiple=FALSE, viewtype="detail"),
      
      textOutput("stockFilePath"),
      
      
      searchInput(inputId="tickerSearch",
                  label="Search stock data by ticker",
                  placeholder = "",
                  btnSearch = icon("search"), 
                  btnReset = icon("remove"),
                  width = "100%"),
      #verbatimTextOutput("tickerSearchRes"),
      uiOutput("tickerSearchRes"),
      
      textInput(inputId = "tickers",
                label = "Give tickers:",
                value = "NOKIA.HE"),
      
      textInput(inputId = "isins",
                label = "Give ISINs",
                value = "XS1333685409"),
      
      
      fluidRow(
        
        column(width=6,numericInput(inputId = "stockweight",
                              label = "Stock allocation (%):",
                              value = 100,
                              min = 0,
                              max = 100)),
        
        column(width=6,numericInput(inputId = "bondweight",
                              label = "Bond allocation (%):",
                              value = 0,
                              min = 0,
                              max = 100))

      ),
    
      
      numericInput(inputId = "months",
                   label = "Number of months to simulate:",
                   value = 20,
                   min = 1,
                   max = 60),
      
      
      dateRangeInput(inputId = "dates",
                     "Choose the start and end date",
                     start = "2016-07-01",
                     end = "2018-08-01",),
      
      sliderInput("slide","Number of simulations",
                  min=0,max=1000,value=500),
      
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
    y_scale = c(min(simResults$stockOnly[,input$months])*1000000,max(simResults$stockOnly[,input$months])*1000000)
    # Axis labels:
    y_lab = "Market Cap"
    x_lab = "Months from beginning"
    
    # Set title and subtitle:
    title_ = "Price path simulation"
    sub_ = paste(unlist(strsplit(input$tickers,",")), collapse = ", ")
    
    # Merge title and subtitle:
    full_title = paste(title_, "\n",sub_)
    
    # First path plot:
    plot(T, simResults$stockOnly[1,]*1000000, main = full_title, ylim = y_scale, type = "l", xlab = x_lab, ylab = y_lab)
    
    # Plot rest with lines:
    for (i in seq(1,input$slide,1)) {
      lines(T, simResults$stockOnly[i,]*1000000)
    }
    
    # Find mean path:
    means = colMeans(simResults$stockOnly)
    # Plot mean path:
    lines(T,means*1000000, col = "#FF0000")
  })
  
  return_histogram <- eventReactive(simResults$stockOnly,{
    histData <- simResults$stockOnly
    Scaled_return = histData[,input$months+1]/histData[,1]*100-100
    hist(Scaled_return, main = title_ret, sub = subtitle, xlab = "Cumulative return (%)", xlim = c(-100,200), breaks = 15)
  })

  marketcap_histogram <- eventReactive(simResults$stockOnly, {
    histData <- simResults$stockOnly
    # Normalized values at end of period:
    Scaled = histData[,input$months+1]
    Cap = Scaled * 1000000
    hist(Cap, main = title_cap, sub = subtitle, xlab = "Market value (Eur)", xlim = c(0,3*Value), breaks = 15)
  })
  
  observeEvent(input$button, {
    simResults$stockOnly <- Simulate_Stocks(unlist(strsplit(input$tickers,",")), format(as.Date(input$dates[1]), "%Y-%m"), format(as.Date(input$dates[2]), "%Y-%m"), input$months, input$slide, matrix(rep(1,length(unlist(strsplit(input$tickers,",")))), nrow = 1), 1000000, input$stockweight, input$bondweight)
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

