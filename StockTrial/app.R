#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)


# Load R packages 
library("TTR") #Technical Trading Rules Package
library("quantmod") # Quantitative Financial Modelling and Trading Framework for R
library("PerformanceAnalytics") #Econometric tools for performance and risk analysis
library("Quandl")

companies = c("Apple,INC"= "AAPL",
              "VALERO ENERGY"="VLO",
              "INTEL CORP"="INTC",
              "WAL-MART STORES INC"="WMT",
              "LYONDELLBASELL INDUSTRIES"="LYB",
              "BOEING CO"="BA",
              "CISCO SYS INC"="CSCO",
              "MICROSOFT CORP"="MSFT",
              "APPLIED MATLS INC"="AMAT",
              "VMWARE INC"="VMW",
              "FORD MTR CO DEL"="F")

# Define UI for application that draws a histogram
ui <- (dashboardPage( 
  dashboardHeader(title ="Trading Analysis"),
  dashboardSidebar(
    sidebarMenu(
    menuItem("Technical Stock Analysis", tabName = "first", icon = icon("dashboard")),
    menuItem("Volatility Analysis", tabName = "second", icon = icon("dashboard"))
  )),
   dashboardBody(
     
     tabItems(
       tabItem( tabName = "first",
                h2("Stock Technicals"),
       
     
     fluidRow(
       box(
         title = "Controls", width = 3,
         selectInput("Stock"," Stock to be analysed", 
                     choices = companies),
         radioButtons("Indicator"," Please select the type of Indicator", choices = c("Leading","Lagging")),
         
         conditionalPanel(condition = "input.Indicator =='Lagging'", 
                          selectInput("LaggingLeading","Please select the Lagging Indicator",
                                      choices = c("Simple Moving Average","Exponential Moving Average","Bollinger Bands"))),
         
         conditionalPanel(condition = "input.Indicator =='Leading'", 
                          selectInput("LaggingLeading","Please select the Leading Indicator",
                                      choices = c("ADX","CCI","MACD")))
         ),
       
       box(width = 9, plotOutput("TechnicalAnalysisChart", height = 350)),
       
       conditionalPanel(condition = "input.Indicator =='Lagging'",
       tabsetPanel(type = "tab", 
                   tabPanel("Simple Moving Average", plotOutput("SMA")),
                   tabPanel("Exponential Moving Average", plotOutput("EMA"))))
     )),
     
     tabItem(tabName = "second", 
             h2("Volatility Analysis of the Stock"),
             
             fluidRow(
               
               
               box(width = 9, plotOutput("Volatility", height = 350)))
             
             ))
   )
  )
)


stockData = getSymbols("AAPL",src="google", auto.assign = FALSE, from = '2012-01-01', to = Sys.Date())

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # generate inputs based on inputs from ui.R
  Ticker = reactive({input$Stock})
  indicatorType = reactive({input$Indicator})
  indicator = reactive({input$LaggingLeading})
  
  #Get data from Google for stock for the last 5 Years
  stockData = reactive({getSymbols(Ticker(),src="google", auto.assign = FALSE, from = '2012-01-01', to = Sys.Date())})
  
  #Output in for Technical Analysis Chart
  output$TechnicalAnalysisChart <- renderPlot({
    
    
    # Technical Analysis Charts
    lineChart(stockData(), name = paste("Line Chart for",Ticker())) # Line Charts
    barChart(stockData(), name = paste("Bar Chart for",Ticker())) # Bar Charts
    #candleChart(stockData(), name = paste("Candle Chart for",Ticker())) # Candle Charts
   
  })
  
  output$SMA <- renderPlot({
    
    # Simple Moving Average
    sma5 <- SMA(Cl(stockData()),n=5)
    sma21 <- SMA(Cl(stockData()),n=21)
    # Technical Analysis Chart
    barChart(stockData())
    addSMA(n=5,col=4)
    addSMA(n=21,col=6)
    # Manual Chart
    plot(Cl(stockData()),main="Simple Moving Averages SMA(5 & 21)")
    lines(sma5,col=4)
    lines(sma21,col=6)
  })
  
  
  output$Volatility = renderPlot({
    # 2. Data Downloading
    htickers <- "CBOE/VIX/4"
    htickers2 <- "^GSPC"
    hdata <- Quandl(htickers,type="xts",start_date="2007-01-01",end_date="2017-01-01")
    GSPC = getSymbols(htickers2,src='yahoo',from="2007-01-01",to="2017-01-01", auto.assign = FALSE)
    hdata <- cbind(GSPC[,1:4],hdata)
    hdata <- hdata[complete.cases(hdata),]
    
    # 3. Historical Volatility Estimation
    hspxohlc <- hdata[,1:4]
    
    # 3.1. Close to Close Estimation
    hvolcc <- volatility(hspxohlc,calc="close",n=21,N=252)
    # 3.1.1. Close to Close Estimation Chart
    plot(hvolcc,main="Close to Close Volatility Estimation")
    #legend("topright",col="black",lty=1,legend="cc")
  })
  
  
  
  }


# Run the application 
shinyApp(ui = ui, server = server)

