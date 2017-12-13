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

#Top Companies based on some of the ETFs in the market
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

# Define UI for application
ui <- (dashboardPage( 
  dashboardHeader(title ="Trading Analysis"),
  dashboardSidebar(
    sidebarMenu(
    menuItem("Technical Stock Analysis", tabName = "first", icon = icon("dashboard")),
    menuItem("Volatility Analysis", tabName = "second", icon = icon("dashboard"))
  )),
  
  
   dashboardBody(
     
#-----------------------Stock Technical Analysis---------------------------     
     
     tabItems(
       tabItem( tabName = "first",
                h2("Stock Technicals"),
       
     
     fluidRow(
       box(
         title = "Controls", width = 3,
         selectInput("Stock"," Stock to be analysed", 
                     choices = companies),
         radioButtons("Indicator"," Please select the type of Indicator", choices = c("Leading","Lagging"))
         
         # conditionalPanel(condition = "input.Indicator =='Lagging'", 
         #                  selectInput("LaggingLeading","Please select the Lagging Indicator",
         #                              choices = c("Simple Moving Average","Exponential Moving Average","Bollinger Bands"))),
         # 
         # conditionalPanel(condition = "input.Indicator =='Leading'", 
         #                  selectInput("LaggingLeading","Please select the Leading Indicator",
         #                              choices = c("ADX","CCI","MACD")))
         ),
       
       box(width = 9, plotOutput("TechnicalAnalysisChart", height = 350)),
       
       
       #Lagging Indicators Charts
       conditionalPanel(condition = "input.Indicator =='Lagging'",
       tabsetPanel(type = "tab", 
                   tabPanel("Simple Moving Average", plotOutput("SMA")),
                   tabPanel("Exponential Moving Average", plotOutput("EMA")),
                   tabPanel("Bollinger Bands BB", plotOutput("Bollinger")),
                   tabPanel("Parabolic Stop and Reverse SAR", plotOutput("Parabolic")))),
       
       #Leading Indicators Charts
       conditionalPanel(condition = "input.Indicator =='Leading'",
                        tabsetPanel(type = "tab", 
                                    tabPanel("Average Directional Movement Index ADX", plotOutput("ADX")),
                                    tabPanel("Commodity Channel Index CCI", plotOutput("CCI")),
                                    tabPanel("Moving Averages Covergence/Divergence MACD", plotOutput("MACD")),
                                    tabPanel("Rate Of Change ROC", plotOutput("ROC")),
                                    tabPanel("Relative Strength Index RSI", plotOutput("RSI")),
                                    tabPanel("Stochastic Momentum Index SMI", plotOutput("SMI")),
                                    tabPanel("Williams %R", plotOutput("WilliamsR")))
                                    )
                   
                   
                   
     )),
     
#-------------------Volatility Analysis UI (Trial)-----------------------------------------------------     
     
     
     tabItem(tabName = "second", 
             h2("Volatility Analysis of the Stock"),
             
             fluidRow(
               box(
                 title = "Stock Select", width = 3,
                 selectInput("StockVolt"," Stock to be analysed", 
                             choices = companies)),
               
               box(width = 9, plotOutput("Volatility", height = 350)))
             
             ))
   )
  )
)


stockData = getSymbols("AAPL",src="google", auto.assign = FALSE, from = '2012-01-01', to = Sys.Date())


##### SERVER CODE ########## SERVER CODE ########## SERVER CODE ########## SERVER CODE ########## SERVER CODE ########## SERVER CODE #####
##### SERVER CODE ########## SERVER CODE ########## SERVER CODE ########## SERVER CODE ########## SERVER CODE ########## SERVER CODE #####
##### SERVER CODE ########## SERVER CODE ########## SERVER CODE ########## SERVER CODE ########## SERVER CODE ########## SERVER CODE #####


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  
#-----------------Stock Technical Analysis------------------------------------------------------
  
  
  # generate inputs based on inputs from ui.R
  Ticker = reactive({input$Stock})
  TickerVolt = reactive({input$StockVolt})
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

  
  #--------------- Lagging indicators ----------------------
  
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
  
  
  output$EMA = renderPlot({
    # Exponential Moving Average
    ema5 <- EMA(Cl(stockData()),n=5)
    ema21 <- EMA(Cl(stockData()),n=21)
    # Technical Analysis Chart
    barChart(stockData())
    addEMA(n=5,col=4)
    addEMA(n=21,col=6)
    # Manual Chart
    plot(Cl(stockData()),main="Exponential Moving Averages EMA(5 & 21)")
    lines(ema5,col=4)
    lines(ema21,col=6)
  })
  
  
  output$Bollinger = renderPlot({
    # Bollinger Bands BB(20,2)
    bb <- BBands(cbind(Hi(stockData()),Lo(stockData()),Cl(stockData())),n=20,sd=2)
    # Technical Analysis Chart
    barChart(stockData())
    addBBands(n=20,sd=2)
    # Manual Chart
    plot(Cl(stockData()),main="Bollinger Bands BB(20,2)")
    # Lower and Upper Bands
    lines(bb[,1],col=4)
    lines(bb[,3],col=4)
    # Middle Band
    lines(bb[,2],col=5)
  })
  
  output$Parabolic = renderPlot({
    # Parabolic Stop and Reverse SAR(0.02,0.2)
    sar <- SAR(cbind(Hi(stockData()),Lo(stockData())),accel=c(0.02, 0.2))
    # Technical Analysis Chart
    barChart(stockData())
    addSAR(accel=c(0.02, 0.2))
    # Manual Chart
    plot(Cl(stockData()),main="Parabolic Stop and Reverse SAR(0.02,0.2)")
    points(sar,col=4)
  })
  
  
  #------------------------ Leading Indicators ------------
  
  output$ADX = renderPlot({
    
    # Average Directional Movement Index ADX(14)
    adx <- ADX(cbind(Hi(stockData()),Lo(stockData()),Cl(stockData())),n=14)
    # Technical Analysis Chart
    barChart(stockData())
    addADX(n=14)
  })
  
  output$CCI = renderPlot({
    # Commodity Channel Index CCI(20,0.015)
    cci <- CCI(cbind(Hi(stockData()),Lo(stockData()),Cl(stockData())),n=20,c=0.015)
    # Technical Analysis Chart
    barChart(stockData())
    addCCI(n=20,c=0.015)
  })
  
  output$MACD = renderPlot({
    # Moving Averages Covergence/Divergence MACD(12,26,9)
    macd <- MACD(Cl(stockData()),nFast=12,nSlow=26,nSig=9)
    # Technical Analysis Chart
    barChart(stockData())
    addMACD()
  })
  
  output$ROC = renderPlot({
    # Rate Of Change ROC(21)
    roc <- ROC(stockData(),n=21)
    # Technical Analysis Chart
    barChart(stockData())
    addROC(n=21)
  })
  
  output$RSI = renderPlot({
    # Relative Strength Index RSI(14)
    rsi <- RSI(Cl(stockData()),n=14)
    # Technical Analysis Chart
    barChart(stockData())
    addRSI(n=14)
    
  })
  
  output$SMI = renderPlot({
    # Stochastic Momentum Index SMI(13,2,25,9)
    smi <- SMI(cbind(Hi(stockData()),Lo(stockData()),Cl(stockData())),n=13,nFast=2,nSlow=25,nSig=9)
    # Technical Analysis Chart
    barChart(stockData())
    addSMI(n=13)
  })
  
  output$WilliamsR = renderPlot({
    # Williams %R(14)
    wpr <- WPR(cbind(Hi(stockData()),Lo(stockData()),Cl(stockData())),n=14)
    # Technical Analysis Chart
    barChart(stockData())
    addWPR(n=14)
  })
  
  
  
  
#-------------------------Volatility Analysis-------------------------------------------------------  
  
  output$Volatility = renderPlot({
    # Data Acquisition from QUandl
    htickers <- "CBOE/VIX/4"
    htickers2 <- "^GSPC"
    hdata <- Quandl(htickers,type="xts",start_date="2007-01-01",end_date="2017-01-01")
    tickerData = reactive({getSymbols(TickerVolt(),src='yahoo',from="2007-01-01",to="2017-01-01", auto.assign = FALSE)})
    voltData = tickerData()
    hdata <- cbind(voltData[,1:4],hdata)
    hdata <- hdata[complete.cases(hdata),]
    
    #Historical Volatility Estimation
    hspxohlc <- hdata[,1:4]
    
    #Close to Close Estimation
    hvolcc <- volatility(hspxohlc,calc="close",n=21,N=252)
    #Close to Close Estimation Chart
    plot(hvolcc,main=paste("Close to Close Volatility Estimation for ",TickerVolt()))
    #legend("topright",col="black",lty=1,legend="cc")
  })
  
  
  
  }


# Run the application 
shinyApp(ui = ui, server = server)

