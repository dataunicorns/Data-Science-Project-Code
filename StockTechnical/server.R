#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
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



# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
   
  output$TechnicalAnalysisChart <- renderPlot({
    
    # generate inputs based on inputs from ui.R
    Ticker = input$Stock
    indicatorType = input$Indicator
    indicator = input$LaggingLeading
    
    
    #Get data from Google for stock for the last 5 Years
    stockData = getSymbols(Ticker,src="google", auto.assign = FALSE, from = '2012-01-01', to = Sys.Date())
    
    
    # Technical Analysis Charts
    lineChart(stockData) # Line Charts
    barChart(stockData) # Bar Charts
    candleChart(stockData, name = paste("Candle Chart for",Ticker)) # Candle Charts
    
    
  })
  
})
