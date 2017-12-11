#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Stock Technical Analysis"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      selectInput("Stock"," Stock to be analysed", 
                  choices = c("AAPL","VLO","INTC","WMT","LYB","BA","CSCO","MSFT","AMAT","VMW","F")),
      selectInput("Indicator"," Please select the type of Indicator", choices = c("Leading","Lagging")),
      
     conditionalPanel(condition = "input.Indicator =='Lagging'", 
                       selectInput("LaggingLeading","Please select the Lagging Indicator",
                      choices = c("Simple Moving Average","Exponential Moving Average","Bollinger Bands"))),
      
      conditionalPanel(condition = "input.Indicator =='Leading'", 
                       selectInput("LaggingLeading","Please select the Leading Indicator",
                                   choices = c("ADX","CCI","MACD"))),
      
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("TechnicalAnalysisChart")
    )
  )
))
