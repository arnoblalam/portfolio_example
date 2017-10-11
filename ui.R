#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(dygraphs)
library(highcharter)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel(HTML("<h1 style='font-size: 2rem'>Optimal Portfolio Allocation (Diversification)
Using Info-Metrics (Chapter 5, Model 5.4)</h1>")),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       textAreaInput("tickers",
                   "Enter ticker symbols(one per line):",
                   rows = 10,
                   value = 
"XLY
XLP
XLE
XLF
XLV
XLI
XLB
XLK
XLU"),
       textAreaInput("names",
                 "Enter the names of the stocks (one per line)",
                 rows = 10,
                 value = 
"Consumer Discretionary
Consumer Staples
Energy
Financials
Health Care
Industrials
Materials
Information Technology
Utilities"),
        sliderInput("years", "How many years of data to estimate returns and correlation?",
                    min = 2, max = 10, value=5, step = 1, round = TRUE),
        textInput("mu0", "Minimum Return (Mean)", value = 5),
        textInput("sigma", "Maximum Risk (Standard Deviation)", value = 10),
        actionButton("go", "Update")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
        highchartOutput("allocations"),
       dygraphOutput("stock_returns")
    )
  )
))
