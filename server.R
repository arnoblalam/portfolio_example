#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(quantmod)
library(dplyr)

# Symbols for the sector indices.  We use ETF's because the sector data is not
# available from Yahoo Finance.

tickers <- c("XLY", # Consumer Discretionary 
             "XLP", # Consumer Staples
             "XLE", # Energy
             "XLF", # Financials
             "XLV", # Health Care
             "XLI", # Industrials
             "XLB", # Materials
             "XLK", # Information Technology
             "XLU", # Utilities
             "SPY") # Index  

# Corresponding sector names
sectors <- c("Consumer Discretionary", "Consumer Staples",
             "Energy", "Financials", "Health Care", "Industrials",
             "Materials", "Information Technology", "Utilities", "Index")

etf_tickers_sectors <- data_frame(ticker = tickers, 
                                  sector = sectors)

etf_weekly_returns <- function(ticker) {
symbols <- getSymbols(tickers, src = "google")

etf_prices <- do.call(merge, lapply(symbols, function(x) Cl(get(x))))

etf_returns <- do.call(merge, lapply(etf_prices, 
                                     function(x) periodReturn(x, 
                                                              period = 'weekly', 
                                                              type = 'log')))

#Change the column names to the sector names from our dataframe above.

colnames(etf_returns) <- etf_tickers_sectors$sector

etf_returns

}

etf_returns <- etf_weekly_returns(etf_tickerS_sectorS$ticker)

sector_index_correlation <- function(x, window) {
    
    merged_xts <- merge(x, etf_returns$'Index')
    
    merged_xts$rolling_test <- rollapply(merged_xts, window, 
                                         function(x) cor(x[,1], x[,2], 
                                                         use = "pairwise.complete.obs"), 
                                         by.column = FALSE)
    
    names(merged_xts) <- c("Sector Returns", "SPY Returns", "Sector/SPY Correlation")
    
    merged_xts
}


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  
})

etf_returns <- etf_weekly_returns(etf_tickerS_sectorS$ticker)

