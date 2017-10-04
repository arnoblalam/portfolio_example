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
library(dygraphs)
library(Rsolnp)
library(lubridate)
library(highcharter)
library(foreach)

# Define server logic required to draw a histogram

shinyServer(function(input, output) {
    
    etf_returns <- eventReactive(input$go, {
        tickers <- unlist(strsplit(input$tickers,"\n"))
        sectors <- unlist(strsplit(input$names, "\n"))
        
        etf_yearly_returns <- function(ticker) {
            symbols <- getSymbols(ticker, src = "google", from =  floor_date(Sys.Date(), "years") - years(input$years))
            etf_prices <- do.call(merge, lapply(symbols, function(x) Cl(get(x))))
            
            etf_returns <- do.call(merge, lapply(etf_prices, 
                                                 function(x) periodReturn(x, 
                                                                          period = 'yearly', 
                                                                          type = 'log',
                                                                          subset = '1990::')))
            
            # Change the column names to the sector names from our dataframe above.
            
            colnames(etf_returns) <- sectors
            
            etf_returns*100
            
        }
        etf_yearly_returns(tickers)
    })
    
    solution <- eventReactive(input$go, {
        
        # Pick a desired retrun mu0 and a risk sigma
        mu0 <- as.numeric(input$mu0)
        sigma <- as.numeric(input$sigma)
        
        # Average returns for eachs sector
        ret <- apply(etf_returns(), 2, mean)
        cov_ <- cor(etf_returns())
        init_values <- rep(1/length(tickers()), length(tickers()))
        # Objective function to be minimized 
        obj <- function(p) {
            h <- ifelse(p == 0, 0, p*log(p))
            sum(h)
        }
        
        # Equality constraints
        eqfun <- function(p) {
            sum(p)
        }
        
        # Values of the constraints
        eqB <- 1
        
        # Inequality Constraints
        ineqfun <- function(p) {
            z1 <- as.vector(p %*% ret)
            z2 <- as.vector(sqrt(p %*% cov_ %*% p))
            return(c(z1, z2))
        }
        
        ineqLB <- c(mu0, 0)
        ineqUB <- c(Inf, sigma)
        
        # Lower bound on the p (probabilities must be positive)
        LB <- rep(0, length(init_values))
        
        # Solve the problem
        solnp(pars = init_values,
              fun = obj,
              eqfun = eqfun,
              eqB = eqB,
              ineqfun = ineqfun,
              ineqLB = ineqLB,
              ineqUB = ineqUB,
              LB = LB)
        
        
    })
    
    sectors <- eventReactive(input$go, {
        unlist(strsplit(input$names, "\n"))
    })
    
    tickers <- eventReactive(input$go, {
        unlist(strsplit(input$tickers,"\n"))
    })
    
    output$allocations <- renderHighchart({
        highchart() %>%
            hc_title(text="Portfolio Allocation") %>%
            hc_add_series_labels_values(sectors(), 
                                        round(solution()$pars*100, 2), 
                                        type = "pie",
                                        name = "Bar", 
                                        colorByPoint = TRUE,
                                        dataLabels = list(format =  "{point.name} {point.y}%")) %>%
            hc_tooltip(pointFormat = "{point.y}%")
        
    })
    output$stock_returns <- renderDygraph({
        dygraph(cbind("Equally Weighted" = apply(etf_returns(), 1, mean),
                      "Info-Metrics" = apply(etf_returns(), 1, weighted.mean, solution()$pars)))
    })
  
})