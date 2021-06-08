library(tidyquant)
library(tidyverse)
library(openxlsx)
library(timetk)
library(broom)
library(highcharter)
library(mvtnorm)

set.seed(1456)

# Read in current portfolio tickers & weights
weights_file <- read.xlsx("weights.xlsx", sheet = "Weights")
t <- weights_file$Tickers
w <- weights_file$Weights 

##### MONTE CARLO SIMULATION FUNCTION #####
mc.simulate <- function(symbols, weights, from, to, return_period = "daily", duration_pred = 3*252, init_invest = 1, nsim = 101){
  # Pulling prices from yahoo & reformatting
  prices <- 
    tq_get(symbols, get= "stock.prices", 
           from = from,
           to = to,
           complete_cases = TRUE) 
  
  prices_mod <- data.frame(matrix(nrow = NROW(prices)/length(symbols), 
                                  ncol = length(symbols)))
  for (i in 1:length(symbols)){
    prices_mod[,i] <- subset(prices, subset = (symbol == symbols[i]),
                             select = c(adjusted), drop = FALSE)
  }
  colnames(prices_mod) <- symbols
  rownames(prices_mod) <- prices$date[1:NROW(prices_mod)]
  
  if (tolower(return_period) == "daily"){
    # Calculate long-term returns
    asset_returns_long <-  
      prices_mod %>% 
      to.daily(OHLC = FALSE) %>% 
      tk_tbl(preserve_index = TRUE, rename_index = "date") %>%
      gather(asset, returns, -date) %>% 
      group_by(asset) %>%  
      mutate(returns = (log(returns) - log(lag(returns)))) %>% 
      na.omit()
  } else if (tolower(return_period) == "monthly"){
    asset_returns_long <-  
      prices_mod %>% 
      to.monthly(OHLC = FALSE) %>% 
      tk_tbl(preserve_index = TRUE, rename_index = "date") %>%
      gather(asset, returns, -date) %>% 
      group_by(asset) %>%  
      mutate(returns = (log(returns) - log(lag(returns)))) %>% 
      na.omit()
  } else{
    errorCondition('Not a valid return period. Please use "daily" or "monthly"')
  }
  
  # Isolating returns of individual assets
  returns <- data.frame(matrix(nrow = NROW(asset_returns_long)/length(symbols), 
                               ncol = length(symbols)))
  for (i in 1:length(symbols)){
    returns[,i] <- subset(asset_returns_long, subset = (asset == symbols[i]),
                          select = c(returns), drop = FALSE)
  }
  
  colnames(returns) <- symbols
  rownames(returns) <- asset_returns_long$date[1:NROW(returns)]
  
  # simulating returns
  portfolio_sim_growth <- data.frame(matrix(nrow = duration_pred + 1, ncol = nsim))
  for (i in 1:nsim){
    ## Create Simulated daily returns for specified number of days using mean and std
    simulated_daily_returns <- rmvnorm(n = duration_pred, 
                                       mean = colMeans(returns), sigma = cov(returns), 
                                       method = "eigen")
    simulated_daily_returns <- rbind(rep(init_invest, NCOL(simulated_daily_returns)),
                                     1 + simulated_daily_returns)
    
    ## Combine simulated returns into portfolio
    ### Multiply each column by corresponding portfolio weights
    portfolio_sim_growth[i] <- scale(simulated_daily_returns, center = FALSE, scale = 1/weights) %>%
      rowSums() %>%
      accumulate(`*`)
  }
  colnames(portfolio_sim_growth) <- paste(rep("Sim", NCOL(portfolio_sim_growth)),
                                          as.character(seq(1, NCOL(portfolio_sim_growth), 1)),
                                          sep = " ")
  portfolio_sim_growth <- round(portfolio_sim_growth, 2)
  return(portfolio_sim_growth)
}


# Testing MC sim function:
test_sim <- mc.simulate(symbols = t, weights = w, from = "2016-12-31", to = "2018-12-31",
                        init_invest = 1,
                        return_period = "daily",
                        duration_pred = 3*252,
                        nsim = 101)

# Simulation Statistics Histograms
hist(apply(test_sim, 2, mean))
hist(apply(test_sim, 2, sd))


##### CAGR FUNCTION #####
mc.cagr <- function(vec, period = "daily"){
  if (period == "daily"){
    cagr <- round((((vec[length(vec)]/vec[1])^(1/(length(vec)/252))) - 1) * 100, 2)
  } else if (period == "monthly"){
    cagr <- round((((vec[length(vec)]/vec[1])^(1/(length(vec)/12))) - 1) * 100, 2)
  }
  return(cagr)
}

# Testing MC sim CAGR function:
sim_cagr <- mapply(mc.cagr, test_sim, "monthly")
summary(sim_cagr)

##### CHARTS FUNCTION #####
mc.plot <- function(x, min.max.med = FALSE){
  # Plotting with hchart
  if (min.max.med == FALSE){
    # Data preparation
    data <- data.frame(stack(x[,1:NCOL(x)]))
    data$id <- as.character(rep(seq(1, NROW(x)), NCOL(x)))
    colnames(data) <- c("Growth", "Simulation", "Period")
    
    # Plotting simulations
    plt <- hchart(data,
                  type = 'line',
                  mapping = hcaes(x = Period,
                                  y = Growth,
                                  group = Simulation)) %>%
      hc_title(text = list("Simulated Portfolio Value")) %>%
      hc_xAxis(title = list(text = "Period")) %>%
      hc_yAxis(title = list(text = "Portfolio Growth"),
               labels = list(format = "${value}"))  %>%
      hc_add_theme(hc_theme_flat()) %>%
      hc_exporting(enabled = TRUE) %>% 
      hc_legend(enabled = FALSE)
    
  } else if (min.max.med == TRUE){
    # Data preparation
    min <- x[1:NROW(x), xts::last(x) == min(as.numeric(xts::last(x)))]
    med <- x[1:NROW(x), xts::last(x) == median(as.numeric(xts::last(x)))]
    max <- x[1:NROW(x), xts::last(x) == max(as.numeric(xts::last(x)))]
    
    x3m <- data.frame(min, med, max)
    
    
    data3m <- data.frame(stack(x3m[,1:NCOL(x3m)]))
    data3m$id <- as.character(rep(seq(1, NROW(x3m)), NCOL(x3m)))
    colnames(data3m) <- c("Growth", "Simulation", "Period")
    
    # Plotting simulations
    plt <- hchart(data3m,
                  type = 'line',
                  mapping = hcaes(x = Period,
                                  y = Growth,
                                  group = Simulation)) %>%
      hc_title(text = list("Simulated Portfolio Value")) %>%
      hc_xAxis(title = list(text = "Period")) %>%
      hc_yAxis(title = list(text = "Portfolio Growth"),
               labels = list(format = "${value}"))  %>%
      hc_add_theme(hc_theme_flat()) %>%
      hc_exporting(enabled = TRUE) %>% 
      hc_legend(enabled = FALSE)
  }
  return(plt)
}

# Testing plot function for MC sim:

# mc.plot(x = test_sim)
mc.plot(x = test_sim, min.max.med = TRUE)