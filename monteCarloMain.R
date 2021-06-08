library(tidyquant)
library(tidyverse)
library(openxlsx)
library(timetk)
library(broom)
library(highcharter)
library(mvtnorm)


#### PART 1 ####

#### Read in current portfolio tickers & weights
data <- read.xlsx("weights.xlsx", sheet = "Weights") # portfolio data frame
symbols <- data$Tickers # symbols
w <- data$Weights # tickers
####

#### Pulling prices from yahoo & reformatting data table
prices <- 
  tq_get(symbols, get= "stock.prices", 
         from = "2016-12-31",
         to = "2019-12-31",
         complete_cases = TRUE) 

prices_mod <- data.frame(matrix(nrow = NROW(prices)/length(symbols), 
                                ncol = length(symbols)))
for (i in 1:length(symbols)){
  prices_mod[,i] <- subset(prices, subset = (symbol == symbols[i]),
                           select = c(adjusted), drop = FALSE)
}

colnames(prices_mod) <- symbols
rownames(prices_mod) <- prices$date[1:NROW(prices_mod)]
####

#### Calculate daily log returns
asset_returns_long <-  
  prices_mod %>% 
  to.daily(OHLC = FALSE) %>% 
  tk_tbl(preserve_index = TRUE, rename_index = "date") %>%
  gather(asset, returns, -date) %>% 
  group_by(asset) %>%  
  mutate(returns = (log(returns) - log(lag(returns)))) %>% 
  na.omit()
####

#### Isolating daily returns of individual assets (reformatting to be user-friendly)
returns <- data.frame(matrix(nrow = NROW(asset_returns_long)/length(symbols), 
                             ncol = length(symbols)))
for (i in 1:length(symbols)){
  returns[,i] <- subset(asset_returns_long, subset = (asset == symbols[i]),
                        select = c(returns), drop = FALSE)
}

colnames(returns) <- symbols
rownames(returns) <- asset_returns_long$date[1:NROW(returns)]
####

#### Create Simulated daily returns for 252 days (1 year) using mean and std (!!!!!)
simulated_daily_returns <- rmvnorm(n = 252,
                                   mean = colMeans(returns), # vector of asset means
                                   sigma = cov(returns),  # covariance matrix
                                   method = "eigen")
####

#### Combining asset returns into portfolio returns (!!!!!)
portfolio_sim_returns <- scale(simulated_daily_returns, center = FALSE, scale = 1/w) %>%
  rowSums()
#### 

#### Adding a baseline investment ($1) (!!!!!)
simulated_returns_add_1 <- 
  tibble(c(1, 1 + portfolio_sim_returns)) %>% 
  `colnames<-`("returns")
####

#### Simulated Growth function using accumulate (!!!!!!!)
accumulated_growth <- accumulate(simulated_returns_add_1, `*`)
####

#### Compute and round cagr (!!!!!!!)
cagr <- (((accumulated_growth[length(accumulated_growth)]/accumulated_growth[1])^(1/(length(accumulated_growth)/252))) - 1) * 100
cagr <- round(cagr, 2)
#### 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# #### PART 2 ####
# 
# #### Create 51 simulations
# sims <- 51
# starts <- 
#   rep(1, sims) %>%
#   set_names(paste("sim", 1:sims, sep = ""))
# 
# #### Monte Carlo the 51 simulations based on portfolio
# monte_carlo_sim_51 <- 
#   map_dfc(starts, 
#           simulation_accum_1, 
#           N = 120, 
#           mean = mean_port_return, 
#           stdev = stddev_port_return)
# ####
# 
# #### Adds month column
# monte_carlo_sim_51 <- 
#   monte_carlo_sim_51 %>% 
#   mutate(month = seq(1:nrow(.))) %>% 
#   select(month, everything()) %>% 
#   `colnames<-`(c("month", names(starts))) %>% 
#   mutate_all(funs(round(., 2))) 
# ####
# 
# #### Create 51 reruns function which should run the simulation 51 times
# reruns <- 51
# 
# monte_carlo_rerun_51 <- 
#   rerun(.n = reruns, 
#         simulation_accum_1(1, 
#                            120,
#                            mean_port_return, 
#                            stddev_port_return)) %>%
#   simplify_all() %>% 
#   `names<-`(paste("sim", 1:reruns, sep = " ")) %>%
#   as_tibble() %>% 
#   mutate(month = seq(1:nrow(.))) %>% 
#   select(month, everything())
# ####
# 
# #### Plots the simulation
# monte_carlo_sim_51 %>% 
#   gather(sim, growth, -month) %>% 
#   group_by(sim) %>% 
#   ggplot(aes(x = month, y = growth, color = sim)) + 
#   geom_line() +
#   theme(legend.position="none")
# ####
# 
# #### Summarizes the data from the simulation
# sim_summary <- 
#   monte_carlo_sim_51 %>% 
#   gather(sim, growth, -month) %>% 
#   group_by(sim) %>% 
#   summarise(final = last(growth)) %>% 
#   summarise(
#     max = max(final), 
#     min = min(final),
#     median = median(final))
# sim_summary
# ####
# 
# #### Plots only min, max, and median
# monte_carlo_sim_51 %>% 
#   gather(sim, growth, -month) %>% 
#   group_by(sim) %>%
#   filter(
#     last(growth) == sim_summary$max || 
#       last(growth) == sim_summary$median ||
#       last(growth) == sim_summary$min) %>% 
#   ggplot(aes(x = month, y = growth)) + 
#   geom_line(aes(color = sim)) 
# ####
# 
# #### Converts data from wide to long tidy format
# mc_gathered <- 
#   monte_carlo_sim_51 %>% 
#   gather(sim, growth, -month) %>% 
#   group_by(sim)
# ####
# 
# #### Plots hcChart with all simulations
# hchart(mc_gathered, 
#        type = 'line', 
#        hcaes(y = growth,
#              x = month,
#              group = sim)) %>% 
#   hc_title(text = "51 Simulations") %>%
#   hc_xAxis(title = list(text = "months")) %>%
#   hc_yAxis(title = list(text = "dollar growth"),
#            labels = list(format = "${value}")) %>%
#   hc_add_theme(hc_theme_flat()) %>%
#   hc_exporting(enabled = TRUE) %>% 
#   hc_legend(enabled = FALSE)
# ####
# 
# #### Filters data for min, max, and median
# mc_max_med_min <- 
#   mc_gathered %>%
#   filter(
#     last(growth) == sim_summary$max || 
#       last(growth) == sim_summary$median ||
#       last(growth) == sim_summary$min) %>% 
#   group_by(sim)
# ####
# 
# #### Plots min, max, and median using hcChart
# hchart(mc_max_med_min, 
#        type = 'line', 
#        hcaes(y = growth,
#              x = month,
#              group = sim)) %>% 
#   hc_title(text = "Min, Max, Median Simulations") %>%
#   hc_xAxis(title = list(text = "months")) %>%
#   hc_yAxis(title = list(text = "dollar growth"),
#            labels = list(format = "${value}")) %>%
#   hc_add_theme(hc_theme_flat()) %>%
#   hc_exporting(enabled = TRUE) %>% 
#   hc_legend(enabled = FALSE)
# ####

#### PART 3 ####

# Create GUI
# Allow for user inputs (Turn code into functions)
# Run the simulation and show results
# iff ^^ gets done: Look into more advanced statistical methods
