# install.packages("quantmod")
# install.packages("PerformanceAnalytics")

library(dplyr)
library(ggplot2)
library(tidyr)
library(gganimate)
library(ggthemes)
library(httr)
library(jsonlite)
library(lubridate)

# setwd("C:/Users/orazz/OneDrive - The City University of New York (1)/MASTER/STA9750-2025-SPRING")
setwd("C:/Users/salda/OneDrive/Documents/money team")
FRED_key <- readLines("FRED_key.txt")
get_fred<- function(id){
  base_url <- "https://api.stlouisfed.org/fred/series/observations?series_id="
  res <- GET(paste0(base_url,id,"&api_key=",FRED_key,"&file_type=json"))
  res_content <- content(res, as = "text", encoding = "UTF-8")
  json <- fromJSON(res_content)
  data <-json$observations
  data <- data |> mutate(value = as.numeric(value),# immediately convert to usable format
                         date = as.Date(date))
  return(data)
}

# Read your Alpha Vantage key
AV_key <- readLines("Alphavantage_key.txt")

# Function to get data from Alpha Vantage for a given ticker
GET_AV <- function(ticker){
  
  # Build the URL for the monthly adjusted time series
  url <- paste0(
    "https://www.alphavantage.co/query?function=TIME_SERIES_MONTHLY_ADJUSTED",
    "&symbol=", ticker,
    "&apikey=", AV_key
  )
  
  # Make the GET request and parse the JSON response
  res <- GET(url)
  res_content <- content(res, as = "text", encoding = "UTF-8")
  j <- fromJSON(res_content, flatten = TRUE)
  
  # Extract the "Monthly Adjusted Time Series" data
  data <- j$`Monthly Adjusted Time Series`
  
  # Create empty vectors to store our data
  close <- c()
  adjusted_close <- c()
  low <- c()
  volume <- c()
  dividend <- c()
  
  # Loop over each element in the data to unpack the values.
  for(i in seq_along(data)){
    close <- append(close, data[[i]][["4. close"]])
    adjusted_close <- append(adjusted_close, data[[i]][["5. adjusted close"]])
    low <- append(low, data[[i]][["3. low"]])
    volume <- append(volume, data[[i]][["6. volume"]])
    dividend <- append(dividend, data[[i]][["7. dividend amount"]])
  }
  
  # Build the data frame
  df <- data.frame(
    date = as.Date(names(data)),
    close = as.numeric(close),
    adjusted_close = as.numeric(adjusted_close),
    low = as.numeric(low),
    volume = as.numeric(volume),
    dividend = as.numeric(dividend)
  )
  
  return(df)
}

# Fetch the data for each ticker
spy_data <- GET_AV("SPY")
mdy_data <- GET_AV("MDY")
iwm_data <- GET_AV("IWM")

# (Optional) Check the names in the data frame to confirm the columns
names(spy_data)  # Should include: "date", "close", "adjusted_close", "low", "volume", "dividend"

# Calculate log returns using the adjusted close
spy_data <- spy_data %>%
  arrange(date) %>%
  mutate(log_return = log(adjusted_close / lag(adjusted_close)))

mdy_data <- mdy_data %>%
  arrange(date) %>%
  mutate(log_return = log(adjusted_close / lag(adjusted_close)))

iwm_data <- iwm_data %>%
  arrange(date) %>%
  mutate(log_return = log(adjusted_close / lag(adjusted_close)))

# Plotting the log returns for SPY
ggplot(spy_data, aes(x = date, y = log_return)) +
  geom_line(color = "steelblue") +
  labs(
    title = "Monthly Log Returns of SPY (S&P 500 ETF)",
    x = "Date",
    y = "Log Return"
  )

# Plotting the log returns for SPY
ggplot(spy_data, aes(x = date, y = log_return)) +
  geom_line(color = "steelblue") +
  labs(title = "Monthly Log Returns of SPY (S&P 500 ETF)",
       x = "Date", y = "Log Return")


spy_avg_monthly_log_return <- mean(spy_data$log_return, na.rm = TRUE)
spy_vol_monthly_log_return <- sd(spy_data$log_return, na.rm = TRUE)

spy_annual_return <- spy_avg_monthly_log_return * 12
spy_annual_volatility <- spy_vol_monthly_log_return * sqrt(12)

cat("Annualized Return:", round(spy_annual_return * 100, 2), "%\n")
cat("Annualized Volatility:", round(spy_annual_volatility * 100, 2), "%\n")

mdy_avg_monthly_log_return <- mean(mdy_data$log_return, na.rm = TRUE)
mdy_vol_monthly_log_return <- sd(mdy_data$log_return, na.rm = TRUE)

mdy_annual_return <- mdy_avg_monthly_log_return * 12
mdy_annual_volatility <- mdy_vol_monthly_log_return * sqrt(12)

cat("Annualized Return:", round(mdy_annual_return * 100, 2), "%\n")
cat("Annualized Volatility:", round(mdy_annual_volatility * 100, 2), "%\n")

iwm_avg_monthly_log_return <- mean(iwm_data$log_return, na.rm = TRUE)
iwm_vol_monthly_log_return <- sd(iwm_data$log_return, na.rm = TRUE)

iwm_annual_return <- iwm_avg_monthly_log_return * 12
iwm_annual_volatility <- iwm_vol_monthly_log_return * sqrt(12)

cat("Annualized Return:", round(iwm_annual_return * 100, 2), "%\n")
cat("Annualized Volatility:", round(iwm_annual_volatility * 100, 2), "%\n")

#get Bond historical data
aaa_yield <- get_fred("BAMLCC0A1AAATRIV")|>   #ICE BofA AAA US Corporate Index Effective Yield
  select(date , AAA = value)
gov10y_yield <- get_fred("GS10")|>
  select(date , Gov10Y = value)
gov30y_yield <- get_fred("GS30")|>
  select(date , Gov30Y = value)
gov3m_yield <- get_fred("DTB3")|>
  select(date , Gov3m = value)

#combining the data
combined_bond <- reduce(list(
  aaa_yield ,
  gov10y_yield ,
  gov30y_yield ,
  gov3m_yield 
  
), full_join, by = "date") |> 
  drop_na()


#graph for gov bonds
combined_bond |> select(-AAA)|>
  pivot_longer(-date, names_to = "Rating", values_to = "Yield") |>
  ggplot(aes(x = date, y = Yield, color = Rating)) +
  geom_line() +
  labs(title = "Bond Yields by Rating", y = "Yield (%)")

# graph for corporate AAA bonds
combined_bond |> select(AAA,date)|>
  pivot_longer(-date, names_to = "Rating", values_to = "Yield") |>
  ggplot(aes(x = date, y = Yield, color = Rating)) +
  geom_line() +
  labs(title = "Bond Yields by Rating", y = "Yield (%)")

# bonds returns by calculating changes in yields
bond_returns <- combined_bond |>
  arrange(date) |>
  filter(Gov3m > 0) |>  # filter out zero yields
  mutate(
    AAA_return = log(AAA / lag(AAA)),
    Gov10Y_return = log(Gov10Y / lag(Gov10Y)),
    Gov30Y_return = log(Gov30Y / lag(Gov30Y)),
    Gov3m_return = log(Gov3m / lag(Gov3m))
  ) |>
  drop_na()

# Historical volatility (standard deviation of returns)
volatilities_bonds <- bond_returns |>
  summarize(
    AAA_vol = sd(AAA_return),
    Gov10Y_vol = sd(Gov10Y_return),
    Gov30Y_vol = sd(Gov30Y_return),
    Gov3m_vol = sd(Gov3m_return)
  )

# check starts and end dates
spy_data |> summarize(start = min(date), end = max(date))
mdy_data |> summarize(start = min(date), end = max(date))
iwm_data |> summarize(start = min(date), end = max(date))
bond_returns |> summarize(start = min(date), end = max(date))

# Create a month column in both stock and bond datasets
spy_data <- spy_data |> mutate(month = floor_date(date, "month"))
mdy_data <- mdy_data |> mutate(month = floor_date(date, "month"))
iwm_data <- iwm_data |> mutate(month = floor_date(date, "month"))
bond_returns <- bond_returns |> mutate(month = floor_date(date, "month"))

# Now merge them on the 'month' column
returns_data <- reduce(list(spy_data, mdy_data, iwm_data, bond_returns), 
                       full_join, by = "month") |> 
  drop_na()

# Check the date range of the merged data
cat("Merged date range: ", range(returns_data$month), "\n")



# Calculate average monthly returns
returns_data <- returns_data |> 
  mutate(
    spy_avg_monthly_return = mean(spy_data$log_return, na.rm = TRUE),
    mdy_avg_monthly_return = mean(mdy_data$log_return, na.rm = TRUE),
    iwm_avg_monthly_return = mean(iwm_data$log_return, na.rm = TRUE),
    aaa_avg_monthly_return = mean(bond_returns$AAA_return, na.rm = TRUE),
    gov10y_avg_monthly_return = mean(bond_returns$Gov10Y_return, na.rm = TRUE),
    gov30y_avg_monthly_return = mean(bond_returns$Gov30Y_return, na.rm = TRUE),
    gov3m_avg_monthly_return = mean(bond_returns$Gov3m_return, na.rm = TRUE)
  )

# Calculate monthly volatility (standard deviation of returns)
returns_data <- returns_data |> 
  mutate(
    spy_vol_monthly = sd(spy_data$log_return, na.rm = TRUE),
    mdy_vol_monthly = sd(mdy_data$log_return, na.rm = TRUE),
    iwm_vol_monthly = sd(iwm_data$log_return, na.rm = TRUE),
    aaa_vol_monthly = sd(bond_returns$AAA_return, na.rm = TRUE),
    gov10y_vol_monthly = sd(bond_returns$Gov10Y_return, na.rm = TRUE),
    gov30y_vol_monthly = sd(bond_returns$Gov30Y_return, na.rm = TRUE),
    gov3m_vol_monthly = sd(bond_returns$Gov3m_return, na.rm = TRUE)
  )

# Calculate annualized returns and volatility
returns_data <- returns_data |> 
  mutate(
    # Annualized Returns (Multiply monthly return by 12)
    spy_annualized_return = spy_avg_monthly_return * 12,
    mdy_annualized_return = mdy_avg_monthly_return * 12,
    iwm_annualized_return = iwm_avg_monthly_return * 12,
    aaa_annualized_return = aaa_avg_monthly_return * 12,
    gov10y_annualized_return = gov10y_avg_monthly_return * 12,
    gov30y_annualized_return = gov30y_avg_monthly_return * 12,
    gov3m_annualized_return = gov3m_avg_monthly_return * 12,
    
    # Annualized Volatility (Multiply monthly volatility by sqrt(12))
    spy_annualized_volatility = spy_vol_monthly * sqrt(12),
    mdy_annualized_volatility = mdy_vol_monthly * sqrt(12),
    iwm_annualized_volatility = iwm_vol_monthly * sqrt(12),
    aaa_annualized_volatility = aaa_vol_monthly * sqrt(12),
    gov10y_annualized_volatility = gov10y_vol_monthly * sqrt(12),
    gov30y_annualized_volatility = gov30y_vol_monthly * sqrt(12),
    gov3m_annualized_volatility = gov3m_vol_monthly * sqrt(12)
  )

# Check results
returns_data |> select(contains("annualized")) |> head()






#get inflation data
cpi_data <- get_fred("CPIAUCSL")|>
  select(date , CPI = value)
# get wage data
wage_data <- get_fred("CES0500000003")|>
  select(date , WAGE = value)


ggplot(cpi_data, aes(x = date, y = CPI)) +
  geom_line(color = "steelblue") +
  labs(title = "Consumer Price Index (CPI)",
       x = "Date", y = "Index Value")

ggplot(wage_data, aes(x = date, y = WAGE)) +
  geom_line(color = "darkgreen") +
  labs(title = "Average Hourly Earnings (Total Private)",
       x = "Date", y = "Earnings (USD)")


rebased_data <- bind_rows(
  spy_data |> select(date, close) |> mutate(asset = "SPY"),
  mdy_data |> select(date, close) |> mutate(asset = "MDY"),
  iwm_data |> select(date, close) |> mutate(asset = "IWM")
) |> 
  group_by(asset) |> 
  arrange(date) |> 
  mutate(rebased = close / first(close) * 100)

ggplot(rebased_data, aes(x = date, y = rebased, color = asset)) +
  geom_line() +
  labs(title = "Rebased Asset Prices (Starting at 100)", y = "Index (100 = Start)")



# Define the user's age
user_age <- 30  # Example: 30 years old

# Define strategy based on age
strategy <- if (user_age < 35) "Aggressive" else if (user_age < 55) "Balanced" else "Conservative"

# Define asset allocation by strategy
strategy_alloc <- switch(strategy,
                         "Aggressive" = c(Stocks = 0.8, Bonds = 0.15, Alternatives = 0.05),
                         "Balanced" = c(Stocks = 0.6, Bonds = 0.35, Alternatives = 0.05),
                         "Conservative" = c(Stocks = 0.4, Bonds = 0.55, Alternatives = 0.05)
)

cat("Portfolio Strategy: ", strategy, "\n")
cat("Asset Allocation: ", strategy_alloc, "\n")

# Define the assets mapped to stocks and bonds
stock_assets <- c("SPY", "MDY", "IWM")
bond_assets <- c("AAA", "Gov10Y", "Gov30Y", "Gov3m")

# Extract the weights for the stock and bond assets
stock_weight <- strategy_alloc["Stocks"]
bond_weight <- strategy_alloc["Bonds"]

# Determine how to distribute the stock weight across the three ETFs
stock_etf_weight <- stock_weight / length(stock_assets)

# Print out the allocated weights for each asset class
cat("Stock Weights: \n")
stock_assets |> purrr::walk(~ cat(.x, ":", round(stock_etf_weight * 100, 2), "%\n"))

cat("\nBond Weights: \n")
bond_assets |> purrr::walk(~ cat(.x, ":", round(bond_weight / length(bond_assets) * 100, 2), "%\n"))

# Set the number of simulations and years for the forecast
num_simulations <- 500
years <- 20
num_months <- years * 12

# Function to perform the Monte Carlo simulation
monte_carlo_simulation <- function(weights, asset_returns, num_simulations, num_months) {
  simulation_results <- matrix(NA, nrow = num_months, ncol = num_simulations)
  
  for (i in 1:num_simulations) {
    # Randomly sample returns for each asset from historical returns
    sampled_returns <- sample(asset_returns, num_months, replace = TRUE)
    
    # Portfolio return for this simulation (weighted returns)
    portfolio_returns <- sum(weights * sampled_returns)
    
    # Calculate the portfolio value over time (compounded)
    portfolio_value <- cumprod(1 + portfolio_returns)
    
    # Replace any NA values with 0 (or another appropriate value like 1)
    portfolio_value[is.na(portfolio_value)] <- 0  # Replace NA with 0
    simulation_results[, i] <- portfolio_value
  }
  
  return(simulation_results)
}

# Run Monte Carlo simulations for the portfolio
simulation_results <- monte_carlo_simulation(weights = c(rep(stock_etf_weight, 3), rep(bond_weight / 4, 4)),
                                             asset_returns = asset_returns,
                                             num_simulations = num_simulations,
                                             num_months = num_months)

# Convert the simulation results to a data frame for ggplot
simulation_results_df <- as.data.frame(simulation_results)
simulation_results_df$date <- seq.Date(from = Sys.Date(), by = "month", length.out = num_months)

# Plot the simulation paths (use the correct column reference style)

simulation_results_long <- simulation_results_df %>%
  gather(key = "simulation", value = "portfolio_value", -date)

# Plot using ggplot (tidy data format)
ggplot(simulation_results_long, aes(x = date, y = portfolio_value, group = simulation)) +
  geom_line(alpha = 0.1, color = "blue") +
  labs(title = "Monte Carlo Simulation of Portfolio Value",
       y = "Portfolio Value", x = "Date")



# Load quantmod package
library(quantmod)

# Get Bitcoin data from Yahoo Finance (BTC-USD)
getSymbols("BTC-USD", src = "yahoo", from = "2010-01-01", to = Sys.Date())

# Extract adjusted closing prices
`BTC-USD` <- `BTC-USD`|>arrange(date) |> 
  mutate(log_return = log(Close / lag(Close)))

# Calculate log returns for Bitcoin
btc_returns <- diff(log(btc_data)) * 100  # Annualized log returns (in percentage)

# Plot Bitcoin Log Returns
plot(btc_returns, main = "Bitcoin Log Returns (BTC-USD)", col = "orange", type = "l")






# Plot Bitcoin Log Returns
ggplot(btc_data, aes(x = date, y = log_return)) +
  geom_line(color = "orange") +
  labs(title = "Monthly Log Returns of Bitcoin",
       x = "Date", y = "Log Return")

#  REIT data (VNQ) from Alpha Vantage
reit_data <- GET_AV("VNQ")

# Calculating log returns for REIT
reit_data <- reit_data |> 
  arrange(date) |> 
  mutate(log_return = log(close / lag(close)))

# Plot REIT Log Returns
ggplot(reit_data, aes(x = date, y = log_return)) +
  geom_line(color = "purple") +
  labs(title = "Monthly Log Returns of REIT (VNQ)",
       x = "Date", y = "Log Return")







# Prepare the returns matrix as you already have it
returns_matrix <- returns_data |>
  select(spy_annualized_return, mdy_annualized_return, iwm_annualized_return,
         aaa_annualized_return, gov10y_annualized_return, gov30y_annualized_return, gov3m_annualized_return) |>
  as.matrix()

# Calculate the covariance matrix of the returns
cov_matrix <- cov(returns_matrix)

# Calculate the expected returns (mean returns)
expected_returns <- colMeans(returns_matrix)

# Define the number of assets in the portfolio
num_assets <- length(expected_returns)

# Define portfolio specification
port_spec <- portfolio.spec(assets = colnames(returns_matrix))

# Add objective functions to the portfolio specification
port_spec <- add.objective(port_spec, type = "return", name = "mean")
port_spec <- add.objective(port_spec, type = "risk", name = "StdDev")

# Optimize the portfolio (Maximize return, minimize risk)
opt_portfolio <- optimize.portfolio(returns_matrix, port_spec, optimize_method = "ROI", trace = TRUE)

# Get the optimal weights
optimal_weights <- opt_portfolio$weights

# Calculate the expected portfolio return
portfolio_return <- sum(optimal_weights * expected_returns)

# Calculate the expected portfolio volatility (standard deviation)
portfolio_volatility <- sqrt(t(optimal_weights) %*% cov_matrix %*% optimal_weights)

# Print the results
cat("Optimal portfolio weights:", optimal_weights, "\n")
cat("Expected Portfolio Return:", portfolio_return, "\n")
cat("Expected Portfolio Volatility:", portfolio_volatility, "\n")

