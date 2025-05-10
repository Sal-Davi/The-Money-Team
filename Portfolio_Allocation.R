library(dplyr)
library(ggplot2)
library(tidyr)
library(gganimate)
library(ggthemes)
library(httr)
library(jsonlite)
library(lubridate)
library(purrr)
library(PortfolioAnalytics)
library(quantmod)
library(PerformanceAnalytics)

setwd("C:/Users/orazz/OneDrive - The City University of New York (1)/MASTER/STA9750-2025-SPRING")

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
  url <- paste0(
    "https://www.alphavantage.co/query?function=TIME_SERIES_MONTHLY_ADJUSTED",
    "&symbol=", ticker,
    "&apikey=", AV_key
  )
  
  res <- GET(url)
  res_content <- content(res, as = "text", encoding = "UTF-8")
  j <- fromJSON(res_content, flatten = TRUE)
  
  # Check if data exists
  if (is.null(j$`Monthly Adjusted Time Series`)) {
    print(j)  # to show what was returned
    stop(paste("No data returned for", ticker, "- possibly API limit reached or invalid ticker."))
  }
  
  data <- j$`Monthly Adjusted Time Series`
  
  df <- data.frame(
    date = as.Date(names(data)),
    close = as.numeric(sapply(data, `[[`, "4. close")),
    adjusted_close = as.numeric(sapply(data, `[[`, "5. adjusted close")),
    low = as.numeric(sapply(data, `[[`, "3. low")),
    volume = as.numeric(sapply(data, `[[`, "6. volume")),
    dividend = as.numeric(sapply(data, `[[`, "7. dividend amount"))
  )
  
  return(df)
}



# get S&P 500 Data
spy_data <- GET_AV("SPY")

# For Mid-Cap (MDY) S&P MidCap 400  
mdy_data <- GET_AV("MDY")

# For Small-Cap (IWM) Russell 2000 
iwm_data <- GET_AV("IWM")

# international ETF 
vxus_data <- GET_AV("VXUS")


# calculating log returns
spy_data <- spy_data |>
  arrange(date) |>
  mutate(log_return = log(adjusted_close / lag(adjusted_close)))

mdy_data <- mdy_data |>
  arrange(date) |>
  mutate(log_return = log(adjusted_close / lag(adjusted_close)))

iwm_data <- iwm_data |>
  arrange(date) |>
  mutate(log_return = log(adjusted_close / lag(adjusted_close)))

vxus_data <- vxus_data|>
  arrange(date)|>
  mutate(log_return = log(adjusted_close / lag(adjusted_close)))


#get Bond historical data
aaa_yield <- get_fred("BAMLCC0A1AAATRIV")|>   #ICE BofA AAA US Corporate Index Effective Yield
  select(date , AAA = value)
gov10y_yield <- get_fred("GS10")|>
  select(date , Gov10Y = value)
gov30y_yield <- get_fred("GS30")|>
  select(date , Gov30Y = value)
gov3m_yield <- get_fred("DTB3")|>
  select(date , Gov3m = value)

resample_to_monthly <- function(df, yield_col) {
  df |>
    mutate(month = format(date, "%Y-%m")) |>
    group_by(month) |>
    summarise(!!yield_col := ifelse(
      # If there is monthly data, keep it; otherwise, take the last daily data point
      any(!is.na(!!sym(yield_col))),
      last(!!sym(yield_col), na.rm = TRUE),  # Last value of the month, if daily data exists
      first(!!sym(yield_col))),  # Otherwise, keep the first monthly data
      .groups = "drop") |>
    mutate(date = as.Date(paste0(month, "-01")))|> select(-month)
}

# Apply to all the datasets
aaa_yield_monthly <- resample_to_monthly(aaa_yield, "AAA")
gov10y_yield_monthly <- resample_to_monthly(gov10y_yield, "Gov10Y")
gov30y_yield_monthly <- resample_to_monthly(gov30y_yield, "Gov30Y")
gov3m_yield_monthly <- resample_to_monthly(gov3m_yield, "Gov3m")

# Combine all bond yield data (now on a monthly basis)
combined_bond_yields <- reduce(
  list(aaa_yield_monthly, gov10y_yield_monthly, gov30y_yield_monthly, gov3m_yield_monthly),
  full_join,
  by = "date"
)


yield_data_long <- combined_bond_yields |>
  pivot_longer(-date, names_to = "ticker", values_to = "yield") |>
  arrange(ticker, date) |>
  mutate(
    log_return = (yield / 100),  # Monthly return approximation (annual yield divided by 100)
    adjusted_close = yield      # adjusted_close should be the yield value for bonds
  ) |>
  select(date, ticker, adjusted_close, log_return)


# commodities 

combined_data_commodities <- bind_rows(
  GET_AV("VNQ") |>
    arrange(date) |>
    mutate(ticker = "VNQ"),
  GET_AV("USO") |>
    arrange(date) |>
    mutate(ticker = "USO"),
  GET_AV("GLD") |>
    arrange(date) |>
    mutate(ticker = "GLD"),
  GET_AV("DBA") |>
    arrange(date) |>
    mutate(ticker = "DBA")
)

combined_data_commodities <- combined_data_commodities|> 
  group_by(ticker)|>
  mutate(log_return = log(adjusted_close / lag(adjusted_close)))|>
  ungroup()


combined_data_stock <- bind_rows(
  spy_data|> mutate(ticker = "SPY"),
  mdy_data|> mutate(ticker = "MDY"),
  iwm_data|> mutate(ticker = "IWM"),
  vxus_data|> mutate(ticker = "VXUS")
)



combined_data <- bind_rows(
  combined_data_stock,
  combined_data_commodities,
  yield_data_long
)

start_date <- as.Date("2005-01-01") # Not NECESSARY
combined_data <- combined_data |>
  filter(date >= start_date)


yearly_returns <- combined_data |>
  drop_na(log_return) |>
  group_by(ticker) |>
  arrange(date) |>
  summarise(
    initial_price = first(adjusted_close),
    final_price   = last(adjusted_close),
    years         = as.numeric(difftime(last(date), first(date), units = "days")) / 365.25,
    total_return_pct = (final_price / initial_price - 1) * 100,
    CAGR_pct      = ((final_price / initial_price)^(1 / years) - 1) * 100,
    volatility    = sd(log_return, na.rm = TRUE) * sqrt(12),
    sharpe_ratio  = (CAGR_pct / 100 - 0.02) / (volatility / 100)
  )



get_portfolio_weights <- function(age) {
  if (age < 30) {
    weights <- c(stocks = 0.90, bonds = 0.05, commodities = 0.05)
  } else if (age >= 30 & age <= 34) {
    weights <- c(stocks = 0.80, bonds = 0.10, commodities = 0.10)
  } else if (age >= 35 & age <= 39) {
    weights <- c(stocks = 0.75, bonds = 0.15, commodities = 0.10)
  } else if (age >= 40 & age <= 44) {
    weights <- c(stocks = 0.70, bonds = 0.20, commodities = 0.10)
  } else if (age >= 45 & age <= 49) {
    weights <- c(stocks = 0.65, bonds = 0.25, commodities = 0.10)
  } else if (age >= 50 & age <= 54) {
    weights <- c(stocks = 0.575, bonds = 0.30, commodities = 0.125)
  } else if (age >= 55 & age <= 59) {
    weights <- c(stocks = 0.50, bonds = 0.35, commodities = 0.15)
  } else if (age >= 60 & age <= 64) {
    weights <- c(stocks = 0.40, bonds = 0.45, commodities = 0.15)
  } else if (age >= 65 & age <= 69) {
    weights <- c(stocks = 0.30, bonds = 0.55, commodities = 0.15)
  } else {
    weights <- c(stocks = 0.15, bonds = 0.65, commodities = 0.20)
  }
  return(weights)
}

# Function to assign weights based on Sharpe ratios within each class
assign_weights_by_age <- function(yearly_returns, age) {
  allocation <- get_portfolio_weights(age)
  
  # Define ticker groups
  stock_tickers <- c("SPY", "MDY", "IWM", "VXUS")
  bond_tickers <- c("AAA", "Gov10Y", "Gov30Y", "Gov3m")
  commodity_tickers <- c("DBA", "USO", "VNQ", "GLD")
  
  # Tag each asset by class
  yearly_returns <- yearly_returns |>
    mutate(class = case_when(
      ticker %in% stock_tickers ~ "stocks",
      ticker %in% bond_tickers ~ "bonds",
      ticker %in% commodity_tickers ~ "commodities",
      TRUE ~ "other"
    ))
  
  # Separate bonds to handle fallback logic
  bonds <- yearly_returns |> filter(class == "bonds")
  other_assets <- yearly_returns |> filter(class != "bonds")
  
  # Check if all bond Sharpe ratios are <= 0
  all_bond_sharpe_negative <- all(bonds$sharpe_ratio <= 0 | is.na(bonds$sharpe_ratio))
  
  if (all_bond_sharpe_negative) {
    # Use yield-to-risk ratio for bonds if Sharpe ratios are all bad
    bonds <- bonds |>
      mutate(
        yield_to_risk = ifelse(volatility == 0, 0, CAGR_pct / volatility),
        raw_weight = yield_to_risk / sum(yield_to_risk, na.rm = TRUE),
        final_weight = raw_weight * allocation[["bonds"]]
      )
  } else {
    # Use Sharpe as usual
    bonds <- bonds |>
      mutate(
        adj_sharpe = ifelse(is.na(sharpe_ratio) | sharpe_ratio < 0, 0, sharpe_ratio),
        raw_weight = adj_sharpe / sum(adj_sharpe, na.rm = TRUE),
        final_weight = raw_weight * allocation[["bonds"]]
      )
  }
  
  # Handle other asset classes (stocks, commodities) with Sharpe
  other_assets <- other_assets |>
    filter(class %in% names(allocation)) |>
    group_by(class) |>
    mutate(
      adj_sharpe = ifelse(is.na(sharpe_ratio) | sharpe_ratio < 0, 0, sharpe_ratio),
      raw_weight = adj_sharpe / sum(adj_sharpe, na.rm = TRUE),
      final_weight = raw_weight * allocation[class]
    ) |>
    ungroup()
  
  # Combine and return
  bind_rows(bonds, other_assets) |>
    select(ticker, class, CAGR_pct, volatility, sharpe_ratio, final_weight)
}


