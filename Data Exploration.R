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


# Calculate log returns using the adjusted close
spy_data <- spy_data |>
  arrange(date) |>
  mutate(log_return = log(adjusted_close / lag(adjusted_close)))

mdy_data <- mdy_data |>
  arrange(date) |>
  mutate(log_return = log(adjusted_close / lag(adjusted_close)))

iwm_data <- iwm_data |>
  arrange(date) |>
  mutate(log_return = log(adjusted_close / lag(adjusted_close)))


GET_AV("VXUS") |>
  arrange(date) |>
  mutate(log_return = log(adjusted_close / lag(adjusted_close)))

# Plotting the log returns for SPY
ggplot(spy_data, aes(x = date, y = log_return)) +
  geom_line(color = "steelblue") +
  labs(
    title = "Monthly Log Returns of SPY (S&P 500 ETF)",
    x = "Date",
    y = "Log Return"
  )


# VXUS VS SPY (International vs US)

# Retrieve data for SPY and VXUS, add a ticker column, and combine the datasets
combined_data <- bind_rows(
  GET_AV("SPY") |> 
    arrange(date) |> 
    mutate(ticker = "SPY"),
  GET_AV("VXUS") |> 
    arrange(date) |> 
    mutate(ticker = "VXUS")
)

# Filter to include data only for the past 10 years
filtered_data <- combined_data |> 
  filter(date >= Sys.Date() - years(14))

# Plot the adjusted close prices for SPY vs VT over the past 15 years
filtered_data |> 
  ggplot(aes(x = date, y = adjusted_close, color = ticker)) +
  geom_line() +
  labs(
    title = "Adjusted Close Price for SPY and VXUS (Past 20 Years)",
    x = "Date",
    y = "Adjusted Close Price",
    color = "Ticker"
  ) +
  theme_minimal()

library(dplyr)
library(lubridate)

filtered_data <- combined_data |>
  filter(date >= Sys.Date() - years(14))

# Calculate the Total Return 
returns_summary <- filtered_data |>
  group_by(ticker) |>
  arrange(date) |>
  summarise(
    initial_value = first(adjusted_close),
    final_value   = last(adjusted_close),
    # Total Return percentage
    total_return_pct = (final_value / initial_value - 1) * 100,
    # Calculate number of years i
    years = as.numeric(difftime(last(date), first(date), units = "days")) / 365.25,
    # CAGR percentage:
    CAGR_pct = ((final_value / initial_value)^(1 / years) - 1) * 100
  )

print(returns_summary)
