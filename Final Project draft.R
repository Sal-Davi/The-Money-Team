library(dplyr)
library(ggplot2)
library(tidyr)
library(gganimate)
library(ggthemes)
library(httr)
library(jsonlite)
library(lubridate)

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
AV_key <- readLines("Alphavantage_key.txt")

GET_AV <- function(ticker){
  
  url <-paste0("https://www.alphavantage.co/query?function=TIME_SERIES_MONTHLY_ADJUSTED","&symbol=",ticker,"&apikey=",AV_key)
  res <- GET(url)
  res_content<-(content(res,as = "text",encoding = "UTF-8"))
  j <- fromJSON(res_content,flatten = TRUE)
  data <- j$`Monthly Adjusted Time Series`
  #### unpacking the data with a for loop
  
  close <- c() #empty list to contain the list
  low <- c()
  volume <- c()
  div <- c()
  
  for(i in seq(1:length(data))){
    close <- append(close,data[[i]][["4. close"]])
    low <- append(low,data[[i]][["3. low"]])
    volume <- append(volume,data[[i]][["6. volume"]])
    div <-append(div,data[[i]][["7. dividend amount"]])
  }
  df <- data.frame(date = as.Date(names(data)),
                   close = as.numeric(close),
                   low = as.numeric(low),
                   volume = as.numeric(volume),
                   dividend = as.numeric(div))
  return(df)
}

# get S&P 500 Data
spy_data <- GET_AV("SPY")

# For Mid-Cap (MDY) S&P MidCap 400 
mdy_data <- GET_AV("MDY")

# For Small-Cap (IWM) Russell 2000 
iwm_data <- GET_AV("IWM")


# calculating log returns
spy_data <- spy_data |> 
  arrange(date) |> 
  mutate(log_return = log(close / lag(close)))

mdy_data <- mdy_data |> 
  arrange(date) |> 
  mutate(log_return = log(close / lag(close)))

iwm_data <- iwm_data |> 
  arrange(date) |> 
  mutate(log_return = log(close / lag(close)))


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






