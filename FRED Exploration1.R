library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)

# Read and trim your FRED API key
FRED_key <- readLines("FRED_key.txt") |> trimws()

# Define the get_fred function to retrieve FRED series data
get_fred <- function(id) {
  base_url <- "https://api.stlouisfed.org/fred/series/observations?series_id="
  url <- paste0(base_url, id, "&api_key=", FRED_key, "&file_type=json")
  
  res <- GET(url)
  res_content <- content(res, as = "text", encoding = "UTF-8")
  json <- fromJSON(res_content, flatten = TRUE)
  
  if (is.null(json$observations)) {
    warning("No data found for series ID ", id, ". Check your API key or series ID. Full response: ", res_content)
    return(data.frame())
  }
  
  data <- json$observations
  data <- data |> mutate(
    value = as.numeric(value),
    date = as.Date(date)
  )
  return(data)
}

# Retrieve U.S. economic data:
# U.S. GDP Growth (quarterly percent change in real GDP)
us_gdp_growth <- get_fred("NGDPSAXDCUSQ")
# U.S. Inflation (Consumer Price Index for All Urban Consumers: All Items)
us_inflation <- get_fred("CPIAUCSL")
# U.S. Unemployment Rate (monthly unemployment rate)
us_unemployment <- get_fred("UNRATE")

# Retrieve U.K. GDP Growth (used as a proxy for England's GDP growth;
# FRED typically covers the entire United Kingdom)
uk_gdp_growth <- get_fred("CLVMNACSCAB1GQGUK")

# Retrieve Eurozone GDP Growth (annual percent change in real GDP for the Euro Area)
eurozone_gdp_growth <- get_fred("NAEXKP01EZQ661S")

# Retrieve China GDP Growth (annual % growth; World Bank indicator)
china_gdp_growth <- get_fred("NGDPXDCCNA")



# Preview the data for each series
cat("US GDP Growth:\n")
print(head(us_gdp_growth))

cat("\nUS Inflation (CPI):\n")
print(head(us_inflation))

cat("\nUS Unemployment Rate:\n")
print(head(us_unemployment))


cat("\nEurozone GDP Growth:\n")
print(head(eurozone_gdp_growth))

cat("\nChina GDP Growth:\n")
print(head(china_gdp_growth))
library(ggplot2)
library(lubridate)

# Plot for U.S. GDP Growth
ggplot(us_gdp_growth, aes(x = date, y = value)) +
  geom_line(size = 1.5, color = "steelblue") +
  labs(
    title = "U.S. GDP Growth",
    x = "Date",
    y = "GDP Growth (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Plot for U.S. Inflation (CPI)
ggplot(us_inflation, aes(x = date, y = value)) +
  geom_line(size = 1.5, color = "darkred") +
  labs(
    title = "U.S. Inflation (CPI)",
    x = "Date",
    y = "CPI Value"
  ) +
  theme_minimal() +
  theme(legend.position = "none")





library(ggplot2)

ggplot(china_gdp_growth, aes(x = date, y = value)) +
  geom_line(size = 1.5, color = "blue") +
  labs(
    title = "China GDP Growth (Annual %)",
    x = "Date",
    y = "GDP Growth (%)"
  ) +
  theme_minimal()




library(ggplot2)
library(lubridate)
library(dplyr)

# Aggregate U.S. GDP Growth (quarterly series) to yearly by averaging quarterly values:
us_gdp_growth_yearly <- us_gdp_growth |>
  mutate(year = year(date)) |>
  group_by(year) |>
  summarise(value = mean(value, na.rm = TRUE)) |>
  ungroup() |>
  mutate(date = as.Date(paste0(year, "-12-31")),
         region = "US")

# For China GDP Growth (already annual), assign a region label:
china_gdp_growth <- china_gdp_growth |>
  mutate(region = "China")

# Combine U.S. and China data:
combined_growth <- bind_rows(us_gdp_growth_yearly, china_gdp_growth)

# Plot the combined U.S. vs. China GDP Growth (yearly)
ggplot(combined_growth, aes(x = date, y = value, color = region)) +
  geom_line(size = 1.5) +
  labs(
    title = "GDP Growth: US vs. China (Yearly)",
    x = "Date",
    y = "GDP Growth (%)",
    color = "Region"
  ) +
  theme_minimal() +
  theme(legend.key.size = unit(1.5, "lines"),
        legend.text = element_text(size = 12))
