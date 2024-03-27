
library(tidyverse)
library(lubridate)
library(ggsci)
library(patchwork)
library(forecast)
library(tseries)
library(openxlsx)

fill_color <- pal_npg()(3)

# Load data
df_clean <- read.xlsx('./Fig Data/fig1.xlsx', detectDates = T)

# Function to calculate MAPE
smape <- function(actual, forecast) {
     n <- length(actual)
     sum_val <- sum(2 * abs(forecast - actual) / (abs(actual) + abs(forecast)))
     smape_val <- (1 / n) * sum_val * 100
     return(smape_val)
}

set.seed(20240321)

df_au <- filter(df_clean, Country == 'AU') |> arrange(Date)
ts_au <- ts(df_au$Cases, start = c(2015, 1), frequency = 12)

df_cn <- filter(df_clean, Country == 'CN') |> arrange(Date)
ts_cn <- ts(df_cn$Cases, start = c(2015, 1), frequency = 12)

df_us <- filter(df_clean, Country == 'US') |> arrange(Date)
ts_us <- ts(df_us$Cases, start = decimal_date(ymd(df_us$Date[1])), frequency = 365.25/7)

df_uk <- filter(df_clean, Country == 'UK') |> arrange(Date)
ts_uk <- ts(df_uk$Cases, start = decimal_date(ymd(df_uk$Date[1])), frequency = 365.25/7)

# model 2019 --------------------------------------------------------------

## AU
ts_train <- window(ts_au, start = c(2015, 1), end = c(2023, 6))
ts_observed <- window(ts_au, start = c(2023, 7), end = c(2024, 2))
model <- auto.arima(ts_train, seasonal = T, ic = 'aicc')
outcome <- forecast(model, h = length(ts_observed))
df_model <- data.frame(
     date = zoo::as.Date(time(outcome$mean)),
     mean = as.matrix(outcome$mean),
     observed = as.matrix(ts_observed),
     country = 'AU',
     model = '2015-2019'
)
df_fit <- data.frame(
     date = zoo::as.Date(time(ts_train)),
     simu = as.numeric(as.matrix(outcome$x)),
     fit = as.numeric(as.matrix(outcome$fitted)),
     country = 'AU',
     model = '2015-2019'
)

## CN
ts_train <- window(ts_cn, start = c(2015, 1), end = c(2023, 6))
ts_observed <- window(ts_cn, start = c(2023, 7), end = c(2024, 2))
model <- auto.arima(ts_train, seasonal = T, ic = 'aicc')
outcome <- forecast(model, h = length(ts_observed))
df_model <- data.frame(
     date = zoo::as.Date(time(outcome$mean)),
     mean = as.matrix(outcome$mean),
     observed = as.matrix(ts_observed),
     country = 'CN',
     model = '2015-2023'
) |> 
     bind_rows(df_model)
df_fit <- data.frame(
     date = zoo::as.Date(time(ts_train)),
     simu = as.numeric(as.matrix(outcome$x)),
     fit = as.numeric(as.matrix(outcome$fitted)),
     country = 'CN',
     model = '2015-2023'
) |> 
     bind_rows(df_fit)

## US
ts_train <- window(ts_us, start = 2015, end = 2023.5)
ts_observed <- window(ts_us, start = 2023.5, end = 2024.5)
model <- auto.arima(ts_train, seasonal = T, ic = 'aicc')
outcome <- forecast(model, h = length(ts_observed))
df_model <- data.frame(
     date = as.Date(df_us$Date[444:479]),
     mean = as.matrix(outcome$mean),
     observed = as.matrix(ts_observed),
     country = 'US',
     model = '2015-2023'
) |> 
     bind_rows(df_model)
df_fit <- data.frame(
     date = as.Date(df_us$Date[1:443]),
     simu = as.numeric(as.matrix(outcome$x)),
     fit = as.numeric(as.matrix(outcome$fitted)),
     country = 'US',
     model = '2015-2023'
) |> 
     bind_rows(df_fit)

## UK
ts_train <- window(ts_uk, start = 2015, end = 2023.5)
ts_observed <- window(ts_uk, start = 2023.5, end = 2024.5)
model <- auto.arima(ts_train)
outcome <- forecast(model, h = length(ts_observed))
df_model <- data.frame(
     date = as.Date(df_uk$Date[444:479]),
     mean = as.matrix(outcome$mean),
     observed = as.matrix(ts_observed),
     country = 'UK',
     model = '2015-2023'
) |> 
     bind_rows(df_model)
df_fit <- data.frame(
     date = as.Date(df_uk$Date[1:443]),
     simu = as.numeric(as.matrix(outcome$x)),
     fit = as.numeric(as.matrix(outcome$fitted)),
     country = 'UK',
     model = '2015-2023'
) |> 
     bind_rows(df_fit)

df_list <- list(df_model, df_fit)

write.xlsx(df_list, './Fig Data/fig4.xlsx')

