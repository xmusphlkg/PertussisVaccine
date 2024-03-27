
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

# AU ----------------------------------------------------------------------

# sequence stationary test
adf_result <- adf.test(ts_au, alternative = 'stationary')

# Sequence randomness
bl_test <- Box.test(ts_au, lag = 12, type = 'Ljung-Box')

# Cross-Correlation
fig1 <- autoplot(acf(ts_au, plot = F), main = 'a') + 
     labs(caption = paste("ADF Test p-value: ", round(adf_result$p.value, 4)))
fig2 <- autoplot(pacf(ts_au, plot = F), main = 'b')+
     labs(caption = paste("Ljung-Box Test p-value: ", round(bl_test$p.value, 4)))

# validation
ts_train <- window(ts_au, start = c(2015, 1), end = c(2019, 6))
ts_test <- window(ts_au, start = c(2019, 7), end = c(2019, 12))
model <- auto.arima(ts_train, seasonal = T, ic = 'aicc')
model_forecast <- forecast(model, h = 6)
SMAPE <- smape(ts_test, model_forecast$mean)
fig3 <- autoplot(model_forecast, main = 'c') + 
     geom_line(aes(x = time(ts_test), y = ts_test), color = "red") +
     labs(caption = paste("SMAPE: ", round(SMAPE, 2), "%"),
          x = 'Date', y = 'Monthly incidence')

ggsave('./appendix/S2.png',
       fig1 + fig2 + fig3,
       width = 9, height = 4, dpi = 300)

# CN ----------------------------------------------------------------------

# sequence stationary test
adf_result <- adf.test(ts_cn, alternative = 'stationary')

# Sequence randomness
bl_test <- Box.test(ts_cn, lag = 12, type = 'Ljung-Box')

# Cross-Correlation
fig1 <- autoplot(acf(ts_cn, plot = F), main = 'a') + 
     labs(caption = paste("ADF Test p-value: ", round(adf_result$p.value, 4)))
fig2 <- autoplot(pacf(ts_cn, plot = F), main = 'b') +
     labs(caption = paste("Ljung-Box Test p-value: ", round(bl_test$p.value, 4)))

# validation
ts_train <- window(ts_cn, start = c(2015, 1), end = c(2019, 6))
ts_test <- window(ts_cn, start = c(2019, 7), end = c(2019, 12))
model <- auto.arima(ts_train, seasonal = T, ic = 'aicc')
model_forecast <- forecast(model, h = 6)
SMAPE <- smape(ts_test, model_forecast$mean)
fig3 <- autoplot(model_forecast, main = 'c') + 
     geom_line(aes(x = time(ts_test), y = ts_test), color = "red") +
     labs(caption = paste("SMAPE: ", round(SMAPE, 2), "%"),
          x = 'Date', y = 'Monthly incidence')

ggsave('./appendix/S3.png',
       fig1 + fig2 + fig3,
       width = 9, height = 4, dpi = 300)

# US ----------------------------------------------------------------------

# sequence stationary test
adf_result <- adf.test(ts_us, alternative = 'stationary')

# Sequence randomness
bl_test <- Box.test(ts_us, lag = 54, type = 'Ljung-Box')

# Cross-Correlation
fig1 <- autoplot(acf(ts_us, plot = F), main = 'a') + 
     labs(caption = paste("ADF Test p-value: ", round(adf_result$p.value, 4)))
fig2 <- autoplot(pacf(ts_us, plot = F), main = 'b') +
     labs(caption = paste("Ljung-Box Test p-value: ", round(bl_test$p.value, 4)))

# validation
ts_train <- window(ts_us, start = 2015, end = 2019.5)
ts_test <- window(ts_us, start = 2019.5, end = 2020)
model <- auto.arima(ts_train, seasonal = T, ic = 'aicc')
model_forecast <- forecast(model, h = 26)
SMAPE <- smape(ts_test, model_forecast$mean)
fig3 <- autoplot(model_forecast, main = 'c') + 
     geom_line(aes(x = time(ts_test), y = ts_test), color = "red") +
     labs(caption = paste("SMAPE: ", round(SMAPE, 2), "%"),
          x = 'Date', y = 'Weekly incidence')

ggsave('./appendix/S4.png',
       fig1 + fig2 + fig3,
       width = 9, height = 4, dpi = 300)

# UK ----------------------------------------------------------------------

# sequence stationary test
adf_result <- adf.test(ts_uk, alternative = 'stationary')

# Sequence randomness
bl_test <- Box.test(ts_uk, lag = 12, type = 'Ljung-Box')

# Cross-Correlation
fig1 <- autoplot(acf(ts_uk, plot = F), main = 'a') + 
     labs(caption = paste("ADF Test p-value: ", round(adf_result$p.value, 4)))
fig2 <- autoplot(pacf(ts_uk, plot = F), main = 'b') +
     labs(caption = paste("Ljung-Box Test p-value: ", round(bl_test$p.value, 4)))

# validation
ts_train <- window(ts_uk, start = 2015, end = 2019.5)
ts_test <- window(ts_uk, start = 2019.5, end = 2020)

model <- auto.arima(ts_train, seasonal = T, ic = 'aicc')
model_forecast <- forecast(model, h = 26)
SMAPE <- smape(ts_test, model_forecast$mean)
fig3 <- autoplot(model_forecast, main = 'c') + 
     geom_line(aes(x = time(ts_test), y = ts_test), color = "red") +
     labs(caption = paste("SMAPE: ", round(SMAPE, 2), "%"),
          x = 'Date', y = 'Monthly incidence')

ggsave('./appendix/S5.png',
       fig1 + fig2 + fig3,
       width = 9, height = 4, dpi = 300)

# model 2019 --------------------------------------------------------------

## AU
ts_train <- window(ts_au, start = c(2015, 1), end = c(2019, 12))
ts_observed <- window(ts_au, start = c(2020, 1), end = c(2020, 12))
model <- auto.arima(ts_train, seasonal = T, ic = 'aicc')
outcome <- forecast(model, h = length(ts_observed))
df_model <- data.frame(
     date = zoo::as.Date(time(outcome$mean)),
     mean = as.matrix(outcome$mean),
     CI_lower = as.matrix(outcome$lower),
     CI_upper = as.matrix(outcome$upper),
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
ts_train <- window(ts_cn, start = c(2015, 1), end = c(2019, 12))
ts_observed <- window(ts_cn, start = c(2020, 1), end = c(2020, 12))
model <- auto.arima(ts_train, seasonal = T, ic = 'aicc')
outcome <- forecast(model, h = length(ts_observed))
df_model <- data.frame(
     date = zoo::as.Date(time(outcome$mean)),
     mean = as.matrix(outcome$mean),
     CI_lower = as.matrix(outcome$lower),
     CI_upper = as.matrix(outcome$upper),
     observed = as.matrix(ts_observed),
     country = 'CN',
     model = '2015-2021'
) |> 
     bind_rows(df_model)
df_fit <- data.frame(
     date = zoo::as.Date(time(ts_train)),
     simu = as.numeric(as.matrix(outcome$x)),
     fit = as.numeric(as.matrix(outcome$fitted)),
     country = 'CN',
     model = '2015-2021'
) |> 
     bind_rows(df_fit)

## US
ts_train <- window(ts_us, start = 2015, end = 2020)
ts_observed <- window(ts_us, start = 2020, end = 2021)
model <- auto.arima(ts_train, seasonal = T, ic = 'aicc')
outcome <- forecast(model, h = length(ts_observed))
df_model <- data.frame(
     date = as.Date(df_us$Date[261:312]),
     mean = as.matrix(outcome$mean),
     CI_lower = as.matrix(outcome$lower),
     CI_upper = as.matrix(outcome$upper),
     observed = as.matrix(ts_observed),
     country = 'US',
     model = '2015-2021'
) |> 
     bind_rows(df_model)
df_fit <- data.frame(
     date = as.Date(df_us$Date[1:260]),
     simu = as.numeric(as.matrix(outcome$x)),
     fit = as.numeric(as.matrix(outcome$fitted)),
     country = 'US',
     model = '2015-2021'
) |> 
     bind_rows(df_fit)

## UK
ts_train <- window(ts_uk, start = 2015, end = 2020)
ts_observed <- window(ts_uk, start = 2020, end = 2021)
model <- auto.arima(ts_train)
outcome <- forecast(model, h = length(ts_observed))
df_model <- data.frame(
     date = as.Date(df_uk$Date[261:312]),
     mean = as.matrix(outcome$mean),
     CI_lower = as.matrix(outcome$lower),
     CI_upper = as.matrix(outcome$upper),
     observed = as.matrix(ts_observed),
     country = 'UK',
     model = '2015-2021'
) |> 
     bind_rows(df_model)
df_fit <- data.frame(
     date = as.Date(df_uk$Date[1:260]),
     simu = as.numeric(as.matrix(outcome$x)),
     fit = as.numeric(as.matrix(outcome$fitted)),
     country = 'UK',
     model = '2015-2021'
) |> 
     bind_rows(df_fit)

df_list <- list(df_model, df_fit)

write.xlsx(df_list, './Fig Data/fig2.xlsx')

