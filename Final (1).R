# Project: Datathon 'enhanced code'

# Libraries
packages_to_install <- c("forecast", "ggplot2", "gridExtra", "readr", "kableExtra", "lmtest", "tseries", "zoo")
install.packages(packages_to_install)

library(forecast)
library(ggplot2)
library(gridExtra)
library(readr)
library(kableExtra)
library(lmtest)  # for Durbin-Watson Test
library(tseries)  # for Ljung-Box Test
library(zoo)  # for aggregation functions

# Import the dataset
data_raw <- read.csv("/Users/pranavchundi/Downloads//Datathon_Fall2023_Dataset.csv", stringsAsFactors = F, header = T)

# Convert to time series
data_ts <- ts(data_raw$Anomaly, start = 1850, frequency = 12)

# Aggregating data to yearly averages
data_yearly <- aggregate(data_ts, nfrequency = 1, FUN = mean)

# Train-test split
h <- 3  # Adjusted for yearly data
data_train <- window(data_yearly, end = c(2019 - 1850 + 1))
data_test <- window(data_yearly, start = c(2019 - 1850 + 2))

# Fit an ARIMA model
fit_arima <- auto.arima(data_train, seasonal=FALSE, stepwise=FALSE, approximation=FALSE)
forecast_arima <- forecast(fit_arima, h = h)

# Calculating MAPE for ARIMA forecast
mape_arima <- mean(100 * abs((data_test - forecast_arima$mean) / data_test))
print(paste("ARIMA MAPE on the test set with yearly data: ", round(mape_arima, 2), "%", sep = ""))

# ... [the beginning of the code remains unchanged]

# Visualize ARIMA forecast on yearly data
if (frequency(data_test_yearly) == frequency(forecast_arima_yearly$mean)) {
  autoplot(data_test_yearly, series = "Actual", col = 'indianred4') +
    autolayer(forecast_arima_yearly$mean, series = "Predicted", col = 'blue') +
    theme_bw(base_size = 14) +
    ggtitle('Actual vs. Predicted Anomalies (ARIMA with Yearly Data)') +
    xlab('Year') + ylab('Temperature (Celsius)')
} else {
  cat("Frequency mismatch between test and forecasted data for yearly ARIMA model.\n")
}

# ... [rest of the code remains unchanged]

# Visualize ARIMA forecast on monthly data
if (frequency(data_test) == frequency(forecast_arima$mean)) {
  autoplot(data_test, series = "Actual", col = 'indianred4') +
    autolayer(forecast_arima$mean, series = "Predicted", col = 'blue') +
    theme_bw(base_size = 14) +
    ggtitle('Actual vs. Predicted Anomalies (ARIMA with Monthly Data)') +
    xlab('Year') + ylab('Temperature (Celsius)')
} else {
  cat("Frequency mismatch between test and forecasted data for monthly ARIMA model.\n")
}

# Further analysis and models can be added as needed


# ... [Continue with any other desired analysis or models]
# Cross-Validation (using naive STL as in original code)
window <- 36
fstl <- function(x, h) {
  forecast(stlf(x, h = h))
}
error <- tsCV(data_ts, fstl, h = h, window = window)

# cv_mape function
cv_mape <- function(error, actual) {
  actual_table <- data.frame(matrix(NA, nrow = length(actual), ncol = h))
  for(i in 1:(length(actual)-window)) {
    if ((i+window+h-1) <= length(actual)) {
      actual_table[i+window-1, ] <- actual[(i+window):(i+window+h-1)]
    } else {
      actual_table[i+window-1, 1:(length(actual)-(i+window-1))] <- actual[(i+window):(length(actual))]
    }
  }
  return(100*mean(abs(as.matrix(error) / as.matrix(actual_table)), na.rm = T))
}

# Performance Table
perf_stl <- data.frame(rbind(
  cbind('rmse',
        formatC(round(accuracy(forecast_sarima)[ , 'RMSE'], 5), format = 'f', digits = 5),
        formatC(round(sqrt(mean((data_test - forecast_sarima$mean)^2)), 5), format = 'f', digits = 5),
        formatC(round(sqrt(mean(error^2, na.rm = T)), 5), format = 'f', digits = 5)),
  cbind('mae',
        formatC(round(accuracy(forecast_sarima)[ , 'MAE'], 5), format = 'f', digits = 5),
        formatC(round(mean(abs(data_test - forecast_sarima$mean)), 5), format = 'f', digits = 5),
        formatC(round(mean(abs(error), na.rm = T), 5), format = 'f', digits = 5)),
  cbind('mape',
        formatC(round(accuracy(forecast_sarima)[ , 'MAPE'], 5), format = 'f', digits = 5),
        formatC(round(mean(100*(abs(data_test - forecast_sarima$mean)) / data_test), 5), format = 'f', digits = 5),
        formatC(round(cv_mape(error, data_ts), 5), format = 'f', digits = 5))
), stringsAsFactors = F)

# Table Visualization
kable(perf_stl, caption = 'Performance - Temperature Anomalies horizon = 12, window = 36', align = 'r', col.names = c('', 'train', 'test', 'cv')) %>%
  kable_styling(full_width = F, position = 'l') %>%
  column_spec(2, width = '7em') %>%
  column_spec(3, width = '4.5em') %>%
  column_spec(4, width = '4.5em')



