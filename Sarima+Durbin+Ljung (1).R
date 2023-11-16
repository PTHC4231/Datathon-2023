# Project: Datathon 'enhanced code'

# Libraries
packages_to_install <- c("forecast", "ggplot2", "gridExtra", "readr", "kableExtra", "lmtest", "tseries")
install.packages(packages_to_install)

library(forecast)
library(ggplot2)
library(gridExtra)
library(readr)
library(kableExtra)
library(lmtest) # for Durbin-Watson Test
library(tseries) # for Ljung-Box Test

# Import the dataset
data_raw <- read.csv("/Users/pranavchundi/Downloads//Datathon_Fall2023_Dataset.csv", stringsAsFactors = F, header = T)

# Convert to time series
data_ts <- ts(data_raw$Anomaly, start = 1850, frequency = 12)

# Initial Data Visualization
autoplot(data_ts, col = 'indianred4') +
  theme_bw(base_size = 14) +
  ggtitle('Global Land and Ocean Temperature Anomalies') +
  xlab('Year') + ylab('Temperature (Celsius)')

# Time Series Decomposition Visualization
data_decomposed <- stl(data_ts, s.window="periodic")
plot(data_decomposed)

# Train-test split
h <- 36
data_train <- window(data_ts, end = c(2019, 08))
data_test <- window(data_ts, start = c(2019, 09))

# SARIMA Forecasting
sarima_forecast <- function(x, h) {
  fit <- auto.arima(x, seasonal=TRUE)
  return(forecast(fit, h = h)$mean)
}
error_sarima <- tsCV(data_ts, sarima_forecast, h = h)

# Residual Analysis for SARIMA
fit_sarima <- auto.arima(data_train, seasonal=TRUE)
checkresiduals(fit_sarima)

# Diagnostics: Durbin-Watson Test (to detect autocorrelation in residuals)
dwtest(fit_sarima$residuals ~ 1)

# Diagnostics: Ljung-Box Test (to test the overall randomness based on a number of lags)
Box.test(fit_sarima$residuals, lag=24, type="Ljung-Box")

# Final Forecast Visualization
forecast_sarima <- forecast(fit_sarima, h = 36)
plot(forecast_sarima)

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

# 1. ACF and PACF plots
par(mfrow=c(2,1))  # Setting the plot window to have 2 rows and 1 column
acf(data_ts, main="Autocorrelation Function")
pacf(data_ts, main="Partial Autocorrelation Function")

# 2. Seasonal decomposition plots
decomposed <- stl(data_ts, s.window="periodic")
plot(decomposed)

# 3. Histogram of residuals (from the SARIMA model)
hist(fit_sarima$residuals, breaks=50, col="lightblue", main="Histogram of Residuals", xlab="Residuals")
abline(v=0, col="red", lwd=2)

# 4. Actual vs. Predicted
predicted <- forecast_sarima$mean
actual <- data_test

# Ensure the lengths are the same for plotting
length_diff <- length(predicted) - length(actual)
if (length_diff > 0) {
  predicted <- predicted[1:length(predicted) - length_diff]
} else if (length_diff < 0) {
  actual <- actual[1:length(actual) + length_diff]
}

plot(actual, type="l", col="red", ylim=range(c(actual, predicted)), main="Actual vs. Predicted", ylab="Temperature (Celsius)", xlab="Time")
lines(predicted, col="blue")
legend("topright", legend=c("Actual", "Predicted"), col=c("red", "blue"), lty=1)



                