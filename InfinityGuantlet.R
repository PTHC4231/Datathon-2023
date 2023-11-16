# Project: Datathon 'enhanced and automated code'

# Libraries
packages_to_install <- c("h2o", "forecast", "ggplot2", "gridExtra", "readr", "kableExtra", "lmtest", "tseries")
install.packages(packages_to_install)

library(h2o)
library(forecast)
library(ggplot2)
library(gridExtra)
library(readr)
library(kableExtra)
library(lmtest) # for Durbin-Watson Test
library(tseries) # for Ljung-Box Test

# Initialize H2O cluster
h2o.init(nthreads = -1)  # Use all available CPU cores

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

# Convert training data to H2OFrame
data_train_h2o <- as.h2o(data_train)

# Use H2O's AutoML for training and selecting the best model
aml <- h2o.automl(y = "Anomaly", training_frame = data_train_h2o, max_models = 10, seed = 1, project_name = "time_series_forecasting")

# Display the AutoML leaderboard
leaderboard <- aml@leaderboard
print(leaderboard)

# Predict on the test set using H2O's best model
predictions <- h2o.predict(aml@leader, newdata = as.h2o(data_test))

# Convert H2O predictions to a vector
predictions_vec <- as.vector(predictions$predict)

# Calculate MAPE for the H2O predictions
mape <- mean(abs((data_test - predictions_vec) / data_test))
print(paste("MAPE on the test set using H2O: ", round(mape * 100, 2), "%", sep = ""))

# Visualization comparing original test data and H2O predictions
autoplot(data_test, series = "Actual", col = 'indianred4') +
  autolayer(predictions_vec, series = "Predicted with H2O", col = 'blue') +
  theme_bw(base_size = 14) +
  ggtitle('Actual vs. Predicted Anomalies with H2O') +
  xlab('Year') + ylab('Temperature (Celsius)')

# SARIMA Forecasting (as in your enhanced code)
sarima_forecast <- function(x, h) {
  fit <- auto.arima(x, seasonal=TRUE)
  return(forecast(fit, h = h)$mean)
}
error_sarima <- tsCV(data_ts, sarima_forecast, h = h)

# Residual Analysis for SARIMA
fit_sarima <- auto.arima(data_train, seasonal=TRUE)
checkresiduals(fit_sarima)

# Diagnostics: Durbin-Watson Test
dwtest(fit_sarima$residuals ~ 1)

# Diagnostics: Ljung-Box Test
Box.test(fit_sarima$residuals, lag=24, type="Ljung-Box")

# Visualization of SARIMA forecast
forecast_sarima <- forecast(fit_sarima, h = 36)
plot(forecast_sarima)

# Cross-validation for STL (as in original code)
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

# Shut down the H2O cluster
h2o.shutdown(prompt = FALSE)

