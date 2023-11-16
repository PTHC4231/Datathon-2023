# Load necessary libraries
install.packages(c("h2o", "forecast", "ggplot2", "gridExtra", "readr", "kableExtra"))
library(h2o)
library(forecast)
library(ggplot2)
library(gridExtra)
library(readr)
library(kableExtra)

# Initialize H2O cluster
h2o.init(nthreads = -1)  # Use all available CPU cores

# Import the dataset
data_raw <- read.csv("/Users/pranavchundi/Downloads//Datathon_Fall2023_Dataset.csv", stringsAsFactors = F, header = T)

# Convert to time series
data_ts <- ts(data_raw$Anomaly, start = 1850, frequency = 12)

# Train-test split
h <- 36
data_train <- window(data_ts, end = c(2019, 08))
data_test <- window(data_ts, start = c(2019, 09))

# Convert training data to H2OFrame
data_train_h2o <- as.h2o(data_train)

# Use H2O's AutoML to train and select the best model
aml <- h2o.automl(y = "Anomaly", training_frame = data_train_h2o, max_models = 10, seed = 1, project_name = "time_series_forecasting")

# Display the AutoML leaderboard
leaderboard <- aml@leaderboard
print(leaderboard)

# Predict on the test set
predictions <- h2o.predict(aml@leader, newdata = as.h2o(data_test))

# Convert H2O predictions to a vector
predictions_vec <- as.vector(predictions$predict)

# Calculate MAPE
mape <- mean(abs((data_test - predictions_vec) / data_test))

# Print MAPE
print(paste("MAPE on the test set: ", round(mape * 100, 2), "%", sep = ""))

# Plot the original test data and predictions
autoplot(data_test, series = "Actual", col = 'indianred4') +
  autolayer(predictions_vec, series = "Predicted", col = 'blue') +
  theme_bw(base_size = 14) +
  ggtitle('Actual vs. Predicted Anomalies') +
  xlab('Year') + ylab('Temperature (Celsius)')

# Shut down the H2O cluster
h2o.shutdown(prompt = FALSE)
