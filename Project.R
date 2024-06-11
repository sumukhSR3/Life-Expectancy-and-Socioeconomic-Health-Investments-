# Load necessary libraries
library(readr)
library(ggplot2)

# Load the data
unemployment_data <- read_csv("/Users/sumukhramagiri/Desktop/Forecasting/unemployment.csv")
View(unemployment_data)
# Convert DATE to Date type
unemployment_data$DATE <- as.Date(unemployment_data$DATE)

# Plotting the unemployment rate over time
ggplot(unemployment_data, aes(x = DATE, y = UNRATE)) +
  geom_line() +
  labs(title = "Unemployment Rate Over Time", x = "Date", y = "Unemployment Rate (%)") +
  theme_minimal()


library(readr)
library(ggplot2)
library(zoo)

# Adding a numeric time index for regression
unemployment_data$TimeIndex <- as.numeric(as.Date(unemployment_data$DATE))

# Linear Regression
trend_model <- lm(UNRATE ~ TimeIndex, data = unemployment_data)

# Predict values to plot the trend line
unemployment_data$TrendLine <- predict(trend_model)

# Plotting the unemployment rate and the trend line
ggplot(unemployment_data, aes(x = DATE)) +
  geom_line(aes(y = UNRATE), colour = "blue") +
  geom_line(aes(y = TrendLine), colour = "red", linetype = "dashed") +
  labs(title = "Unemployment Rate and Trend Line Over Time",
       x = "Date", y = "Unemployment Rate (%)") +
  theme_minimal()


library(zoo)
# Calculate the rolling mean with a window size, for example, 12 for monthly data with an annual cycle
unemployment_data$RollingMean <- rollapply(unemployment_data$UNRATE, width = 12, FUN = mean, by = 1, align = "center", fill = NA)

# Plot the rolling mean with the original data
plot(unemployment_data$DATE, unemployment_data$UNRATE, type = "l", col = "blue")
lines(unemployment_data$DATE, unemployment_data$RollingMean, type = "l", col = "red")


library(tseries)
adf_test_result <- adf.test(unemployment_data$UNRATE, alternative = "stationary")




# ACF and PACF plots
acf(unemployment_data$UNRATE, main="ACF for Unemployment Rate")
pacf(unemployment_data$UNRATE, main="PACF for Unemployment Rate")

