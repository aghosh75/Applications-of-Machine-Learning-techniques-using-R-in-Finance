#Here we are going to forecast the stock prices of Amazon for the year 2020 based on historical prices for 2014-19.

# We need the following Libraries
library(prophet)
library(lubridate)
library(ggplot2) 

# Importing amazon stock price data
data <- read.csv("~/LearningR/AMZN.csv")
str(data)
head(data)
data$Date <- dmy(data$Date)
#Plotting the data
qplot(Date, Close, data = data, main = 'Amazon closing Prices 2014-2019')

# Log transformation
ds <- data$Date
y <- log(data$Close)
df <- data.frame(ds, y)

#Plotting the logarithmic value of closing price data
qplot(ds, y, data = df, main = 'Amazon closing Prices 2014-2019')

# Forecasting with Facebook's prophet package
m <- prophet(df)
str(m)
#Making forecast for the next 365 days
future <- make_future_dataframe(m, periods = 365)
#Seeing the last few values of future
tail(future)
#Now we will store the predictions
forecast <- predict(m, future)

# Plot forecast
plot(m, forecast)
#Another interactive plot
dyplot.prophet(m, forecast)
#Plotting the components
prophet_plot_components(m, forecast)

# Model performance
str(forecast)
pred <- forecast$yhat[1:1258]
actual <- m$history$y
plot(actual, pred)
#Drawing ab line of best fit
abline(lm(pred~actual), col = 'red')
#Getting R-square
summary(lm(pred~actual))

#Some further model performance metrics
x <- cross_validation(m, 365, units = 'days')
performance_metrics(x, rolling_window = 0.1)   # Using 10% rolling window
plot_cross_validation_metric(x,
                             metric = 'mape',
                             rolling_window = 0.1)
