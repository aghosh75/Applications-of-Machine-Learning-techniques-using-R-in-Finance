# Importing S&P stock price data
data <- read.csv("~/LearningR/S_P.csv")
str(data)
head(data)

#Needed packages
install.packages("zoo")
install.packages("FinTS")
install.packages("e1071")
install.packages("rugarch")
install.packages("parallel")
install.packages("Quandl")
install.packages("tseries")

library(FinTS)
library(zoo)
library(e1071)
library(rugarch)
library(tseries)
library(parallel)
library(Quandl)
library(ggplot2)
library(tseries)


#Plotting the S&P index
qplot(Date, SP, data = data, main = 'S&P closing Prices 2014-2019')
plot.ts(data$SP) 

#How to get volatility
sp <- data$SP
n<- length(sp)
returns.sp <- sp[-n]/sp[-1] -1
rsp <-returns.sp

# Now we apply GARCH model stepwise in 5 steps.
#Step 1: Plotting the time series.
plot.ts(rsp)

#Step 2: Testing for ARCH
ArchTest(rsp)
# So, the null hypothesis is rejected and there is presence of ARCH effect.

#Step 3: Running the GARCH model.
garch(rsp,grad = "numerical", trace = FALSE)

# This shows that there is a ARCH(1) and GARCH(1) effect. That is one-period lagged of squered residuals and and one-period lag of squared returns of S&P have signficnat effect on the current period return on S&P.
# Hence, GARCH (1,1) is the best fit model

#Step 4: Specifying and saving the GARCH model output.
x<- ugarchspec(variance.model=list(garchorder=c(1,1)),mean.model=list(armaorder=c(0,0)))
x_fit<-ugarchfit(x, data= rsp)
x_fit

# From the model we can infer: Present period volatility = 0.000004 +0.210908*previous period squared residual + 0.738566*previous period variance

#Step 5: Forecasting of volatility
exrtf <- ugarchforecast(x_fit, n.ahead = 365)
exrtf


#Now we are going to apply the Exponential GARCH (EGARCH) model on the same S&P returns data. 
# In order to apply this model we need to have a Leverage effect, that is negative correlation between past return and volatility of future returns.

#Step 1
x = ugarchspec(variance.model= list(model = "eGARCH", garchOrder = c(1,1)),mean.model= list(armaOrder = c(0,0)))
x_fit = ugarchfit(spec = x, data =rsp)
x_fit

#Interpreting the results.
#Both ARCH and GARCH effects, given by alpha1 and beta1 are positive and significant.
#Based on the output, gamma1 represents the EGARCH value. It is positive and significant. 
# In order to apply EGARCH model, gamma1 needs to be negative. So, there is no leverage effect.
# so, we cannot apply EGARCH model, but simply use Standard GARCH as done earlier.