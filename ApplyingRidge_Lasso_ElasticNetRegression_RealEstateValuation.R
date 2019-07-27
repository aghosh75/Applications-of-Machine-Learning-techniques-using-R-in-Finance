#Libraries Needed
library(caret)
library(glmnet)
library(mlbench)
library(psych)

#Importing the datset
data <- read.csv("~/LearningPython/UCI_Repository_database/RealEstateValuation.csv", header = TRUE)
str(data)

# We want to create a prediction model of house price as a function of other variables.
pairs.panels(data[c(-8)])  #Exclude the 1th variable which is the no. of observations, 2nd variable is the transaction data, and the target variable that is 8th. 
pairs.panels(data[c(-8)], cex = 2)  #To make the plots bigger. 

#When independent variables are highly correlated that leads to multicollinearity problems. This leads to overfifting. 
#So three ways out:
#1. Ridge regression - shrinks coefficients to non-zero values to prevent overfit, but keeps all variables.
#2. Lasso regression - Shrinks regression coefficients, with some shrunk to zero. Thus it also helps with feature selection. 
#3. Elastic Net regression - mix of ridge and lasso regressions. 

#Data Partition
set.seed(222)  #This ensures we get repeatable results. 
ind <- sample(2, nrow(data), replace = T, prob = c(0.7, 0.3))
train <- data[ind == 1,]
test <- data[ind == 2,] 


#Custom control parameters
#cv stands for cross verification. verbose iteration let's us see what is going on. 
# In 10-fold cross validation training data is broken into 10 parts.
# Then model is made from 9 parts and one part is used for error estimation.
# This is repeated 5 times with a different part used for error estimation.

custom <- trainControl(method = "repeatedcv", number = 10, repeats = 5, verboseIter = T)    

#Linear model
set.seed(1234)
lm <- train(Y.house.price.of.unit.area  ~ .,train, method = 'lm', trControl = custom)

#Results
lm$results
lm
summary(lm)
plot(lm$finalModel)

#Ridge regression
# It tries to shrink the coefficients but keeps all variables in the model.
set.seed(1234)
ridge <- train(Y.house.price.of.unit.area~.,train, method = 'glmnet', tuneGrid = expand.grid(alpha=0, lambda =seq(0.0001,1, length = 5)), trControl = custom)
# When lamda increases it makes coefficents to shrink.
summary(ridge) 

#Plot results
plot(ridge)
ridge
plot(ridge$finalModel, xvar = "lambda", label = T)
plot(ridge$finalModel, xvar = "dev", label = T)
plot(varImp(ridge, scale = F))
#Changes the scale to 0 and 100.
plot(varImp(ridge, scale = T))   



#Lasso Regression
# It does both shrinkage as well as feature selection
# If there is a group of highly correlated variables that are causing multicollinearity, lasso regression will select one variable from the group and ignore the others. 
set.seed(1234)
lasso <- train(Y.house.price.of.unit.area~.,train, method = 'glmnet', tuneGrid = expand.grid(alpha=1, lambda =seq(0.0001,1, length = 5)), trControl = custom)
summary(lasso)

#Plot results
plot(lasso)
lasso
plot(lasso$finalModel, xvar = "lambda", label = T)
plot(lasso$finalModel, xvar = "dev", label = T)

#Let us look at variable importance plot 
plot(varImp(lasso, scale = F))
plot(varImp(lasso, scale = T))


#Elastic Net Regression
set.seed(1234)
# Here we have to create a sequence for alpha.
en <- train(Y.house.price.of.unit.area~.,train, method = 'glmnet', tuneGrid = expand.grid(alpha=seq(0,1, length=10), lambda =seq(0.0001,1, length = 5)), trControl = custom)
summary(en)

#Plot results
plot(en)
en
plot(en$finalModel, xvar = "lambda", label = T)
plot(en$finalModel, xvar = "dev", label = T)
plot(varImp(en, scale = F))
plot(varImp(en, scale = T)) 

#Compare Models
model_list <- list(LinearModel = lm, Ridge = ridge, Lasso = lasso, ElasticNet = en)
res <- resamples(model_list)
summary(res)

# So, elastic net model has the lowest mean RMSE and highest mean R-square.

bwplot(res)
xyplot(res, metric = 'RMSE')

# For dots that are above the dotted line, those models perform better for Ridge model and for those dots that are below the dotted line, those models perform better for linear model.

#Best Model
en$bestTune
# Since final value of alpha is 0.11 it is closer to zero, and hence more of a ridge model.
best <- en$finalModel
coef(best, s = en$bestTune$lambda)

#Save Final Model for Later Use
saveRDS(en, "final_model.RDS")
fm <- readRDS("final_model.RDS")
print(fm)

#Prediction
p1 <- predict(fm, train)
sqrt(mean((train$Y.house.price.of.unit.area - p1)^2)) 

p2 <- predict(fm, test)
sqrt(mean((test$Y.house.price.of.unit.area - p2)^2))
