# We are going to use this boosting algorithm to classify and predict the occurence of banking crisis in a nation. such an occurence has huge economic and social costs.


library(xgboost)
library(magrittr)
library(dplyr)
library(Matrix)


#Reading data
data <- read.csv("~/LearningR/KNN.csv", header = TRUE)
str(data)
View(data)

# XGboost requires numeric matrix so we will keep the banking crisis variable as integer.


#Partitioning the data with 80% training data and 20% test.
set.seed(1234)
ind <- sample(2, nrow(data), replace = T, prob = c(0.8, 0.2))   
train <- data[ind == 1,]
test <- data[ind == 2,]

#Create Matrix - One Hot Encoding for Factor Variables
# This will convert factor variables into dummies and keep the numeric variables unchanged.
trainm <- sparse.model.matrix(BankingCrisisdummy~. -1, data = train) # First column is banking crisis variable so we use -1.
head(trainm)

#We specify our response variable here.
train_label <- train[,"BankingCrisisdummy"]
# Now we will create  a train matrix to convert the data in the necessary format needed to run this algorithm.
train_matrix <- xgb.DMatrix(data = as.matrix(trainm), label=train_label)


# Do the same for test data.
testm <- sparse.model.matrix(BankingCrisisdummy~. -1, data = test)
head(testm)
test_label <- test[,"BankingCrisisdummy"]
test_matrix <- xgb.DMatrix(data = as.matrix(testm), label = test_label)

#Parameters

nc <- length(unique(train_label)) # Number of classes
nc
xgb_params <-list("objective"= "multi:softprob", "eval_metric"= "mlogloss", "num_class" = nc)
watchlist <- list(test = test_matrix)


#eXtreme Gradient Boosting Model
bst_model <- xgb.train(params = xgb_params, data= train_matrix, nrounds = 100, watchlist = watchlist)


#Training and test error plot
bst_model

# We will first store errors in e.
e <- data.frame(bst_model$evaluation_log)
plot(e$iter,e$train_mlogloss, col = 'blue')
# Now we will plot the test error
lines(e$iter,e$test_mlogloss, col = 'red')

# Mimimum value of test error
min(e$test_mlogloss)
e[e$test_mlogloss == 0.011465,]

# Now we will look at more features in the model to optimize it.Low values of eta ensures model is robust to overfitting.
bst_model <- xgb.train(params = xgb_params, data= train_matrix, nrounds = 99, watchlist = watchlist, eta = 0.05)

# So, as we see above iteration 100 gives the minimum value of the error.

#Feature Importance information

imp <- xgb.importance(colnames(train_matrix), model = bst_model)
print(imp)
xgb.plot.importance(imp)

# clearly we see Population is the most important variable.

#Prediction and confusion matrix using test data

p <- predict(bst_model, newdata = test_matrix)
head(p)

#We will store the probabilitiesin a matrix format.
pred <- matrix(p, nrow = nc, ncol = length(p)/nc) %>%
  t()%>% data.frame()%>%mutate(label = test_label, max_prob=max.col(., "last")-1)
head(pred)

#Next we create the confusion matrix
table(Prediction = pred$max_prob, Actual= pred$label)

#More XGBoost Parameters. Larger values of gamma gives us a more conservative algorithm.

bst_model <- xgb.train(params = xgb_params, data= train_matrix, nrounds = 100, watchlist = watchlist, eta = 0.05, max.depth =6, gamma = 0)

# More modifications. Lower values of sub-sample helps to reduce overfitting.
bst_model <- xgb.train(params = xgb_params, data= train_matrix, nrounds = 100, watchlist = watchlist, eta = 0.05, max.depth =3, gamma = 0, subsample = 1, colsample_bytree = 1, missing = NA, seed = 333)
e <- data.frame(bst_model$evaluation_log)
plot(e$iter,e$train_mlogloss, col = 'blue')
lines(e$iter,e$test_mlogloss, col = 'red')

min(e$test_mlogloss)
e[e$test_mlogloss == 0.053,]

#Prediction and confusion matrix again.
p <- predict(bst_model, newdata = test_matrix)
head(p)
pred <- matrix(p, nrow = nc, ncol = length(p)/nc) %>%
  t()%>% data.frame()%>%mutate(label = test_label, max_prob=max.col(., "last")-1)
head(pred)
table(Prediction = pred$max_prob, Actual= pred$label)
