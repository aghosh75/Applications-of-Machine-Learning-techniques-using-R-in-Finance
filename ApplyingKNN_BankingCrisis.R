# K-nearest neighbors algorithm is a non-parametric method used for classifcation and regression.

# Here will used this in the context of banking crisis is a nation. this is important becuase if nations go through banking crisis it inflicits signficant economic and social costs on citizens of a nation.
# For ilustrative purposes we will use three features -- population, GDP and the share of foreign banks to total banks in the nation. 
# We use Euclidean distance measures to calculate one obervation with its nearest neighbors.  
# We can also use the Manhattan method whehre we use the total of absolute values of the difference bewteen two points.  
# now we will apply this for classification and regression of banking crisis. 


#We need the following Libraries
library(caret)
library(pROC)
'citation("pROC")'
library(mlbench)
library(lattice)
library(ggplot2)

KNN <- read.csv("~/LearningR/KNN.csv", header = TRUE)
View(KNN)
attach(KNN)
str(KNN)

# This makes banking crisis a character variable
KNN$BankingCrisisdummy[KNN$BankingCrisisdummy == 0] <- 'No'
KNN$BankingCrisisdummy[KNN$BankingCrisisdummy == 1] <- 'Yes'

# Now we are going to chnage this to factor variable
KNN$BankingCrisisdummy <- factor(KNN$BankingCrisisdummy)
str(KNN)

#Now we will partition the data. 

set.seed(1234)
ind <- sample(2, nrow(KNN), replace = T, prob = c(0.7, 0.3))
training <- KNN[ind==1,]   #Nothing means all columns are included.
test <- KNN[ind==2,]


#Now we will develop the KNN model. We will use repeated cross-validation and 10 resampling iterations, and 3 complete set of folds to repeat cross-validation.
# Accuracy is used to select the optimal model.
trControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(222)
# Now we fit the model by using z-score standardization. We use Preprocess option from caret package
head(KNN)
fit <- train(BankingCrisisdummy~ ., data =  training, method = 'knn', tuneLength = 20,trControl = trControl, preProc = c("center","scale")) 


#Model performance
fit
plot(fit)    #Shows acuracy values corresponding to different values of k. 
varImp(fit)  #Gives variabe importance

#Predict the model using the test data and create the confusion matrix
pred <- predict(fit, newdata =test)
confusionMatrix(pred, test$BankingCrisisdummy)


#Another method
# Here ROC is used to select the optimal model.
trControl <- trainControl(method = "repeatedcv",number = 10, repeats = 3, classProbs = TRUE,summaryFunction = twoClassSummary)
set.seed(222)
fit <- train(BankingCrisisdummy~ ., data =  training, method = 'knn', tuneLength = 20,trControl = trControl, preProc = c("center","scale"),metric = "ROC", tuneGrid =expand.grid(k=1:60))

#Model performance
fit
plot(fit)    #Shows acuracy values corresponding to different values of k. 
varImp(fit)  #Gives variabe importance

#Predict the model using the test data and create the confusion matrix
pred <- predict(fit, newdata =test)
confusionMatrix(pred, test$BankingCrisisdummy)






