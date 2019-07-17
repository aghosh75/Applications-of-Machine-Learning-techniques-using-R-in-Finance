#We will apply a Random Forest machine learning techique to cross-country exchange rate regime classification. 
# Instead of one decision tree, here we can aggregate trees. It can be used for classification or regression.
# It also avoids overfitting and helps feature selection based on importance.

#Reading data
data <- read.csv("~/LearningR/Data.csv", header = TRUE)
str(data)
data$Regime1 <- as.factor(data$Regime1)
table(data$Regime1)


#Data Partitioning into 70% training and 30% testing. 
set.seed(123)
ind <- sample(2,nrow(data), replace = TRUE, prob = c(0.7, 0.3))
train <- data[ind==1,]  #Blank means all the columns
test <- data[ind==2,]

#Random Forest Model
library(randomForest)
set.seed(222)
rf <- randomForest(Regime1~., data = train) #Original random forest command
print(rf)
attributes(rf)
rf$confusion
rf$call
rf$votes

library(caret)
library(lattice)
library(ggplot2)


#Prediction and condusion matrix - train data
p1<- predict(rf, train)
head(p1)

head(train$Regime1)

confusionMatrix(p1, train$Regime1)

#Prediction and condusion matrix - test data
p2<- predict(rf, test)
confusionMatrix(p2, test$Regime1)

#Error Rate for Random Forest
plot(rf)

#Tune Random Forest Model

#Tune mtry
# We remove the second column in the training datste as it has the response variable.  
t<-tuneRF(train[, -2], train[, 2], stepFactor =0.5, plot = TRUE, ntreeTry = 300, trace = TRUE, improve = 0.05)

# Now we will chane the original Random Forest model
rf <- randomForest(Regime1~., data = train, ntree = 300, mtry =2, importance = TRUE, proximity = TRUE )
print(rf)
attributes(rf)
rf$confusion

p1<- predict(rf, train)
head(p1)
head(train$Regime1)
confusionMatrix(p1, train$Regime1)

p2<- predict(rf, test)
confusionMatrix(p2, test$Regime1)



#Number of Nodes for Trees. Gives us the distribution of number of nodes in the 300 trees.
hist(treesize(rf),main = "No. of Nodes for the Trees", col = "green")

#Variable Importance

varImpPlot(rf)
varImpPlot(rf, sort = T, n.var = 5, main = "Top 5 variable importance")
importance(rf)
varUsed(rf)


#Partial Dependence Plot, say we do it for the variable Population
partialPlot(rf, train, Population, "0")

#Extract Single Tree from the Forest. status -1 means the node is a terminal node. 
getTree(rf, 1, labelVar = TRUE)

#Multi-dimensional Scaling Plot of Proximity Matrix
MDSplot(rf, train$Regime1)



