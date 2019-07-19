# Main objective of Kohonen networks or Self Organizing Maps is to convert high dimensional maps into low dimensional maps.
# closer nodes are similar and closer to one another.

#Need the following library
library(kohonen)


# Import the bank failure dataset
data <- read.csv("~/LearningR/BankFailures.csv")

# Remove all rows with mising observations
data <- data[complete.cases(data), ] 
data$failure <- as.factor(data$failure)
str(data)
summary(data)

#We do the zscore standardization of the data by subtractign from mean and dividing by standard deviation with the exception of the dependent or response variable, so use -1.
X<- scale(data[, -1])
summary(X)

#SOM
set.seed(222) 
g <- somgrid(xdim =4, ydim = 4, topo = "rectangular")
map <- som(X, grid = g, alpha = c(0, 0.5), radius = 1 )
plot (map, type = 'changes')
plot(map)
plot (map, type = 'codes', palette.name = rainbow, main = "4 by 4 mapping of Banking Failure data")
plot(map, type = 'count')
plot(map, type = 'mapping')
plot(map, type = 'dist.neighbours')  #Dark color means neighbours are at closer distance.



#Supervised Self-Organizing Maps to make classification and predictions

# Splitting data into training and test.
set.seed(123)
ind <- sample(2, nrow(data), replace = T, prob = c(0.7, 0.3))
train <- data[ind == 1, ] # Nothing after comma means we take all columns
test <- data[ind == 2, ]

#Standardization of data
trainX<- scale(train[, -1]) # We do not standardize the first column, our response variable. 
trainX
# We normalize test data using mean and standard deviation of train dataset that is stored in attr.
testX<- scale(test[, -1], center = attr(trainX, "scaled:center"), scale =  attr(trainX, "scaled:scale"))
# We store the dependentvariable as a facror variable.
trainY <- factor(train[, 1])
Y <- factor(test[, 1])
test[,1] <- 0  #Make all values for dependent variable be zero.
testXY <- list(independent = testX, dependent = test[,1])
testXY

#Classification and Prediction of Model
set.seed(222)
# With 10 independent variables or features and our dependent variable we use 11 by 11 in the somgrid.
map1 <- xyf(trainX, classvec2classmat(factor(trainY)), grid = somgrid(11,11, "hexagonal"), rlen = 100)
plot(map1, type = 'changes')
plot(map1)
plot(map1, type = 'count')


#Classification and Prediction of Model
set.seed(222)
map1 <- xyf(trainX, classvec2classmat(factor(trainY)), grid = somgrid(5,5, "hexagonal"), rlen = 100)
plot(map1, type = 'changes')
plot(map1)
plot(map1, type = 'count')

#Prediction
pred <- predict(map1) #Prediction based on training data.
#However to access how good the model is we need to predictions based on the test data.
pred <- predict(map1, newdata = testXY)

#Confusion matrix
table(Predicted = pred$predictions[[2]], Actual = Y) 
# Prediction accuracy is 78%. 

#Cluster Boundaries
par(mfrow = c(1,2))
plot(map1, type = 'codes', main = c("Codes X", "Codes Y"))
map1.hc <- cutree(hclust(dist(map1$codes[[2]])), 2)
add.cluster.boundaries(map1, map1.hc)
par(mfrow = c(1,1))
