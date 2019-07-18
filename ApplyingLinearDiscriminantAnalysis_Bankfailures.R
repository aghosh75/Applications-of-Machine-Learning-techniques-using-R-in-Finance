# Linear Discriminant Analysis helps to obtain linear combination of original variables that provide the best possible seperation between groups.
# Here we will use this to establish a relationship between bank failures in US Metropolitan Statistical Areas (MSAs) and a set of 10 indepdendent variables or features.

# Import the bank failure dataset
data <- read.csv("~/LearningR/BankFailures.csv")

# Remove all rows with mising observations
data <- data[complete.cases(data), ] 
str(data)
data$failure <- as.factor(data$failure)
summary(data)

library(psych)

#Scatter plot among the indepdendent variables
pairs.panels(data[2:11], gap = 0, bg = c("red", "green")[data$failure], pch = 21)

#Objective: We are looking for the linear combination of the 10 variables that gives us the best seperation among the two groups (MSAs with and without bank failures). 

#Data partition
set.seed(555)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.6, 0.4))
training <- data[ind==1,]
testing <- data[ind==2,]

#Linear Discriminant Analysis. We will use the library called MASS. 
install.packages("MASS")
library(MASS)
# We apply LDA on the standardized data.
#First discriminant is a linear combination of the 10 variables. 
linear <- lda(failure~., training, center =TRUE, scale. = TRUE)
linear

# To get attributes of LDA we use
attributes(linear)
linear$prior
linear$counts   #Gives us the number of datapoint for each bank failure observations. 
linear$scaling  #Gives values of the coefficients.

#Stacked Histograms of Discriminant Function values
p <- predict(linear, training)
p
ldahist(data <-p$x[,1], g=training$failure)


#Bi-Plot 
# We would need the following packages.
library(devtools)
install_github("fawda123/ggord")
library(ggord)

ggord(linear, training$failure)
ggord(linear, training$Species, ylim = c(-10,10)) 


#Partition Plots  6 plots for every combination of the two variables. 
library(klaR)
partimat(failure~.,data = training, center =TRUE, scale. = TRUE, method = "lda")

#Quadratic discriminant analysis plot
partimat(failure~.,data = training, center =TRUE, scale. = TRUE, method = "qda") 

#Confusion matrix and accuracy - training data
p1 <- predict(linear, training)$class 
tab <- table(Predicted = p1, Actual = training$failure)
tab

#Accurancy of the model
sum(diag(tab))/sum(tab)

#Misclassification error 
1 - sum(diag(tab))/sum(tab)


#Confusion matrix, accuracy and misclassification error - testing data
p2 <- predict(linear, testing)$class 
tab1 <- table(Predicted = p2, Actual = testing$failure)
tab1
sum(diag(tab1))/sum(tab1)
1 - sum(diag(tab1))/sum(tab1)
