# We will use principal components anlysis to predict bank failures in a Metrpolitan Statistical Area (MSA). 
# PCA is relevant here as it helps to make the feature items independent and avoid multcollinearity porblem.

# Import the bank failure dataset
data <- read.csv("~/LearningR/BankFailures.csv")

# Remove all rows with mising observations
data <- data[complete.cases(data), ] 
str(data)
data$failure <- as.factor(data$failure)
summary(data)

#Partition data
set.seed(111)
ind <- sample(2, nrow(data),replace = TRUE, prob = c(0.8,0.2))
training <- data[ind ==1,]
test <- data[ind == 2,]

#Scatter plot and correlations
library(psych)
pairs.panels(training[, -1], gap =0, bg = c("red", "yellow")[training$failure],pch = 21)


#Principal Component Analysis is done only on the independent variables or features
# Before doing PCA we standardize all independent variables.
pc <- prcomp(training[,-1], center =TRUE, scale. = TRUE)
attributes(pc)

pc$center #Gives average of each feature
mean(training$tier1capital_wt)

pc$scale  #Gives standard deviation of each feature 
sd(training$tier1capital_wt)

#Now we will print the principal components. 
# This will give us the standard devaitions and loadings (or rotations) of the principal components. 
# Each PC is a normalized linear combination of original variables. They lie between -1 and 1.
print(pc)

#Now we get the importance of components
summary(pc)

#Orthogonality of Principal Components
pairs.panels(pc$x, gap=0, bg = c("red", "yellow")[training$failure],pch = 21)

#Neeed the following packages to do Bi-Plot
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
install.packages("ggplot2")
library(ggplot2)
library(plyr)
library(scales)
install_github("ggbiplot","vqv")

#Bi-plot
g<- ggbiplot(pc, obs.scale = 1, var.scale =1, groups = training$failure, ellipse = TRUE, circle = TRUE, ellipse.prob = 0.68)
g<- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
print(g)


#Prediction with Principal Components
#First we sue predict function to predict and store
trg <- predict(pc, training)
trg
# Now we convert this into a data frame but also the actual bank failure information from the training dataset. 
trg <- data.frame(trg, training[1])
tst <- predict(pc, test)
tst
tst <- data.frame(tst, test[1])


#Now we will use a multinomial Logistic Regression
library(nnet)
trg$failure <- relevel(trg$failure, ref ="0")
mymodel <- multinom(failure~PC1+PC2+PC3+PC4, data=trg)
summary(mymodel)

#Confusion matrix and Miscalculation Error - training
p <- predict(mymodel, trg)
tab <- table(p, trg$failure)
tab
1-sum(diag(tab))/sum(tab)  #Miscalculation Error

#Confusion matrix and Miscalculation Error - testing
p1 <- predict(mymodel, tst)
tab1 <- table(p1, test$failure)
tab1
1-sum(diag(tab1))/sum(tab1)
