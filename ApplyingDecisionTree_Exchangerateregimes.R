library(readxl)
DecisionTree <- read_excel("~/LearningR/DecisionTree.xlsx")
View(DecisionTree)
attach(DecisionTree)
str(DecisionTree)
data<-DecisionTree  #Renaming the data
str(data)

# Creating Factor variables
data$Regime1F <- factor(data$Regime1)
data$Regime2F <- factor(data$Regime2)
data$Regime3F <- factor(data$Regime3)
data$AREAERcapitalcontrolsindex_m <- as.numeric(as.character(data$AREAERcapitalcontrolsindex))

#Partition data into Training and Validation datasets

set.seed(1234)
pd<-sample(2, nrow(data), replace = TRUE, prob = c(0.8, 0.2))
train <- data[pd==1,]       #for all columns
validate <- data[pd==2,]    #for all columns 

#Decision tree with party package
#Let us install party package first
install.packages("party")
install.packages("sandwich")
library(party)
library(sandwich)

# We will call the Decision Tree as tree
tree <-ctree(Regime1F~ TradeOpenness_Ghosh + ExportConcentration + AREAERcapitalcontrolsindex_m + Landarea + Population + IQ, data = train)
tree <-ctree(Regime1F~ TradeOpenness_Ghosh + ExportConcentration + AREAERcapitalcontrolsindex_m + Landarea + Population + IQ, data = train, controls =ctree_control(mincriterion = 0.9, minsplit = 200))
tree
# Plot the decision tree. The picture is upside down with the root at the top.
plot(tree)

#Predict on the validation dataset based on the tree.
predict(tree,validate,type = "prob")

# The model gives the prediction wheher a country belongs to fixed, intermediate of flexible exchnage rate regime. 
predict(tree,validate)


# In the next part we are goiing to use Decision Tree based on rpart package
#Decision tree with rpart
library(rpart)
library(rpart.plot)

#Since we already have  a decidion tree, tree, we will call the new tree, tree1.
tree1 <- rpart(Regime1F~ TradeOpenness_Ghosh + ExportConcentration + AREAERcapitalcontrolsindex_m + Landarea + Population + IQ, data = train)
rpart.plot(tree1)
rpart.plot(tree1, extra =4)

#Predictions using the validation dataset
predict(tree1, validate)

# Confusion matrix and Misclassification error for 'train' data

# So, first we will create a table using the table function
tab <- table(predict(tree), train$Regime1F)
print(tab)

# Now we get the misclassification error
1 - sum(diag(tab))/sum(tab)


#Misclassification error for 'validate' data
testPred <- predict(tree, newdata = validate)
tab3 <- table(testPred,validate$Regime1F)
print(tab3)

# Now we again get the misclassification error
1 - sum(diag(tab3))/sum(tab3)



