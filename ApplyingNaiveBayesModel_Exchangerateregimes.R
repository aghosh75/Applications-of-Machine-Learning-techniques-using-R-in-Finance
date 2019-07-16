# We would need the following libraries to run a Naive Bayes Model
library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)

#Reading data
data <- read.csv("~/LearningR/Data.csv", header = TRUE)

str(data)
xtabs(~Regime1+TradeOpenness_Ghosh, data = data)

#Convert exchange rate Regime into a factor variable.
data$Regime1 <- as.factor(data$Regime1)

#Convert character variable, Captal controls, into a numeric variable.
data$AREAERcapitalcontrolsindex <- as.numeric(as.character(data$AREAERcapitalcontrolsindex))


#Visualization
# We will first check if the features are highly correlated or not. 
pairs.panels(data[-1])   #Using -1 ensures we do not include the dependent variable or label, Regime1, from the pairwise plots. 
# As we see above high correlation is not a concern.


# Now we will create some box plots
# We first define our x and y axes.
data %>%
ggplot(aes(x=Regime1, y = TradeOpenness_Ghosh, fill = Regime1))+geom_boxplot()+ ggtitle("Box Plot")
  
data %>%
ggplot(aes(x=Regime1, y = ExportConcentration, fill = Regime1))+geom_boxplot()+ ggtitle("Box Plot")


# Next we will do some density plots
# alpha determines the extent of transparency in the data
data %>%
  ggplot(aes(x=ExportConcentration, fill = Regime1))+geom_density(alpha = 0.8, color= 'black')+ ggtitle("Density Plot")

data %>%
  ggplot(aes(x=TradeOpenness_Ghosh, fill = Regime1))+geom_density(alpha = 0.8, color= 'black')+ ggtitle("Density Plot")                                                                   


#Data Partition with 80% training data.

set.seed(1234)
ind <- sample(2, nrow(data), replace = T, prob = c(0.8, 0.2))
train <- data[ind==1,]
test <- data[ind==2,]

#Naive Bayes' model
# To imporve the accurancy of the model we can use kernel based density when numerical variables are not nomrally distributed.
model<- naive_bayes(Regime1~TradeOpenness_Ghosh+ExportConcentration + AREAERcapitalcontrolsindex + Landarea + Population + IQ, data = train, usekernel = T)
model
train %>% 
  filter(Regime1 == "0")%>% 
  summarise(mean(TradeOpenness_Ghosh), sd(TradeOpenness_Ghosh))
plot(model)  


#Predict
p <- predict(model, train, type = 'prob' )
# Looking at the probabilites and the actual data
head(cbind(p, train))

#Confusion Matrix - train and test data
# First we store predictions in p1 and then create table and confusion matrix
p1 <- predict(model, train)
(tab1 <- table(p1,train$Regime1))
1 - sum(diag(tab1))/sum(tab1)

p2 <- predict(model, test)
(tab2 <- table(p2,test$Regime1))
1 - sum(diag(tab2))/sum(tab2)


