# We are going to apply cluster analysis here.
# For cluster anlysis we need quantitative data.
# The dataset has key economic and demographic indicators of nations averaged over a period of time.

library(readxl)
ClusterAnalysis <- read_excel("~/LearningR/ClusterAnalysis.xlsx")
View(ClusterAnalysis)
attach(ClusterAnalysis)
str(ClusterAnalysis)

#We convert countries into factor variables.
# All other variables are numeric variables
ClusterAnalysis$Countries <- as.factor(ClusterAnalysis$Countries)

#Scatter Plot. We do this for illustration using inflation and unemployment rates
plot(Inflation~Unemployment,ClusterAnalysis)

#Adding country labels to the scatter plot
plot(Inflation~Unemployment,ClusterAnalysis)
with(ClusterAnalysis,text(Inflation~Unemployment, labels = Countries)) 

#Adding country labels to the scatter plot and specifying position
plot(Inflation~Unemployment,ClusterAnalysis)
with(ClusterAnalysis,text(Inflation~Unemployment, labels = Countries, pos = 4)) 

#Adding country labels to the scatter plot and specifying position and size of text
plot(Inflation~Unemployment,ClusterAnalysis)
with(ClusterAnalysis,text(Inflation~Unemployment, labels = Countries, pos = 4, cex = 0.4)) 

#Normalization. 
#We do the z-score standardization here by subtracting from mean and divding by standard deviation.  
#Can be applied only to quantitative data.So, remove the first variable country name. 

z<- ClusterAnalysis[,-c(1,1)]  #We are not using the Countries column
m<-apply(z, 2, mean) # 2 denotes columns
s <- apply(z, 2, sd)
z <- scale(z, m, s)


#Calculate the Euclidean distance
distance <-dist(z)
print(distance, digits = 3)


#Cluster dendogram with complete linkage
#We will do hierachial clustering here.
#Initally each country is treated as a single cluster and then we find whoch countries are the closest distance.
#Finally, all the countries are formed into on elarge cluster.
hc.c <- hclust(distance)   #h denotes hierchial cluster
plot(hc.c)
plot(hc.c, labels =ClusterAnalysis$Countries)
plot(hc.c, hang = -1)

#Cluster dendogram with average linkage
hc.a <- hclust(distance, method = "average")
plot(hc.a, hang = -1)


#Cluster Membership
member.c <- cutree(hc.c, 3) # c denotes complete linkage method
member.a <- cutree(hc.a, 3) #a denotes average linkage method
table(member.c, member.a)

#Cluster means
aggregate(z, list(member.c), mean) #clustering in standardized form. Gives which variables are important for countries in a certain cluster.
aggregate(ClusterAnalysis[,-c(1,1)],list(member.c),mean)  #Doing clustering of the riginal variables.

#Silhouette Plot
library(cluster)
plot(silhouette(cutree(hc.c, 3), distance))

#Scree plot. Plots within cluster variability with number of clusters
# We get the within group sum of squares. 
wss<- (nrow(z)-1)*sum(apply(z,2, var))   
for (i in 2:36) wss[i] <- sum(kmeans(z,centers = i)$withinss)
plot(1:36, wss, type = "b", xlab = "Number of clusters", ylab = "Within group SS")


#K-Means Clustering
# Non-hierachial clustering

kc <- kmeans(z, 3)
kc
kc$cluster
kc$centers
plot(GDPpercapita~TradeOpenness,ClusterAnalysis, col = kc$cluster)
