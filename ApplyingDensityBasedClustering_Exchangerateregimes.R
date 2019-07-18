#Grop objects into cluster 
# We are able to create clusters with various shapes and sizes and not very sensitive to noise.
# so, here we will group nations exchange rate regimes by clusters. 


# Here we will apply a density based cluster aalysis to classify exchange rate regimes.

#Reading data
data <- read.csv("~/LearningR/Data.csv", header = TRUE)


#Convert exchange rate Regime into a factor variable.
data$Regime1 <- as.factor(data$Regime1)

#Convert character variable, Captal controls, into a numeric variable.
data$AREAERcapitalcontrolsindex <- as.numeric(as.character(data$AREAERcapitalcontrolsindex))
str(data)

# In clustering we make use of quantitative variables so we remove the first column.
new <- data[,-1] 
new 

# Installing the required packages
library(fpc)
library(dbscan)
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/factoextra")
library(factoextra)

#eps - radius of a circle from a point.
#Obtaining optimal eps value
kNNdistplot(new, k = 3)
abline (h=1000000, lty = 2)

#eps provides us the maximum distance. If eps value is too small partial clusters will be defined as noise. but if it is too big the denser clustersshould be merged together.

#Density-based clustering with fpc and dbscan.
# MinPts is the minimum number of poins within the distance eps. 
set.seed(123)
f <- fpc::dbscan(new, eps = 1000000, MinPts = 4) #This is Density-based clustering using dbs.
f

d<- dbscan::dbscan(new, 1000000, 4)
d 

# Cluster Visualization
fviz_cluster(f, new, geom = "point")
fviz_cluster(d, new, geom = "point")

# Sensitivity Analysis
f <- fpc::dbscan(new, eps = 1000000, MinPts = 3)
d<- dbscan::dbscan(new, 1000000, 3)
f
d
fviz_cluster(f, new, geom = "point")
fviz_cluster(d, new, geom = "point")

f <- fpc::dbscan(new, eps = 1000000, MinPts = 10)
d<- dbscan::dbscan(new, 1000000, 10)
f
d
fviz_cluster(f, new, geom = "point")
fviz_cluster(d, new, geom = "point")

#Using Minimum points =2 gives the minimum noise.
f <- fpc::dbscan(new, eps = 1000000, MinPts = 2)
d<- dbscan::dbscan(new, 1000000, 2)
f
d
fviz_cluster(f, new, geom = "point")
fviz_cluster(d, new, geom = "point")
