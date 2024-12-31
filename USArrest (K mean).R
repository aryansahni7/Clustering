setwd("D:/Term 2/Machine Learning/Working Directory")
USArrests <- read.csv("USArrest.csv", row.names = 1) #row.names to change the row name from S.No to a column, making the column rows as index (making US states as index). It only works if the column is a primary key,i.e. it has unique values.
summary(USArrests)
View(USArrests)

library(cluster) #for clustering
library(factoextra) #for graph

#scaling (Standardization, also known as z-score normalization or feature scaling)
#standardizes each variable by subtracting its mean and dividing by its standard deviation, resulting in a mean of zero and a standard deviation of one for each variable
#Done when variables are measured on different scales and you want to give them equal weight in your analysis
USArrests1 <- scale(USArrests)
View(USArrests1)

km <- kmeans(USArrests1,2) #K mean clustering with number of clusters 2
fviz_cluster(km, data=USArrests1) #Visualize clusters

str(km) #structure

Accuracy = km$betweenss/km$totss #computes the proportion of total variance that is explained between clusters relative to the total variance in the dataset.
Accuracy
#A higher value indicates that the clusters are well-separated and explain a large portion of the total variance in the dataset

#Elbow Method → to determine the optimal number of clusters
number <- 1:10 #vector
wss <- 1:10 #vector to store the within-cluster sum of squares 

for(i in 1:10)
{
  wss[i] <- kmeans(USArrests1,i)$tot.withinss #Accesses the total within-cluster sum of squares from the k-means clustering result.
}
plot(number,wss,type="b",pch=19)

km1 <- kmeans(USArrests1,4)
fviz_cluster(km1, data = USArrests1)

str(km1)

Accuracy1 = km1$betweenss/km1$totss
Accuracy1

#Silhoutte
#plot where the x-axis represents the number of clusters, and the y-axis represents the average silhouette width for each clustering solution. The silhouette width measures the quality of the clustering, with higher values indicating better-defined clusters. The plot helps identify the optimal number of clusters by highlighting peaks or plateau regions in the silhouette width curve.
fviz_nbclust(USArrests1, kmeans, method="silhouette")
#higher silhouette width indicating better-defined clusters
#silhouette score:
#+1 well clustered
#0 object lies on the boundary
#-1 object assigned to wrong cluster

# Make Cluster Column

USArrests$cluster <- km1$cluster
View(USArrests)


# Profiling of cluster →  Creating a story from clusters
cmeans <- aggregate(USArrests,by=list(USArrests$cluster),mean)
cmeans

#INTERPRETATION
#cluster1 → high crime, normal pop (worst)
#cluster2 → low crime, lowest pop (ideal)
#cluster3 → high crime, high pop (obvious)
#cluster4 → normal crime, high pop (mixed)