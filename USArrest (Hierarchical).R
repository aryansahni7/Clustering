setwd("D:/Term 2/Machine Learning/Working Directory")

library(cluster)
library(factoextra)

USArrests <- read.csv("USArrest.csv", row.names = 1)
summary(USArrests)
nrow(USArrests)
View(USArrests)

# Distance Matrix

#The dissimilarity matrix is a symmetric matrix where each element represents the distance between two observations in the dataset
dmatrix <- daisy(USArrests, metric = c("euclidean"),stand = TRUE) #STAND =TRUE FOR SCALING computing the dissimilarity matrix.(mean is 0)
View(dmatrix)
class(dmatrix)

#to view/export the distance matrix
distance <- dist(dmatrix)
d <- as.matrix(distance)
View(d)

#heatmap
fviz_dist(distance, lab_size = 8) #heatmap visualization of the pairwise distances

#step 2
hc <- hclust(dmatrix,method = "average") #hierarchical clustering on the dissimilarity matrix
#The "average linkage" method calculates the distance between two clusters as the average of the pairwise distances between all pairs of observations in the two clusters.
plot(as.dendrogram(hc)) #converts hierarchical clustering into a dendrogram

rect.hclust(hc,5) #used to add rectangles to a dendrogram plot to highlight specific clusters (5 in this case)

# Example of calculating cluster statistics
cluster_labels <- cutree(hc, k = 5) #number of clusters is 5
cluster_labels
table(cluster_labels) #value count of each cluster
cluster_means <- aggregate(USArrests, by = list(cluster_labels), FUN = mean) #mean of each variable for each cluster 
cluster_means

# Visualizing cluster characteristics
boxplot(USArrests$Murder ~ cluster_labels, main = "Murder Rate by Cluster", xlab = "Cluster", ylab = "Murder Rate") #distribution of murder rates across different clusters

# Evaluating cluster validity
silhouette <- silhouette(cluster_labels, dist = dmatrix)
plot(silhouette)
#higher silhouette width indicating better-defined clusters
#silhouette score:
#+1 well clustered
#0 object lies on the boundary
#-1 object assigned to wrong cluster