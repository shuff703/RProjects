getwd()
setwd('/users/Scott/Documents/R')

library(data.table)

data <- read.csv("CSVDatasets/hospital_ortho.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?"))

nc_data <- data[(data$state == "NC") | (data$state == "SC") | (data$state == "VA") | (data$state == "GA") | (data$state == "TN"),]

library(plyr)
head(nc_data)

#data cleaning
typeof(nc_data)
nc_data <- nc_data[,!(names(nc_data) %in% c('zip', 'hid', 'rehab', 'trauma', 'hip', 'knee', 'city', 'state'))]
print(names(nc_data))

nc_data

nc_data <- scale(nc_data[-1])

withinssplot <- function(data, nc=15, seed=69){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}
withinssplot(nc_data) 

k_means <- kmeans(nc_data, 3)

if(!require('cluster')) install.packages('cluster')
library(cluster)

if(!require('clustertend')) install.packages('clustertend')
library(clustertend)

hopkins(nc_data, n = nrow(nc_data) - 1)

clusplot(nc_data, k_means$cluster, main = 'CLUSTER BOI', color = TRUE, shade = TRUE)

k_means <- kmeans(nc_data, 10)
clusplot(nc_data, k_means$cluster, main = 'CLUSTER BOI', color = TRUE, shade = TRUE)

k_means <- kmeans(nc_data, 4)
clusplot(nc_data, k_means$cluster, main = 'CLUSTER BOI', color = TRUE, shade = TRUE)

k_means <- kmeans(nc_data, 6)
clusplot(nc_data, k_means$cluster, main = 'CLUSTER BOI', color = TRUE, shade = TRUE)

k_means <- kmeans(nc_data, 7)
clusplot(nc_data, k_means$cluster, main = '2D Cluster Plot', color = TRUE, shade = TRUE)

k_means$size

#Hierarchical Clustering
m <- dist(nc_data, method = 'euclidian')

single_link <- hclust(m, method='single')
complete_link <- hclust(m, method='complete')
avg_link <- hclust(m, method='average')

plot(single_link)
plot(complete_link)
plot(avg_link)

groups <- cutree(complete_link, k=5)

plot(complete_link)
rect.hclust(complete_link, k=5, border='red')

#DBSCAN
if(!require('dbscan')) install.packages('dbscan')
library(dbscan)

kNNdistplot(nc_data, k =11)
abline(h=1, col="red")

db <- dbscan(nc_data, eps=4, minPts=11)
db
db$cluster

clusplot(nc_data, db$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

nc_data$kmeans <- k_means$cluster
nc_data$hclust <- groups # these groups are created in hierarchical clustering
nc_data$db <- db$cluster
aggregate(nc_data[,c("sales12","rbeds","hip12","knee12","femur12")], list(nc_data$kmeans), mean)
aggregate(nc_data[,c("sales12","rbeds","hip12","knee12","femur12")], list(nc_data$hclust), mean)
aggregate(nc_data[,c("sales12","rbeds","hip12","knee12","femur12")], list(nc_data$db), mean)
