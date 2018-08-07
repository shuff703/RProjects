setwd("Documents/R")
dungarees<-read.csv("CSVDatasets/dungaree.csv")

rownames(dungarees)<-dungarees[,"STOREID"]
dungarees_use=dungarees[,-c(1,6)]

head(dungarees_use)
dungarees_nm<-na.omit(dungarees_use)

#Euclidian Distance
dungarees_dist <- dist(dungarees_nm)

#Heirarchial Clustering
complete.out<-hclust(dungarees_dist)
par(mfrow=c(1,1))
plot(complete.out, main="Complete Linkage Dendrogram", cex=0.5, hang=-1)
clust_dend<-cutree(complete.out, 3)
dendr_complete<-as.dendrogram(complete.out)
plot(dendr_complete, type="triangle")

#K-Means
set.seed(1)
km.out<-kmeans(dungarees_nm, 6, nstart=25)
table(km.out$cluster)
require("cluster")
clusplot(dungarees_dist, diss=TRUE,km.out$cluster,color=TRUE,shade=TRUE,labels=2,lines=0)
plot(dungarees_nm,col=(km.out$cluster+1))

#Silhouette
sil<-silhouette(km.out$cluster, dungarees_dist) #6 clusters .36 AVG
silh<-silhouette(clust_dend, dungarees_dist) #3 clusters .35 AVG
plot(silh, main="Heirarchial Silhouette")
plot(sil, main="Silhouette Plot")
summary(sil)
summary(sil)$avg.width
# Function definition for within sum of squares plot
wssplot <- function(data, nc=15, seed=1234)
{
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i, nstart=25)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}

wssplot(dungarees_nm)

#For the K-Means algorithm, using the elbow method and analysis of silhouette score, I found
#that K=6 provides the best result with .36 AVG silhouette width.  7 clusters provides the 
#same average but more extreme instances among the clusters.  For the complete linkage heirarchial
#clustering, it was found that dividing into 3 clusters provides the best silhouette score of
#.35.  Both the models had at least 1 very high scoring cluster (K-means had 2), while the others
#showed rather low scores.  Also, the clusters with the highest silhouette scores tended to be the 
#smallest.
