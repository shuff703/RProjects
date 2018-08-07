# Read the data and remove unnecessary columns
getwd()
setwd("Documents/R")
dungarees<-read.csv("CSVDatasets/dungaree.csv")
head(dungarees)
rownames(dungarees)<-dungarees[,"STOREID"]
#dungarees_use <- dungarees[,-c(2, 3, 4, 5, 6)]
head(dungarees_use)
summary(dungarees_use)

#There are still NA's.  Let's remove them.  Five rows will be deleted.
#dungarees_nm <- na.omit(dungarees_use)
dim(dungarees)

#pairwise distance

dungarees_dist <- dist(dungarees)
dungarees_dist <- dist(dungarees, method="manhattan") # Euclidean is preferred
dungarees_dist <- dist(dungarees, method="minkowski",p=2) # same as Euclidean

#scaling inputs and distances
dungarees_scaled <- scale(dungarees)
dungarees_scaled_dist<-dist(dungarees_scaled)

#Hierarchical clustering with default method = "complete"
complete.out = hclust(dungarees_dist)
single.out = hclust(dungarees_dist, method="single")
average.out = hclust(dungarees_dist, method="average")
ward.out = hclust(dungarees_dist, method="ward.D")

complete2.out = hclust(dungarees_scaled_dist)
single2.out = hclust(dungarees_scaled_dist, method="single")
average2.out = hclust(dungarees_scaled_dist, method="average")
ward2.out = hclust(dungarees_scaled_dist, method="ward.D")


plot(single.out, main='Dendrogram using single linkage', cex=0.7, hang=-1)
plot(single2.out, main='Dendrogram using single linkage', cex=0.7, hang=-1)
plot(average.out, main='Dendrogram using average linkage', cex=0.7, hang=-1)
plot(average2.out, main='Dendrogram using average linkage', cex=0.7, hang=-1)
plot(complete.out, main='Dendrogram using complete linkage', cex=0.7)
plot(complete2.out, main='Dendrogram using complete linkage', cex=0.7)
plot(ward.out, main='Dendrogram using Ward\'s method', cex=0.7)
plot(ward2.out, main='Dendrogram using Ward\'s method', cex=0.7)

#Playing with dendrogram objects
dndr_ward = as.dendrogram(ward2.out)
plot(dndr_ward, type="triangle")
plot(cut(dndr_ward, h = 7)$upper, main = "Upper tree of cut at h=7")
plot(cut(dndr_ward, h = 7)$lower[[4]], main = "Fourth branch of lower tree with cut at h=7") 


#Cut the dendrogram tree to create three clusters.
single_cl3.out<-cutree(single2.out,3)
table(single_cl3.out)
complete_cl3.out<-cutree(complete2.out,3)
table(complete_cl3.out)
ward_cl3.out<-cutree(ward2.out,3)
table(ward_cl3.out)

#colorful dendrogram
old.par<-par() # Store parameter values so we can later restore them.
source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")
par(bg = "gray15") 
cols=c("red", "blue", "yellow")
A2Rplot(ward2.out, k = 3, boxes = FALSE, col.up = "gray75", col.down = cols) 
par(old.par) # Restore the stored parameter values.


# members in each cluster for "hierarchical" clustering
counts = sapply(2:10,function(ncl)table(cutree(ward2.out, ncl)))
names(counts) = 2:10
counts

cl8.out=cutree(ward2.out,8)
table(cl8.out)
sapply(unique(cl8.out),function(g)rownames(dungarees_nm)[cl8.out == g])

# k-means algorithm
set.seed(1)
km.out = kmeans(dungarees, 5, nstart = 25)
table(km.out$cluster)

#Change the random seed and see whether the results are similar
set.seed(6201)
km.out = kmeans(cars_nm, 5, nstart = 25)
table(km.out$cluster)

# You may check the cluster membership and size of each cluster directly as follows
km.out
km.out$cluster
km.out$size

# The following shows that kmeans clustering is not "hierarchical" at all.
counts = sapply(2:10,function(ncl)table(kmeans(dungarees, ncl,nstart=25)$cluster))
names(counts) = 2:10
counts

#clusplot
par(mfrow=c(1,2))
require("cluster")
km_cl3.out = kmeans(dungarees_scaled, 3, nstart = 25) # 3 clusters
clusplot(dungarees_scaled_dist, diss = TRUE, km_cl3.out$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)
clusplot(dungarees_scaled_dist, diss = TRUE, complete_cl3.out, color = TRUE, shade = TRUE, labels = 2, lines = 0)

# Plots
km_cl3.out = kmeans(dungarees, 3, nstart = 1)
plot(dungarees_nm,col=(km_cl3.out$cluster+1))

# different parameter values
km.out = kmeans(dungarees_nm, 5, nstart = 1)
km_scaled.out<-kmeans(dungarees_scaled, 5, nstart = 1)
km_n25.out<-kmeans(dungarees_nm, 5, nstart = 1)
km_scaled_n25.out<-kmeans(dungarees_scaled, 5, nstart = 25)

par(mfrow=c(2,2))
plot(cars_dist,col=(km.out$cluster+1), main = "Cars")
plot(cars_scaled_dist,col=(km_scaled.out$cluster+1), main = "Cars scaled")
plot(cars_dist,col=(km_n25.out$cluster+1), main = "Cars nstart=25")
plot(cars_scaled_dist,col=(km_scaled_n25.out$cluster+1), main = "Cars scaled & nstart=25")
par(mfrow=c(1,1))

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

wssplot (cars_scaled)

#Determining number of clusters
if(!require("NbClust")) install.packages("NbClust")
library(NbClust)
nbcl.out = NbClust(cars_scaled, distance = "euclidean", 
                  min.nc = 2, max.nc = 10, method = "ward.D2", index = "all")

nbcl.out$Best.nc
par(mfrow=c(1,1))

#Silhouette
km_sil<-silhouette(km_scaled_n25.out$cluster, cars_scaled_dist)
plot(km_sil, main = "Silhouette plot for k-means result")
summary(km_sil)
summary(km_sil)$avg.width

ward_sil<-silhouette(cutree(ward2.out,5), cars_scaled_dist)
plot(ward_sil, main = "Silhouette plot for Ward's method result")
complete_sil<-silhouette(cutree(complete2.out,5), cars_scaled_dist)
plot(complete_sil, main = "Silhouette plot for complete linkage result")
single_sil<-silhouette(cutree(single2.out,5), cars_scaled_dist)
plot(single_sil, main = "Silhouette plot for single linkage result")


# comparison of cluster membership
kmeans_cl5<-km_scaled_n25.out$cluster
complete_cl5<-cutree(complete2.out,5)
ward_cl5<-cutree(ward2.out,5)
table (kmeans_cl5, complete_cl5)
table (kmeans_cl5, ward_cl5)
table (ward_cl5, complete_cl5)


