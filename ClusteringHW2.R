x1<-c(5, 4, 8, 4, 2, 9, 2, 9, 1, 5)
x2<-c(3, 9, 4, 7, 6, 2, 1, 7, 4, 5)

example2d<-data.frame(x1,x2)
# plot ranges and labels are specified.
plot(example2d, xlim=c(0, 10), ylim=c(0, 10))
text(x1,x2, labels=rownames(example2d),pos=4)

d <- dist(example2d, method="euclidean")
hclust.out <- hclust(d)
plot(hclust.out)
# cut the dendogram to make 3 clusters
cl3<-cutree(hclust.out, k=3)
table(cl3)

if(!require("NbClust")) install.packages("NbClust")
library(NbClust)
example.nb = NbClust(example2d, distance = "euclidean", min.nc = 2, max.nc = 7, 
                     method = "complete", index = "all")

# Change the layout of plots back to single figure.
par(mfrow=c(1,1))

if(!require("cluster")) install.packages("cluster")
library(cluster)
set.seed(3) 
#Changing the seed alters the random placing of the centroids...
#thus changes the clusters for the final output (ex. set.seed(5))
km.out<-kmeans(example2d,2)
km.out$cluster
km.out$size
attributes(km.out)

clusplot(dist(example2d),diss=TRUE,labels=2,color=TRUE,km.out$cluster)

sil<-silhouette(km.out$cluster, dist(example2d))
plot(sil, main = "Silhouette plot for k-means result")
summary(sil)
summary(sil)$avg.width

