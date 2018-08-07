dir <- '/Users/Scott/Documents/R'



getwd()
setwd(dir)
#Q1
college_data = read.csv('CSVDatasets/college_data.csv')

if(!require('dplyr')) install.packages('dplyr')
library(dplyr)

#Get Standard Deviation for Feature Selection
sapply(dplyr::select_if(college_data, is.numeric), sd, na.rm = TRUE)

sapply(dplyr::select_if(college_data, is.numeric), min, na.rm = TRUE)

sapply(dplyr::select_if(college_data, is.numeric), max, na.rm = TRUE)

drop_cols <- c('UNITID', 'INSTNM', 'CITY', 'STABBR', 'ZIP', 'INSTURL', 'NPCURL', 'HCM2', 'main',
           'HBCU', 'PBI', 'TRIBAL', 'HSI', 'NANTI', 'MENONLY', 'WOMENONLY', 'CIP05CERT4',
           'CIP16CERT4', 'CIP25CERT4', 'CIP30CERT4', 'CIP41CERT4', 'DISTANCEONLY', 'CURROPER')

college_data <- college_data[,!(names(college_data) %in% drop_cols)]
colnames(college_data)
ncol(college_data)

clean_data <- college_data[complete.cases(college_data),]

#PCA to 50 dimensions covers > 75% of variance in dataset
pca <- prcomp(clean_data, center = TRUE, scale. = TRUE, rank. = 50)
plot(pca, type = "l")
summary(pca)

pca_data <- as.data.frame(pca[5])

withinssplot <- function (data=pca_data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}
withinssplot(pca_data, nc=10) 

k_means <- kmeans(pca_data, 3)

if(!require('clustertend')) install.packages('clustertend')
library(clustertend)
hopkins(pca_data, n = nrow(pca_data) - 1)

if(!require('cluster')) install.packages('cluster')
library(cluster)
clusplot(pca_data, k_means$cluster, main='Clusters', color=TRUE, shade=TRUE, labels=2, lines=0)

pca_data$kmeans <- k_means$cluster

plot(silhouette(k_means$cluster, dist))

dist <- dist(pca_data, method = "euclidean")
single_link <- hclust(dist, method="single")

labels <- cbind(pca_data, clusterNum = k_means$cluster)
head(labels)
labels$clusterNum

sort(table(labels$clusterNum),decreasing=TRUE)[1:3]

complete_link <- hclust(dist, method="complete")
plot(complete_link)

average_link <- hclust(dist, method="average")
plot(average_link)

ward <- hclust(dist, method="ward.D2")
plot(ward)

groups <- cutree(ward, k=3)
plot(ward)
rect.hclust(ward, k=3, border="red")
if(!require('dbscan')) install.packages('dbscan')
library(dbscan)

kNNdistplot(pca_data, k =3)
abline(h=25, col="red")

db_scan <- dbscan(pca_data, eps=25, minPts=51)
db_scan
db_scan$cluster

clusplot(pca_data, db_scan$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

#INDEXES
#Chapel Hill 236
#Charlotte 237
#NC State 241
#ASU Tempe 15
#Arizona Tuscon 17
#GSU 115
#Georgia Athens 116
#GIT 110
#Emory 106
k_means$cluster[237]
k_means$cluster[115]
k_means$cluster[17]
groups[237]
groups[115]
groups[17]
db_scan$cluster[237]
db_scan$cluster[115]
db_scan$cluster[17]

k_means$cluster[236]
k_means$cluster[241]
k_means$cluster[15]
groups[236]
groups[241]
groups[15]
db_scan$cluster[236]
db_scan$cluster[241]
db_scan$cluster[15]

k_means$cluster[116]
k_means$cluster[115]
groups[116]
groups[115]
db_scan$cluster[116]
db_scan$cluster[115]

k_means$cluster[110]
k_means$cluster[106]
groups[110]
groups[106]
db_scan$cluster[110]
db_scan$cluster[106]

#Q2
divorce_data <- read.csv('CSVDatasets/divorce_data.csv')
head(divorce_data)

install.packages("survival", repos = "https://cran.r-project.org")
devtools::install_github("sachsmc/ggkm")

if(!require('survival')) install.packages('survival')
library(survival)

if(!require('ggplot2')) install.packages('ggplot2')
library(ggplot2)

if(!require('ggkm')) install.packages('ggkm')
library(ggkm)

attach(divorce_data)
km1 <- survfit(Surv(years, div)~1)
plot(km1, xlab = "Years", ylab = "1 divorce boi")

km2 <- survfit(Surv(years, div)~heduc)
summary(km2)

plot(km2, col=3:5, lty=3:5)
lLab <- gsub("x=","",names(km2$strata))
legend(
  "top",
  legend=lLab,
  lty=3:5,
  col=3:5,
  bty='n')

km3 <- survfit(Surv(years, div)~mixed)
summary(km3)

plot(km3, col=3:4, lty=3:4)
lLab <- gsub("x=","",names(km3$strata))
legend(
  "top",
  legend=lLab,
  lty=3:5,
  col=3:5,
  bty='n')

km4 <- survfit(Surv(years, div)~heblack)
summary(km4)

plot(km4, col=3:4, lty=3:4)
lLab <- gsub("x=","",names(km4$strata))
legend(
  "top",
  legend=lLab,
  lty=3:5,
  col=3:5,
  bty='n')

detach(divorce_data)


#Q3
cpi_data <- read.csv('CSVDatasets/cpi_earners.csv')

if(!require('forecast')) install.packages('forecast')
library(forecast)

x <- ts(cpi_data[,-1], start = c(1960,1), frequency=4) #Data is quarterly
plot(x)

arima_fit <- auto.arima(x, approximation=FALSE,trace=TRUE)

cpi_forecast <- predict(arima_fit, n.ahead = 16)

par(mfrow = c(1,1))
plot(x, type='l',xlim=c(1960,2020),ylim=c(0,1.5),xlab = 'Year',ylab = 'CPI')
lines(cpi_forecast$pred,col='blue') 
lines(cpi_forecast$pred+2*cpi_forecast$se,col='orange')
lines(cpi_forecast$pred-2*cpi_forecast$se,col='orange')

print(cpi_forecast)

summary(arima_fit)
