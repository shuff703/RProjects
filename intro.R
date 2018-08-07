# Note that any command/expression will be ignored after #
# In order to run a line or selection, press "Ctrl+Enter"
# In order to run the whole script, press "Ctrl+Shift+S"
# get working directory
getwd()

# set working directory
#setwd("C:/R")

# come calculations, built-in constants, functions
10^2+36
LETTERS
month.abb
sqrt(2)
sin(pi/2)
exp(1)
runif(10)
rnorm(10)
sample(10)

# installing a package PLEASE DO NOT run the next line in class!
install.packages("plot3D")
library(plot3D)

# assignment operator and logical operator
a<-4
b=a+2
a==b
a!=b
c<-a+b
rm(c)
rm(list=ls())

# a vector using concatenator function
b<-c(3,4,5)
class(b)

#creating a function
nHello<-function(n)
{
  for(i in 1:n)
      print("Hello")
}

nHello(20)

nHello(n=5)

# median function?  
# This needs to be fixed. But, see how R treats non-integer index
my_median<-function(x)
{
  xsorted<-sort(x)
  xsorted[(length(x)+1)/2]
}

my_median(c(2,3,4,1))
median(c(2,3,4,1))

# index of a vector
x<-c(1,2,3)
x[1.5]
x[1.999999]
x[2.0]

# generating random numbers
rnorm(10,mean=1.2,sd=3.4)
runif(10,min=20,max=25)

# creating plots
x=rnorm(1000)
plot(x)

y=runif(1000)
plot(y)

z<-x+y
library(plot3D)
scatter3D(x,y,z)

# Getting help
help(rnorm)
?runif

help.start()

#####################################
# Data structures: vectors
vec1=c(1,4,6,8,10)
vec1
vec1[5] # to retrieve the 5th element
vec1[3] = 12 # to assign 12 to the 3rd element
vec1

vec2=seq(from=0,to=1,by=0.25)
vec2
sum(vec1)
vec1+vec2

####################################
# Data structures: matrices
mat=matrix(data=c(9,2,3,4,5,6),ncol=3)
mat
# a matrix is merely another representation of a vector
dim(mat)
# You can change the dimension. 
dim(mat)<-c(3,2)  
# Note the order of elements
mat

# accessing an element of a matrix
mat[1,2]
mat[1,]
mat[,2]
mean(mat)
########################################
# Data structures: data frames
t = data.frame(x = c(11,12,14), y = c(19,20,21), z = c(10,9,7))
# each column has a name
t
mean(t$z)
# you can change the name of a column
names(t)[1]<-("a")
names(t)<-(c("a","b","c"))
t

mean(t[["c"]])
#######################################
# Data Structures: list
L=list(one=1, two=c(1,2), five=seq(0, 1, length=5))
L
names(L)
L$five +10

######################################
# More plotting..
plot(rnorm(100), type="l", col="gold")
hist(rnorm(100))

plot(t$a, type="l", ylim=range(t), lwd=3, col=rgb(1,0,0,0.3))
lines(t$b, type="s", lwd=2, col=rgb(0.3,0.4,0.3,0.9))
points(t$c, pch=20, cex=4,col=rgb(0,0,1,0.3))

#######################################
# Reading and writing data files
d = data.frame(a = c(3,4,5),b = c(12,43,54))
d
write.table(d,file="tst0.txt",row.names=FALSE)
d2 = read.table(file="tst0.txt",header=TRUE)
d2

write.csv(d,file="tst0.csv", row.names=FALSE)

#########################################
# Not available data
j=c(1,2,NA)
max(j)
max(j,na.rm=TRUE)

########################################
# Dates
date1=strptime( c("20100225230000","20160226000000", "20160226010000"), format="%Y%m%d%H%M%S")
date1

#######################################
#If statement
w=3
if(w<5)
{
  d=2
}else{
  d=10
}
  
d

######################################
#A bit confusing but useful condition for vector indices
a = c(1,2,3,4)
b = c(5,6,7,8)
f = a[b==5 | b==8]
f

######################################
# For loop
h = seq(from=1, to=8)
s = c()
for(i in 2:10)
{
  s[i] = h[i] * 10  
}
s
