# Load data
fitness=read.csv("CSVDatasets/FITNESS.csv")

#boxplot of every variable
boxplot(fitness,xlab=names(fitness), las=2)

#boxplot for categorical variables
boxplot(fitness$Oxygen_Consumption~fitness$Gender)

#Adding a value based on a condition in data frame
fitness$NGender[fitness$Gender=="M"] <- 1
fitness$NGender[fitness$Gender=="F"] <- 0

#Removing columns 
fitness$Gender=NULL
fitness$Name=NULL

#You can use similar code to add a calculated column
# For example, 
fitness$LOxygCons = -log(fitness$Oxygen_Consumption)
names(fitness)
head(fitness)
View(fitness)

#str displays the structure of an R object
str(fitness)

# lower/upper inner fence and upper inner fence formula is given at
# http://www.itl.nist.gov/div898/handbook/prc/section1/prc16.htm
# RunTime and Weight has outliers
uif_RunTime<-quantile(fitness$RunTime,.75)+1.5*IQR(fitness$RunTime)
lif_Weight<-quantile(fitness$Weight, .25)-1.5*IQR(fitness$Weight)

# Check the outlier values
fitness$RunTime[fitness$RunTime>uif_RunTime]
fitness$Weight[fitness$Weight<lif_Weight]

# Boxplot shows min and max w/o outliers, 25th and 75th percentile, 
# and the median.
boxplot(fitness$RunTime)

# How do we remove outliers, then?
fitness_nol<-subset(fitness,RunTime<uif_RunTime)

# Compare the values now with the boxplot of RunTime
boxplot(fitness_nol$RunTime)
min(fitness_nol$RunTime)
max(fitness_nol$RunTime)
quantile(fitness$RunTime,.25)
quantile(fitness$RunTime,.75)
median(fitness$RunTime)

# Try identifying these values for the boxplot of Weight.

# In many cases, you should not remove outliers in your model. 
# But obvious outliers (e.g. negative values when you are expecting 
# a positive value) should be removed.