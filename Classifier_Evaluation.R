setwd("Documents/R")
titanic=read.csv("CSVDatasets/TITANIC_FORMATTED.csv")
attach(titanic)
# Let's try step-wise regression, it works even with logistic regression
step(glm(Survival~Age+Gender+Class+Fare,family=binomial), direction="both")
# It seems that there are some missing values (NA's) in Age.
summary(titanic)

# For simplicity we will remove missing values, and run the step-wise again.
Titanic_NM = na.omit(titanic)
detach(titanic)
attach(Titanic_NM)
step(glm(Survival~Age+Gender+Class+Fare,family=binomial), direction="both")

# For a better treatment of missing values, (e.g., imputation of missing data)
# the "Amelia" package would be a great tool. http://gking.harvard.edu/amelia/
# The visualization of missing values immediately tells us that the later part
# of the data has many age values missing.
install.packages("Amelia")
library(Amelia)
missmap(titanic)

# The dataset needs to be split into training set and test set.
# It is not a good idea to use the row id of the data because the data is ordered
# by class. 
Titanic_NM$Class

# So, let's randomly pick 30% based on the row number and split the data.
testrows <- sample(1:nrow(Titanic_NM),size=0.3*nrow(Titanic_NM))
titan_test <- Titanic_NM[testrows,]
titan_train <- Titanic_NM[-testrows,]

# We're going to use multiple classification methods and evaluate their performance.
attach(titan_train)
glm.fit<-step(glm(Survival~Age+Gender+Class+Fare,family=binomial), direction="both")

library(MASS)
lda.fit<-lda(Survival~Age+Gender+Class)
qda.fit<-qda(Survival~Age+Gender+Class)
plot(lda.fit)
install.packages("e1071")
library(e1071)
bayes.fit<-naiveBayes(Survival~Age+Gender+Class,data=titan_train)


# KNN requires manual recoding of categorical variables
contrasts(Gender)
contrasts(Class)
library(class)
attach(titan_train)
GenderMale<-ifelse(Gender=="Male", 1, 0)
Class2nd<-ifelse(Class=="2nd",1,0)
Class3rd<-ifelse(Class=="3rd",1,0)
Xvar_train<-cbind(Age, GenderMale, Class2nd, Class3rd)
attach(titan_test)
GenderMale<-ifelse(Gender=="Male", 1, 0)
Class2nd<-ifelse(Class=="2nd",1,0)
Class3rd<-ifelse(Class=="3rd",1,0)
Xvar_test<-cbind(Age, GenderMale, Class2nd, Class3rd)
set.seed(1)
knn.fit<-knn(Xvar_train,Xvar_test,titan_train$Survival,k=3)

# Now let us construct the confusion matrices for each.
contrasts(Survival)
glm.probs <- predict(glm.fit, titan_test,type="response")
glm_pred <- ifelse(glm.probs>0.5, "Survived", "Died")
table(glm_pred, titan_test$Survival)

lda_pred <- predict(lda.fit, titan_test)
table(lda_pred$class, titan_test$Survival)

qda_pred <- predict(qda.fit, titan_test)
table(qda_pred$class, titan_test$Survival)

bayes_pred<-predict(bayes.fit,titan_test)
table(bayes_pred,titan_test$Survival)

table(knn.fit, titan_test$Survival)

# Calculate different evaluation metrics by hand first.
# Accuracy, Precision, Recall, Sensitivity, Specificity, 
# True Positive rate, True Negative rate, ...
# There is an R package that calculate these metrics
if(!require(caret)) install.packages("caret")
if(!require(e1071)) install.packages("e1071")
library(caret)
confusionMatrix(table(glm_pred, titan_test$Survival))
confusionMatrix(table(lda_pred$class, titan_test$Survival))
confusionMatrix(table(qda_pred$class, titan_test$Survival))
confusionMatrix(table(bayes_pred, titan_test$Survival))
confusionMatrix(table(knn.fit, titan_test$Survival))
# If you want to change the positive to Survived, add positive="Survived"
confusionMatrix(table(glm_pred, titan_test$Survival), positive="Survived")
# Details on each calculation are available in the documentation.
?confusionMatrix
