getwd()
#install packages if they are not installed
if(!require("tree")) install.packages("tree")
if(!require("rpart")) install.packages("rpart")
if(!require("party")) install.packages("party")
if(!require("C50")) install.packages("C50")
if(!require("caret")) install.packages("caret")
if(!require("e1071")) install.packages("e1071")
if(!require("ROCR")) install.packages("ROCR")

# function definitions to simplify ROC curve drawing and AUC calculation
plotROC <-function (pred_probs, ...)
{
  pred_obj<-prediction(pred_probs, titan_test$Survival)
  perf<-performance(pred_obj, measure="tpr",x.measure="fpr")
  plot(perf, ...)
}

calculateAUC <-function (pred_probs)
{
  pred_obj<-prediction(pred_probs, titan_test$Survival)
  auc_obj<-performance(pred_obj, "auc")
  auc_obj@y.values[[1]]
}

# Read the dataset and remove missing values
titanic=read.csv("CSVDatasets/TITANIC_FORMATTED.csv")
Titanic_NM = na.omit(titanic)

# Let's use a different method to split the dataset to training and test set.
# changing the random seed may result in different result.
# Please try "Amelia" package if you are interested improving the model
# by including missing values.

set.seed(6201)
dtype<-sample(2,nrow(Titanic_NM),replace=TRUE,prob=c(0.7,0.3))
#dtype=1 : training set dtype=2: test set
sum(dtype==1) # size of training set
sum(dtype==2) # size of test set

titan_train <- Titanic_NM[dtype==1,]
titan_test <- Titanic_NM[dtype==2,]

# We're going to first use the "tree" package.
attach(titan_train)
library(tree)
tree.fit<-tree(Survival~Age+Gender+Class+Fare, data=titan_train)
summary(tree.fit)
print(tree.fit)
plot(tree.fit)
text(tree.fit,pretty=0)
library(caret)
confusionMatrix(table(predict(tree.fit,type="class",titan_train),titan_train$Survival))
tree_pred<-predict(tree.fit, type="class",titan_test)
confusionMatrix(table(tree_pred,titan_test$Survival))

library(ROCR)
tree_probs<-(predict(tree.fit, type="vector",titan_test))[,2]
plotROC(tree_probs, colorize=TRUE)
abline(a=0,b=1)
calculateAUC(tree_probs)

tree.fit2<-tree(Survival~Age+Gender+Class+Fare, split="gini", data=titan_train)
summary(tree.fit2)
print(tree.fit2)
plot(tree.fit2)
text(tree.fit2,pretty=0)
confusionMatrix(table(predict(tree.fit2,type="class",titan_train),titan_train$Survival))
tree_pred2<-predict(tree.fit2, type="class",titan_test)
confusionMatrix(table(tree_pred2,titan_test$Survival))

tree_probs2<-(predict(tree.fit2, type="vector",titan_test))[,2]
plotROC(tree_probs2, colorize=TRUE)
abline(a=0,b=1)
calculateAUC(tree_probs2)

# Another package for decision tree classifiers
library(rpart)
rpart.fit<-rpart(Survival~Age+Gender+Class+Fare, data=titan_train, parms=list(split="information"))
summary(rpart.fit)
print(rpart.fit)
plot(rpart.fit)
text(rpart.fit, use.n=T)
confusionMatrix(table(predict(rpart.fit,type="class",titan_train),titan_train$Survival))
rpart_pred<-predict(rpart.fit,type="class",titan_test)
confusionMatrix(table(rpart_pred,titan_test$Survival))
rpart_probs<-(predict(rpart.fit, type="prob",titan_test))[,2]
plotROC(rpart_probs, colorize=TRUE)
abline(a=0,b=1)
calculateAUC(rpart_probs)

# Yet another package for decision tree classifers
library(party)
ctree.fit<-ctree(Survival~Age+Gender+Class+Fare, data=titan_train)
ctree.fit
summary(ctree.fit)
plot(ctree.fit)
confusionMatrix(table(predict(ctree.fit,type="response",titan_train),titan_train$Survival))
ctree_pred<-predict(ctree.fit,type="response",titan_test)
confusionMatrix(table(ctree_pred,titan_test$Survival))
ctree_probs<-unlist(lapply(predict(ctree.fit, type="prob", titan_test), `[[`,2))
plotROC(ctree_probs, colorize=TRUE)
abline(a=0,b=1)
calculateAUC(ctree_probs)

# This is an improved version of the popular ID3/C4.5 algorithms.
library(C50)
see5.fit<-C5.0(Survival~Age+Gender+Class+Fare, data=titan_train)
summary(see5.fit)
plot(see5.fit)
confusionMatrix(table(predict(see5.fit,type="class",titan_train),titan_train$Survival))
see5_pred<-predict(see5.fit,type="class",titan_test)
confusionMatrix(table(see5_pred,titan_test$Survival))
see5_probs<-(predict(see5.fit, type="prob",titan_test))[,2]
plotROC(see5_probs, colorize=TRUE)
abline(a=0,b=1)
calculateAUC(see5_probs)


# Predictions using other classifers we've learned before.
glm.fit<-step(glm(Survival~Age+Gender+Class+Fare,family=binomial, 
                  data=titan_train), direction="both")
library(MASS)
lda.fit<-lda(Survival~Age+Gender+Class, data=titan_train)
qda.fit<-qda(Survival~Age+Gender+Class, data=titan_train)

library(e1071)
bayes.fit<-naiveBayes(Survival~Age+Gender+Class,data=titan_train)


# for KNN
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
knn.fit<-knn(Xvar_train,Xvar_test,titan_train$Survival,k=3, prob = TRUE)

# probabilities and classification results for evaluation and ROC drawing
contrasts(Survival)
glm_probs <- predict(glm.fit, titan_test,type="response")
glm_pred <- ifelse(glm_probs>0.5, "Survived", "Died")
plotROC(glm_probs, colorize=TRUE)
calculateAUC(glm_probs)

lda_pred <- predict(lda.fit, titan_test)
lda_probs <- lda_pred$posterior[,2]
qda_pred <- predict(qda.fit, titan_test)
qda_probs <- qda_pred$posterior[,2]
bayes_pred<-predict(bayes.fit,titan_test)
bayes_probs<-(predict(bayes.fit, type="raw", titan_test))[,2]
knn_probs<- ifelse(knn.fit=="Survived", attr(knn.fit,"prob"), 1-attr(knn.fit,"prob"))


#Evaluation metrics and ROC curves for all classifiers
library(caret)
confusionMatrix(table(glm_pred, titan_test$Survival))
confusionMatrix(table(lda_pred$class, titan_test$Survival))
confusionMatrix(table(qda_pred$class, titan_test$Survival))
confusionMatrix(table(bayes_pred, titan_test$Survival))
confusionMatrix(table(knn.fit, titan_test$Survival))
confusionMatrix(table(tree_pred, titan_test$Survival))
confusionMatrix(table(tree_pred2, titan_test$Survival))
confusionMatrix(table(ctree_pred, titan_test$Survival))
confusionMatrix(table(rpart_pred, titan_test$Survival))
confusionMatrix(table(see5_pred, titan_test$Survival))

plotROC(glm_probs)
abline(a=0,b=1)
calculateAUC(glm_probs)
plotROC(lda_probs, add=TRUE, col="red")
calculateAUC(lda_probs)
plotROC(qda_probs, add=TRUE, col="blue")
calculateAUC(qda_probs)
plotROC(bayes_probs, add=TRUE, col="cyan")
calculateAUC(bayes_probs)
plotROC(knn_probs, add=TRUE, col="green")
calculateAUC(knn_probs)
plotROC(tree_probs, add=TRUE, col="navy")
calculateAUC(tree_probs)
plotROC(tree_probs2, add=TRUE, col="azure")
calculateAUC(tree_probs2)
plotROC(ctree_probs, add=TRUE, col="purple")
calculateAUC(ctree_probs)
plotROC(rpart_probs, add=TRUE, col="orange")
calculateAUC(rpart_probs)
plotROC(see5_probs, add=TRUE, col="brown")
calculateAUC(see5_probs)

