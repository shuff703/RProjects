#Model 1 - Red, Model 2 - Blue, Dominant Model - Green

library(ROCR)
#Build the ROC Curves based on the models
actual<-c("p","p","n","p","n","p","n","n","p","n")
prob1<-c(0.52, 0.95, 0.55, 0.70,0.45,0.40,0.55,0.30,0.65,0.35)
prob2<-c(0.75,0.80,0.30,0.40,0.20,0.55,0.75,0.35,0.40,0.25)
hwdata<-data.frame(actual,prob1, prob2)
hwdata
pred1<-prediction(hwdata$prob1, hwdata$actual)
pred2<-prediction(hwdata$prob2, hwdata$actual)
perf1<-performance(pred1, measure="tpr",x.measure="fpr")
perf2<-performance(pred2, measure="tpr",x.measure="fpr")
plot(perf1,col="red")
abline(a=0,b=1)
plot(perf2,add=TRUE, col="blue")
auc_obj1<-performance(pred1, "auc")
auc_obj1@y.values
auc_obj2<-performance(pred2, "auc")
auc_obj2@y.values

#Almost perfect model
hwdata$actual<-c("p","p","p","p","p","n","n","n","n","n")
hwdata$prob3<-c(0.99,0.98,0.97,0.96,0.95,0.05,0.04,0.03,0.02,0.01)
pred3<-prediction(hwdata$prob3, hwdata$actual)
perf3<-performance(pred3, measure="tpr",x.measure="fpr")
plot(perf3,add=TRUE, col="green")
auc_obj3<-performance(pred3, "auc")




auc_obj3@y.values
