## Read the Data:
data <- read.csv("~/Downloads/train_u6lujuX_CVtuZ9i.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?"))

summary(data)

## Impute Missing Values:
library(mice)
impute_data <- mice(data[,2:13], meth='pmm')
data <- complete(impute_data)

## Create the Training and Test Data:
n = nrow(data) # n will be ther number of obs. in data
trainIndex = sample(1:n, 
                    size = round(0.7*n), 
                    replace=FALSE) # We create an index for 70% of obs. by random
train_data = data[trainIndex,] # We use the index to create training data
test_data = data[-trainIndex,] # We take the remaining 30% as the testing data
summary(train_data)
summary(test_data)

## Build the RF Classifier:
library(randomForest)

rf <- randomForest(Loan_Status~., data = train_data, ntree=10, mtry = 3, na.action=na.exclude, importance=T) 
print(rf)

## Test RF Classifier Using ROC Curve:
library(ROCR)
predicted_values <- predict(rf, test_data, type= "prob") # Use the classifier to make the predictions
pred <- prediction(predicted_values[,2], test_data$Loan_Status)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]

roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="Random Forest")

library(ggplot2)
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc))

## Apply the Final RF Classifier to Hackathon's Test Data:
t_data <- read.csv("~/Downloads/test_Y3wMUE5_7gLdaTN-2.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?"))

## Impute Missing Values in Hackathon's Test Data:
impute_t_data <- mice(t_data[,2:12], exclude = "Loan_ID", meth = 'pmm')
temp_data <- complete(impute_t_data)
temp_data$Loan_Status <- predict(rf, temp_data, type= "response")
temp_data$Loan_ID <- t_data$Loan_ID

out_data <- temp_data[,c("Loan_ID","Loan_Status")]
write.csv(out_data,"~/Downloads/to_submit.csv", row.names = F)
