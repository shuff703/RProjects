getwd()
setwd('Documents/R')

train_data <- read.csv('CSVDatasets/train_u6lujuX_CVtuZ9i.csv')
test_data <- read.csv('CSVDatasets/test_Y3wMUE5_7gLdaTN.csv')

sum(is.na(train_data$Loan_ID))
sum(is.na(train_data$Gender))
sum(is.na(train_data$Married))
sum(is.na(train_data$Dependents))
sum(is.na(train_data$Education))
sum(is.na(train_data$Self_Employed))
sum(is.na(train_data$ApplicantIncome))
sum(is.na(train_data$CoapplicantIncome))
sum(is.na(train_data$LoanAmount))
sum(is.na(train_data$Loan_Amount_Term))
sum(is.na(train_data$Credit_History))
sum(is.na(train_data$Property_Area))
sum(is.na(train_data$Loan_Status))

train_filtered <- train_data[complete.cases(train_data),]
train_filtered <- train_filtered[,!names(train_filtered) %in% c('Loan_ID')]

if(!require(randomForest)) install.packages("randomForest")
library(randomForest)

if(!require(caret)) install.packages("caret")
library(caret)

if(!require(gbm)) install.packages('gbm')
library(gbm)

X_train <- train_filtered[,!names(train_filtered) %in% c('Loan_Status', 'Loan_ID')]
X_test <- test_data[,!names(train_filtered) %in% c('Loan_Status', 'Loan_ID')]

y_train <- train_filtered$Loan_Status

y_train <- as.factor(y_train)

fit_control <- trainControl(method = "cv",
                            number = 5)

model <- train(Loan_Status~., train_filtered, trControl=fit_control, metric = 'Accuracy')


y_pred <- predict(model, newdata=X_test, type='raw')
print(model)

out_data <- data.frame()
out_data$Loan_ID <- ifelse(y_pred == 1, 'Y', 'N')
out_data$Loan_Status <- y_test$Loan_ID
  


out_data <- temp_data[,c("Loan_ID","Loan_Status")]
write.csv(out_data,"~/Downloads/to_submit.csv", row.names = F)