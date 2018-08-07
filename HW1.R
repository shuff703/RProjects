getwd()
setwd('Documents/R/')

#import
data <- read.csv("CSVDatasets/election_campaign_data.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?")) 

#drop cols
data <- data[,!names(data) %in% c('cand_id', 'last_name', 'first_name', 'twitterbirth', 'facebookdate', 'facebookjan', 'youtubebirth')]

#convert binary columns to factors
data[,'twitter']  <- as.factor(data[,'twitter'])
data[,'facebook'] <- as.factor(data[,'facebook'])
data[,'youtube'] <- as.factor(data[,'youtube'])
data[,'cand_ici'] <- as.factor(data[,'cand_ici'])
data[,'gen_election'] <- as.factor(data[,'gen_election'])

data <- data[complete.cases(data),]

#random sample to split the dataset 70/30
sample_index <- sample(nrow(data), nrow(data)*.7)
train_data <- data[sample_index,]
test_data <- data[-sample_index,]

#load randomForest
if(!require('randomForest')) install.packages('randomForest')
library('randomForest')

#build the random forest ntree=10
X <- train_data[,!names(data) %in% c('gen_election')]
X_test <- test_data[,!names(data) %in% c('gen_election')]
model <- randomForest(X, y=train_data$gen_election, xtest = X_test, ytest = test_data$gen_election, ntree=10)
print(model)


#build the random forest ntree=20
model <- randomForest(X, y=train_data$gen_election, xtest = X_test, ytest = test_data$gen_election, ntree=20)
print(model)

#build the random forest ntree=30
model <- randomForest(X, y=train_data$gen_election, xtest = X_test, ytest = test_data$gen_election, ntree=30)
print(model)

#build the random forest ntree=40
#Lowest OOB estimate of error greater than this  (6.15%)
model <- randomForest(X, y=train_data$gen_election, xtest = X_test, ytest = test_data$gen_election, ntree=40)
print(model)

#build the random forest ntree=45 (5.54%)
model <- randomForest(X, y=train_data$gen_election, xtest = X_test, ytest = test_data$gen_election, ntree=45)
print(model)

#build the random forest ntree=50
#Lowest OOB estimate of error less than this (6.15%)
model <- randomForest(X, y=train_data$gen_election, xtest = X_test, ytest = test_data$gen_election, ntree=50)
print(model)

#build the random forest ntree=60
model <- randomForest(X, y=train_data$gen_election, xtest = X_test, ytest = test_data$gen_election, ntree=60)
print(model)

#Calculate the best value for number of variable to try at each split (7)
mtry <- tuneRF(train_data[-26], train_data$gen_election, ntreeTry=45,  stepFactor=1.5, improve=0.01, trace=TRUE, plot=TRUE, na.action=na.exclude)

#build a new randomforest ntree=45 & mtry=7
model <- randomForest(X, y=train_data$gen_election, xtest = X_test, ytest = test_data$gen_election, ntree=45, mtry=7, keep.forest = TRUE, importance = TRUE)
print(model)


#require and use caret
if(!require('caret')) install.packages('caret')
library('caret')

pred <- predict(model, newdata=X_test)
print(model$predicted)
confusionMatrix(pred, test_data$gen_election, positive=levels(test_data$gen_election.class[2]))

#require and use ROCR
if(!require('ROCR')) install.packages('ROCR')
library('ROCR')

#ggplot
if(!require('ggplot2')) install.packages('ggplot2')
library('ggplot2')


#AUC & ROC Curve
prediction <- prediction(as.numeric(pred), as.numeric(test_data$gen_election))
performance <- performance(prediction, measure = 'tpr', x.measure = 'fpr')

auc <- performance(prediction, measure = 'auc')

auc <- auc@y.values[[1]]
roc.data <- data.frame(fpr=unlist(performance@x.values),
                       tpr=unlist(performance@y.values),
                       model='ANN')
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0('ROC Curve with AUC=', auc))

#Feature Importance
varImpPlot(model)

#Neural Network
if(!require('nnet')) install.packages('nnet')
library('nnet')

ann <- nnet(gen_election~., data=train_data, size = 5)
print(ann)

#Visualize the Neural Network
if(!require('devtools')) install.packages('devtools')
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

plot.nnet(ann)

#Predict
ann_pred <- predict(ann, newdata = X_test, type='class') 

#1 hacky boi
ann_pred <- as.factor(ann_pred)

confusionMatrix(ann_pred, test_data$gen_election, positive=levels(test_data$gen_election.class[2]))

#AUC & ROC Curve
prediction <- prediction(as.numeric(ann_pred), as.numeric(test_data$gen_election))
performance <- performance(prediction, measure = 'tpr', x.measure = 'fpr')

auc <- performance(prediction, measure = 'auc')

auc <- auc@y.values[[1]]
roc.data <- data.frame(fpr=unlist(performance@x.values),
                       tpr=unlist(performance@y.values),
                       model='ANN')
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0('ROC Curve with AUC=', auc))

#20 hidden nodes
ann <- nnet(gen_election~., data=train_data, size = 20)

#50 hidden nodes (too many)
ann <- nnet(gen_election~., data=train_data, size = 50)

#30 hidden nodes (too many)
ann <- nnet(gen_election~., data=train_data, size = 30)

#25 hidden nodes (too many)
ann <- nnet(gen_election~., data=train_data, size = 25)

#23 hidden nodes
ann <- nnet(gen_election~., data=train_data, size = 23)

#24 hidden nodes (Max)
ann <- nnet(gen_election~., data=train_data, size = 24)

#Same code from above to generate metrics and ROC
#Predict
ann_pred <- predict(ann, newdata = X_test, type='class') 

#1 hacky boi
ann_pred <- as.factor(ann_pred)

confusionMatrix(ann_pred, test_data$gen_election, positive=levels(test_data$gen_election.class[2]))

#AUC & ROC Curve
prediction <- prediction(as.numeric(ann_pred), as.numeric(test_data$gen_election))
performance <- performance(prediction, measure = 'tpr', x.measure = 'fpr')

auc <- performance(prediction, measure = 'auc')

auc <- auc@y.values[[1]]
roc.data <- data.frame(fpr=unlist(performance@x.values),
                       tpr=unlist(performance@y.values),
                       model='ANN')
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0('ROC Curve with AUC=', auc))

#Visualize the Neural Network
if(!require('devtools')) install.packages('devtools')
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

plot.nnet(ann)


#Social Media Analysis
ftable(xtabs(~facebook+gen_election, data=data))
ftable(xtabs(~twitter+gen_election, data=data))
ftable(xtabs(~youtube+gen_election, data=data))
ftable(xtabs(~facebook+twitter+youtube+gen_election, data=data))

#NNET Importance viz
source_url('https://gist.githubusercontent.com/fawda123/6206737/raw/d6f365c283a8cae23fb20892dc223bc5764d50c7/gar_fun.r')

#create a pretty color vector for the bar plot
cols<-colorRampPalette(c('red','green'))(25)

#use the function on the model created above
par(mar=c(4,4,1,1),family='serif')
plot <- gar.fun('y',ann)

plot <- plot + theme_bw() + theme(legend.position = 'none', axis.text.x=element_text(angle=90,hjust=1))

plot
