# Read data set and name it "bodyfat"
bodyfat<-read.csv("CSVDatasets/BODYFAT2.csv")
# Split the data set into two
bodyfat_train<-bodyfat[1:150,]
bodyfat_test<-bodyfat[151:252,]
attach(bodyfat_train)

# Let's try some intuitive models
lm.fit1<-lm(PctBodyFat2~Weight+Height+Adioposity+FatFreeWt)
lm.fit2<-lm(PctBodyFat2~Weight+Height+Adioposity+FatFreeWt+Knee)
lm.fit3<-lm(PctBodyFat2~Weight+Height+Adioposity+FatFreeWt
            +Knee+Ankle)
lm.fit4<-lm(PctBodyFat2~Weight+Height+FatFreeWt)

library(car)
summary(lm.fit1)
vif(lm.fit1)
summary(lm.fit2)
vif(lm.fit2)
summary(lm.fit3)
vif(lm.fit3)
summary(lm.fit4)
vif(lm.fit4)
# The step-wise regression checks if two models are different enough.  This can
# be accomplished by running partial F-test. The significance of F, i.e., Pr(>F)
# is less than <.05 between model 1 and model 2, but >.05 between model2 and model 3.
# In general, if you include a variable(s) that are not significant, you do not reject 
# the hypothesis that two models are the same (H0: coeff. of added x var =0).

anova(lm.fit1,lm.fit2)
anova(lm.fit2,lm.fit3)

# It seems like PctBodyFat1 is another potential response variable to replace
# PctBodyFat2 with. So, let's exclude it for fair comparison of alternative models.

step(lm(PctBodyFat2~.-PctBodyFat1,data=bodyfat_train))

# This is the result of step-wise regression.
lm.fit4<-lm(formula = PctBodyFat2 ~ Density + Age + Weight + Height + 
              FatFreeWt + Chest + Thigh + Knee + Forearm, data = bodyfat_train)
summary(lm.fit4)

# The above may be the best model.  What would happen if we include all variables?
# R^2, will obviously increase.  But does this indicate it is the best model?
# You will later see, the prediction will hurt if you focus only on fitting the data. 
lm.fitall<-lm(PctBodyFat2~.-PctBodyFat1, data=bodyfat_train)

# The following is needed to calculate the MSE on the test set.
bodyfat_test$sqerr_m1<-(bodyfat_test$PctBodyFat2-
                          predict(lm.fit1, bodyfat_test))^2
bodyfat_test$sqerr_m2<-(bodyfat_test$PctBodyFat2-
                          predict(lm.fit2, bodyfat_test))^2
bodyfat_test$sqerr_m3<-(bodyfat_test$PctBodyFat2-
                          predict(lm.fit3, bodyfat_test))^2
bodyfat_test$sqerr_m4<-(bodyfat_test$PctBodyFat2-
                          predict(lm.fit4, bodyfat_test))^2
bodyfat_test$sqerr_m5<-(bodyfat_test$PctBodyFat2-
                          predict(lm.fitall, bodyfat_test))^2

# Estimated MSE from the training set is as follows.
# Note that you have to divide the sum by n-p-1 (#obs. - #variables -1)

sum((lm.fit1$residuals)^2)/(150-4-1)
sum((lm.fit2$residuals)^2)/(150-5-1)
sum((lm.fit3$residuals)^2)/(150-6-1)
sum((lm.fit4$residuals)^2)/(150-9-1)
sum((lm.fitall$residuals)^2)/(150-17-1)

# The MSE on the test set can be calculated as follows.
# Note that you do not subtract d.f. for the test set. 
# You just need to average the squared errors.
# Compare these with the values above.

mean(bodyfat_test$sqerr_m1)
mean(bodyfat_test$sqerr_m2)
mean(bodyfat_test$sqerr_m3)
mean(bodyfat_test$sqerr_m4)
mean(bodyfat_test$sqerr_m5)

