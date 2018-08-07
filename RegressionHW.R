getwd()
setwd("Documents/R")
housing_data <- read.csv("CSVDatasets/Boston_Housing.csv", header = TRUE)

bostrain <- housing_data[1:330,]
bostest <- housing_data[331:506,]
#boxplot(LSTAT)
#boxplot(MEDV)
boxplot(horizontal = TRUE, housing_data$LSTAT, housing_data$MEDV, names = c("LSTAT", "MEDV"), las = 2)
max
plot(housing_data$LSTAT, housing_data$MEDV, xlab = "LSTAT", ylab = "MEDV")
summary(housing_data$LSTAT)
lstat_q1 <- 6.95
lstat_q3 <- 16.95
lstat_iqr <- lstat_q3-lstat_q1
#CUTOFF MIN/MAX
lstat_q1 - (1.5*lstat_iqr) #min: -8.05
lstat_q3 + (1.5*lstat_iqr) #max: 31.95
summary(housing_data$MEDV)
medv_q1 <- 17.02
medv_q3 <- 25
medv_iqr <- medv_q3-medv_q1
#CUTOFF MIN/MAX
medv_q1 - (1.5*medv_iqr) #min: 5.05
medv_q3 + (1.5*medv_iqr) #max: 36.97
min(housing_data$MEDV) #5... outlier???
attach(bostrain)
lm.fit <- lm(log(MEDV)~LSTAT+CRIM+AGE+RM+DIS)
summary(lm.fit)
anova(lm.fit)
#OUTPUT
# Call:
#   lm(formula = log(bostrain$MEDV) ~ bostrain$LSTAT + bostrain$CRIM + 
#        bostrain$AGE + bostrain$RM + bostrain$DIS)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.32533 -0.08164 -0.00651  0.06659  0.43622 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     1.4712340  0.1101920  13.352  < 2e-16 ***
#   bostrain$LSTAT -0.0101065  0.0019878  -5.084 6.26e-07 ***
#   bostrain$CRIM  -0.0127594  0.0128427  -0.994    0.321    
# bostrain$AGE   -0.0023239  0.0003902  -5.955 6.75e-09 ***
#   bostrain$RM     0.3241891  0.0146892  22.070  < 2e-16 ***
#   bostrain$DIS   -0.0266659  0.0051469  -5.181 3.89e-07 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1269 on 324 degrees of freedom
# Multiple R-squared:  0.8379,	Adjusted R-squared:  0.8354 
# F-statistic:   335 on 5 and 324 DF,  p-value: < 2.2e-16
#ANOVA OUTPUT:
# Analysis of Variance Table
# 
# Response: log(bostrain$MEDV)
# Df  Sum Sq Mean Sq   F value    Pr(>F)    
# bostrain$LSTAT   1 18.5367 18.5367 1150.6261 < 2.2e-16 ***
#   bostrain$CRIM    1  0.0003  0.0003    0.0155  0.900931    
# bostrain$AGE     1  0.1673  0.1673   10.3858  0.001399 ** 
#   bostrain$RM      1  7.8492  7.8492  487.2247 < 2.2e-16 ***
#   bostrain$DIS     1  0.4324  0.4324   26.8425 3.888e-07 ***
#   Residuals      324  5.2197  0.0161                        
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
library(car)

vif(lm.fit)
#vif OUTPUT
# bostrain$LSTAT  bostrain$CRIM   bostrain$AGE    bostrain$RM   bostrain$DIS 
# 2.923187       1.445196       2.563427       2.095535       1.993199 

#Do any variables have to be dropped because of multicollinearity?
#From the vif criteria provided from the initial model, it does not appear
#that any of the variables need to be dropped due to multicollinearity.
#The variance inflation factor is less than 3 for each of the independent
#variables in the initial model.

#Report the coefficients obtained by your model.  Would you drop any of the
#variables used in your model based on the t-scores or p-values?
#Based on the summary from the initial model, the CRIM(crime) independent variable
#fails to reject the null hypothesis  at a level of 95% certainty or even 90%.  
#This warrants dropping the CRIM variable from the model. All of the other variables
#prove to be significant with 99.9% confidence (denoted by '***').

lm.fit1 <- lm(log(MEDV)~LSTAT+AGE+RM+DIS)
summary(lm.fit1)
# Call:
#   lm(formula = log(bostrain$MEDV) ~ bostrain$LSTAT + bostrain$AGE + 
#        bostrain$RM + bostrain$DIS)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.31706 -0.07934 -0.00646  0.06535  0.44300 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     1.4599849  0.1096066  13.320  < 2e-16 ***
#   bostrain$LSTAT -0.0103253  0.0019756  -5.227 3.10e-07 ***
#   bostrain$AGE   -0.0023731  0.0003871  -6.131 2.53e-09 ***
#   bostrain$RM     0.3250424  0.0146637  22.166  < 2e-16 ***
#   bostrain$DIS   -0.0253307  0.0049682  -5.099 5.82e-07 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1269 on 325 degrees of freedom
# Multiple R-squared:  0.8374,	Adjusted R-squared:  0.8354 
# F-statistic: 418.5 on 4 and 325 DF,  p-value: < 2.2e-16

#What is the value of R-squared? What does it signify?
#R-Squared for the updated regression model is .8374 (.8379 prev).
#This tells us that the new model explains 83.74% of the variance
#seen in the dataset. The decrease in r-squared is likely because 
#the model now has 1 less independent variable.  I draw this conclusion
#from noting that the adjusted r-squared in both models are equal (.8354).

anova(lm.fit, lm.fit1)
# Analysis of Variance Table
# 
# Response: log(bostrain$MEDV)
# Df  Sum Sq Mean Sq   F value    Pr(>F)    
# bostrain$LSTAT   1 18.5367 18.5367 1150.6719 < 2.2e-16 ***
#   bostrain$AGE     1  0.1526  0.1526    9.4746  0.002261 ** 
#   bostrain$RM      1  7.8619  7.8619  488.0326 < 2.2e-16 ***
#   bostrain$DIS     1  0.4188  0.4188   25.9950 5.824e-07 ***
#   Residuals      325  5.2356  0.0161                        
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#What is the overall F-statistic and the corresponding p-value of this final model?
#What does it signify?
#F-statistic for the new model is 418.5 with a corresponding overall p-value of
#less than 2.2*10^-16. This means that I can say the independent variables as a group
#have a relationship with the dependent variable with greater than 99.9% confidence.

vif(lm.fit1)
# bostrain$LSTAT   bostrain$AGE    bostrain$RM   bostrain$DIS 
# 2.887307       2.522270       2.088371       1.857303 

#squared error column
bostest$ssqerr<- (log(bostest$MEDV)-predict(lm.fit1, bostest))^2

predict(lm.fit1, bostest)
bostest
#MSE
sum(lm.fit1$residuals^2)/(330-4-1) #0.01610943
mean(bostest$ssqerr) #0.2042487

#Report the MSE obtained on bostrain.  How much does this increase when you 
#score your model on Bostest?
#There was a significant increase when performed on bostest.  Bostrain reported
#MSE of 1.61%, which jumped to 20.42% when calculated on Bostest.

plot()

