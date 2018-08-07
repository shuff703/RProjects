###########################################
# Understanding multicollinearity.        #
# Please see Dr. Park if you need more    #
# information about how you use this code #
###########################################
# Suppose the true relationship between y and x variables can be 
# described as 
# y = 10+5*x1+5*x2-2*x3. 
# But, x3 is correlated with x1 with the relationship 
# x3=2-5*x1. In fact, you could've set the equation as 
# y = 6 + 15*x1 + 5*x2 [= 10+5*x1+5*x2-2(2-5*x1)] or 
# y = 12 +5*x2 -3*x3 = [=10+(2-x3) +5*x2 - 2*x3]
# See how vif can be used to detect the multicollinearity and 
# how the coefficients change when two indep. variables are correlated.

if (!require(car)) install.packages("car")
library(car)

collinearity_test <-function (n=100, sdev=100, sd_x13 = 1)
{
  x1<-runif(n,min=10,max=20) # randomly pick n values for x1
  x2<-runif(n,min=-50, max=50)
  err13 <-rnorm (n, mean=0,sd=sd_x13)
  x3<- 2-5*x1 +err13
  
  err<- rnorm(n, mean=0, sd=sdev) # errors are drawn from a Normal dist'n.
  
  ytrue <- 10+5*x1+5*x2-2*x3
  y <- ytrue+err
  
  dataset=data.frame(y,x1,x2,x3)
  
  pairs(dataset) # graphically shows the relationship between variables
  
  lm.fit1<-lm(y~x1)
  lm.fit2<-lm(y~x2)
  lm.fit3<-lm(y~x3)
  lm.fit12<-lm(y~x1+x2)
  lm.fit13<-lm(y~x1+x3)
  lm.fit23<-lm(y~x2+x3)
  lm.fitall<-lm(y~x1+x2+x3)
  
  # You can observe that R^2 increases as you add more variables.
  print(summary(lm.fit1))
  print(summary(lm.fit2))
  print(summary(lm.fit3))
  print(summary(lm.fit12))
  print(vif(lm.fit12))
  print(summary(lm.fit13))
  print(vif(lm.fit13))
  print(summary(lm.fit23))
  print(vif(lm.fit23))
  print(summary(lm.fitall))
  print(vif(lm.fitall))
}

# Try the following and examine the coefficients

# collinearity_test(100,100,1)
collinearity_test(5,10,10)

# Both the coefficient estimates and p-values are affected when there is a multicollinearity.
# If you drop one of the correlated variables, you will be able to estimate the coefficients
# accurately.

# It is, however, possible for you to estimate the coefficients accurately even when two
# variables are correlated. This happens when vif values are low. 
# Look at the following interesting example. 

# collinearity_test(1000,100,100)

# Due to high error variance (sd_x13=100) between x1 and x3, vif values are close to 1.
# As a result, you won't have problem in estimating the coefficients.

