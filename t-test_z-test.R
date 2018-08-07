# Let X be a random variable that denotes the daily sales
# of ice cream (in lb) of Queen City Dessert Place
# Suppose X~ N(120, 30^2)
# And then, take 5 observations from this distribution.
x<-rnorm(5,120,30)
# sample mean and sample s.d. can be obtained as follows.
xBar<-mean(x); s<-sqrt(sum((x-mean(x))^2)/(5-1))
# t-statistic is calculated as follows

t<- (xBar-120)/(s/sqrt(5))

t_critical<-qt(p=0.05, df=4) #one-tail  
t < t_critical # If this is true, we reject the Hypothesis
                # In fact, this indicates that we are committing 
                # a Type I error (rejecting H0 when it is true)

# z-test can be used if you know the real s.d. (=30 in this example)
z<-(xBar-120)/(30/sqrt(5))
z_critical<-qnorm(p=0.05) # one-tail for z-test
z < z_critical

# Try this 1000 times and record the test results 
HypoTest_t<-NULL
HypoTest_z<-NULL
for(i in 1:1000)
{
  x<-rnorm(5,120,30)
  xBar<-mean(x); s<-sqrt(sum((x-mean(x))^2)/(5-1))
  t<- (xBar-120)/(s/sqrt(5))
  HypoTest_t[i]<- (t<t_critical)
  z<-(xBar-120)/(30/sqrt(5))
  HypoTest_z[i] <-(z<z_critical)
  
}
table(HypoTest_t)
table(HypoTest_z)

