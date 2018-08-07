setwd("Documents/R/")
Titanic1<-read.csv("CSVDatasets/TITANIC_FORMATTED.csv", header = TRUE)
head(Titanic1)

attach(Titanic1)

glm.fit<-glm(Survival~Age, family = binomial, data = Titanic1)
summary(glm.fit)
#Summary determines significance

predictDeath <- function(age, gender, class2nd, class3rd) {
  p<-1/(1 + exp(1)^(-logReg(age, gender, class2nd, class3rd)))
  return(p)
}
predictDeath(5, 0, 0, 0)
predictDeath(30)

glm.fit<-glm(Survival~Age+Gender+Class, family = binomial)
summary(glm.fit)
detach(glm.fit)
logReg <- function(age, gender, class2nd, class3rd) {
  return(glm.fit$coefficients[1]+glm.fit$coefficients[2]*age+glm.fit$coefficients[3]*gender+glm.fit$coefficients[4]*class2nd+glm.fit$coefficients[5]*class3rd)
}
predictDeath(30, 0, 0, 1)

predictDeath(1, 1, 1, 0) + predictDeath(1, 1, 0, 1)
predictDeath(7, 0, 0, 0)
predictDeath(7, 1, 0, 0)
plot(predictDeath(1:150, 1, 0:1, 0:1))
plot(predictDeath(1:150, 0:1, 1, 0))
plot(predictDeath(1:150, 0:1, 0, 1))
plot(predictDeath(1:150, 1 ,0, 0))
plot(predictDeath(1:150, 1, 1, 0))
plot(predictDeath(1:100, 1, 0, 1))
predictDeath(100, 0, 0, 0)

install.packages("aod")
library(aod)
wald.test(Sigma = vcov(glm.fit), b = coef(glm.fit), Terms = 4:5)
wa