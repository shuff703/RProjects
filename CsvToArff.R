getwd()
setwd('data/csv/')

mydata = read.csv('reviews-with-conditions/conditions.csv', header = TRUE)
if(!require("foreign")) install.packages("foreign")
library('foreign')

setwd('../')

write.arff(x = mydata, file = 'Conditions.arff')
