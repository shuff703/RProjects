getwd()
setwd('/Users/Scott/Documents/R')
library(data.table)
data <- fread("CSVDatasets/ATT_Twitter.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?", "Inf"))

if(!require('forecast'))install.packages('forecast')
library(forecast)

x = ts(data[,2])

par(mar=c(1,1,1,1))
plot(x)

z <- log10(x)
plot(z)

y <- diff(z)
plot(y)

PP.test(x)
version

nrow(is.na(y))

par(mfrow = c(1,2))
acf(y,main='ACF', na.rm = T)
pacf(y,main='PACF', na.rm = T)
