
#Run Association Rules
if(!require(arules)) install.packages("arules")
library(arules)

setwd("Documents/R")
#### Large dataset ####

# Reading the data set as a dataframe object creates many problems.
# So, let us read the data set as a transaction object.
#bank<-read.csv("bank.csv")
transactions<-read.transactions("CSVDatasets/transactions.csv", format="single", 
                             cols=c(2,3), sep=",", rm.duplicates = T, skip=1)
#Display items
transactions@itemInfo

# Frequent itemsets
frqisets<-apriori(transactions, parameter=list(minlen=2, supp=0.02, conf=0.2, target="frequent itemsets"))
inspect(sort(frqisets, by="support"))

# Association rules
rules<-apriori(transactions, parameter=list(minlen=2, supp=0.02, conf=0.2, target="rules"))
inspect(rules)
rules.sorted<-sort(rules, by="lift")
inspect(rules.sorted)

quality(rules)<-round(quality(rules), digits=3)
inspect(sort(rules,by="confidence"))

if(!require(arulesViz)) install.packages("arulesViz")
library(arulesViz)
# Different plots
plot(rules.sorted)
plot(rules.sorted, method="grouped")
plot(rules.sorted, method="graph", control=list(type="items"))
plot(rules.sorted, method="paracoord")
# Deep dive into a single rule
set.seed(6201)
onerule<-sample(rules.sorted, 1)
inspect(onerule)
plot(onerule, method="doubledecker", data=transactions)

