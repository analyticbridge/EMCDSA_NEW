# Module 4 Lab 2: Association Rules
# from "Data Science and Big Data Analytics" 

###################################################
# Step 1: # Install the packages and load libraries
###################################################
setwd("~/LAB05")
install.packages('arules') #Make sure to specify a location
install.packages('arulesViz')
library('arules')
library('arulesViz')

###################################################
#Setp 3: read in the csv file as a transaction data 
###################################################
txn <- read.transactions ("MBAdata.csv",rm.duplicates = FALSE,format="single",sep=",",cols=c(1,2))

###################################################
#Step 4:Inspect transaction data
###################################################
txn@transactionInfo 
txn@itemInfo

image(txn) #What does image do?

###################################################
#Step 6: Mine Aasociation Rules
###################################################
basket_rules <- apriori(txn,parameter=list(sup=0.5,conf=0.9,target="rules"))
inspect(basket_rules)

###################################################
#Step 7: Read in Groceries data
###################################################
data(Groceries)
Groceries
Groceries@itemInfo

###################################################
#Step 8: Mine rules
###################################################
rules <- apriori(Groceries, parameter=list(support=0.001, confidence=0.5))

###################################################
#Step 9: Extract rules with confidence =0.8
###################################################
rules
subrules <- rules[quality(rules)$confidence > 0.8]
inspect(subrules)

# Visualize rules as a scatter plot (with jitter to reduce occlusion)
plot (subrules, control=list(jitter=2))

#Extract the top three rules with high lift 
rules_high_lift <- head(sort(rules, by="lift"), 3)
inspect(rules_high_lift)
plot(rules_high_lift, method="graph", control=list(type="items"))