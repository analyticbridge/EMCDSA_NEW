# Module 4 Lab 6: Decision Trees
# from "Data Science and Big Data Analytics" 

###################################################
#Step 2: Set the Working Directory and Install Packages
###################################################
setwd("~/LAB09")
install.packages("rpart.plot")
library("rpart")
library("rpart.plot")

###################################################
#Step 3: Read in the Data
####################################################
play_decision <- read.table("DTdata.csv",header=TRUE,sep=",")
play_decision
summary(play_decision)

###################################################
#Step 4: Build the Decision Tree
####################################################
fit <- rpart(Play ~ Outlook + Temperature + Humidity + Wind, method="class", data=play_decision,
    control=rpart.control(minsplit=1),parms=list(split='information'))
summary(fit)

###################################################
#Step 5: Plot the Decision Tree
####################################################
rpart.plot(fit, type=4, extra=1)


###################################################
#Step 6: Prepare Data to Test the Fitted Model
####################################################
newdata <- data.frame(Outlook="rainy",Temperature="mild",Humidity="high",Wind=FALSE)
newdata

###################################################
#Step 7: Predict a Decision from the Fitted Model
####################################################
predict(fit,newdata=newdata,type=c("prob"))
predict(fit,newdata=newdata,type=c("class"))


