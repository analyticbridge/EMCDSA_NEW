# Module 4 Lab 4: Logistic Regression
# from "Data Science and Big Data Analytics" 

###################################################
# Step 2: Set the Working Directory
###################################################
setwd("~/LAB07")

###################################################
# Step 4: Read in and Examine the Data
###################################################
Mydata <- read.csv("survey.csv",header=TRUE,sep=",")

#Explore data
table(Mydata$MYDEPV)
with(Mydata, table(Price,MYDEPV))
summary(Mydata$Age)
cor.mat <- cor(Mydata[,-1])
cor.mat

###################################################
#Step 5: Build and Review the Logistic Regression Model
###################################################
mylogit <- glm(MYDEPV ~ Income + Age + as.factor(Price),
           data =Mydata, family=binomial(link="logit"),
           na.action=na.pass)
summary(mylogit)

###################################################
#Step 6: Review the Results and Interpret the Coefficients
###################################################
confint(mylogit)
exp(mylogit$coefficients)

# Compute Pseudo R-squared
attributes(mylogit)  # get me the names of the 'class members'
1- with(mylogit, deviance/null.deviance)

###################################################
#Step 8: Relevel
###################################################
#We will now change the refernce level at price point 30

Mydata$pricefactor = relevel(as.factor(Mydata$Price), "30")

mylogit2 = glm(MYDEPV ~ Income + Age + pricefactor  ,
            data= Mydata,family=binomial(link="logit"), na.action=na.pass)
summary(mylogit2)

###################################################
#Step 9: Plot the ROC Curve
###################################################
install.packages("bitops")
library(bitops)
install.packages("caTools")
library(caTools)
install.packages("ROCR")
library(ROCR)

pred = predict(mylogit, type="response") # this returns the probability scores on the training data
predObj = prediction(pred, Mydata$MYDEPV) # prediction object needed by ROCR
rocObj = performance(predObj, measure="tpr", x.measure="fpr")  # creates ROC curve obj
aucObj = performance(predObj, measure="auc")  # auc object
auc = aucObj@y.values[[1]]  
auc   # the auc score

# plot the roc curve
plot(rocObj, main = paste("Area under the curve:", auc))

###################################################
#Step 10: Predict Outcome given Age and Income
###################################################
Price <- c(10,20,30)
Age <- c(mean(Mydata$Age))
Income <- c(mean(Mydata$Income))
newdata1 <- data.frame(Income,Age,Price)
newdata1
newdata1$PurchaseP <- predict (mylogit,newdata=newdata1,type="response")
newdata1

###################################################
#Step 11: Predict Outcome for a Sequence of Age Values 
#         at price30 and Mean Income
###################################################
newdata2 <- data.frame(Age=seq(min(Mydata$Age),max(Mydata$Age),2),
                       Income=mean(Mydata$Income),Price=30)
newdata2$AgeP<-predict(mylogit,newdata=newdata2,type="response")
cbind(newdata2$Age,newdata2$AgeP)
plot(newdata2$Age,newdata2$AgeP)

###################################################
#Step 12: Predict Outcome for a Sequence of Income 
#         at price30 and Age at its mean
###################################################
newdata3 <- data.frame(Income= seq(20,90,10),Age=mean(Mydata$Age),Price=30)
newdata3$IncomeP<-predict(mylogit,newdata=newdata3,type="response")
cbind(newdata3$Income,newdata3$IncomeP)
plot(newdata3$Income,newdata3$IncomeP)

###################################################
#Step 14: Use Logistic Regression as a Classifier
###################################################
newdata4 <- data.frame (Age= round(runif(10,min(Mydata$Age),max(Mydata$Age))), 
                        Income= round(runif(10,min(Mydata$Income),max(Mydata$Income))),
                        Price = round((runif(10,10,30)/10))*10)
newdata4$Prob <- predict(mylogit,newdata=newdata4,type="response")
newdata4

