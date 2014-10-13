# Module 4 Lab 3: Linear Regression
# from "Data Science and Big Data Analytics" 

###################################################
# Step 2: Generate the OLS Model
###################################################
setwd("~/LAB06")
x <- runif(100, 0, 10)  #This generates random numbers to model
y <- 5 + 6*x + rnorm(100)               

#Plot it
plot (x,y)

###################################################
# Step 4: OLS model
###################################################
d <- lm(y ~ x)

# Learn about this object 
help(lm)

###################################################
# Step 5: Compact model results
###################################################
print(d)

# Pretty graphics for regression diagnostics --
par(mfrow=c(2,2))
plot(d)
ypred <- predict(d)
par(mfrow=c(1,1))
plot(y,y, type="l", xlab="true y", ylab="predicted y")
points(y, ypred)

###################################################
#Generate Summary Outputs
###################################################

# Detailed model results --
d1<-summary(d)
print(d1)

# Learn about this object by saying ?summary.lm and by saying str(d)
cat("OLS gave slope of ", d1$coefficients[2,1], "and an R-sqr of ", d1$r.squared, "\n")

###################################################
#Step 7: Introduce a slight nonlinearity
###################################################
x1 <- runif(100) # as before
y1 = 5 + 6*x1 + 0.1*x1*x1 + rnorm(100)

#Put x1 and y1 into a data fraom and plot it
fitdata<-data.frame(x1,y1)
names(fitdata)<-c("X", "Y")
plot(fitdata)

###################################################
# Step 8: OLS model
###################################################
d2 <- lm(Y ~ ., data = fitdata)
print(d2)
summary(d2) #How have the R-squared and coeffcients changed?

###################################################
#Step 9: Make predictions 
###################################################
x2 <- runif(20)
predictdata<-data.frame(x2)
names(predictdata)<-c("X")
y2 <- predict(d2, newdata=predictdata)
plot(x2,y2) #How do these predictions look?