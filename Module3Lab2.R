# Module 3, Lab 1
# from "Data Science and Big Data Analytics" 

###################################################
# Step 1: Start R and Read the Data Set Back Into Your Workspace
###################################################
setwd("~/LAB01")
options(digits=3) #What is this line doing?
ls()
load(file="Labs.Rdata")
ls()

rm(lab2)
ds <- lab1
colnames(ds) <- c("income", "rooms") 

###################################################
# Step 2: Examine Household Income
###################################################
summary(ds$income)
range(ds$income)
sd(ds$income)
var(ds$income)
plot(density(ds$income))  # right skewed

###################################################
#Step 3:  Examine the Number of Rooms
###################################################
summary(ds$rooms)
range(ds$rooms)
sd(ds$rooms)
plot(as.factor(ds$rooms))

###################################################
# Step 4: Removing Outliers
###################################################
(m <- mean(ds$income, trim=0.10) )

ds <- subset(ds, ds$income  >= 10000 & ds$income < 1000000)
summary(ds)
quantile(ds$income, seq(from=0, to=1, length=11))


###################################################
# Step 5: Stratifying a Variable
###################################################
breaks <- c(0, 23000, 52000, 82000, 250000, 999999)
labels <- c("Poverty", "LowerMid", "UpperMid", "Wealthy", "Rich") 
wealth <- cut(ds$income, breaks, labels)
#Add wealth as a column to ds 
ds <- cbind(ds, wealth)
#Show the 1st few lines.
head(ds)

wt <- table(wealth)
percent <- wt/sum(wt)*100
wt <- rbind(wt, percent)
wt
plot(wt)    #This does not seem to give good results, why?

nt <- table(wealth, ds$rooms)
print(nt)
plot(nt)   #Nice mosaic plot     

rm(wealth,breaks,labels)
save(ds, wt, nt, file="Census.Rdata")

###################################################
# Step 6: Histograms and Distributions 
###################################################    
library(MASS) #What is this doing?
with(ds, {
  hist(income, main="Distribution of Household Income",   freq=FALSE)
  lines(density(income), lty=2, lwd=2)
  xvals = seq(from=min(income), to=max(income),length=100)
  param = fitdistr(income, "lognormal")
  lines(xvals, dlnorm(xvals, meanlog=param$estimate[1],
          sdlog=param$estimate[2]), col="blue")
} )


#Now try the same thing with log10(income)
logincome = log10(ds$income)
hist(logincome, main="Distribution of Household Income", freq=FALSE)
lines(density(logincome), lty=2, lwd=2)  # line type (lty) 2 is dashed
xvals = seq(from=min(logincome), to=max(logincome), length=100)
param = fitdistr(logincome, "normal")
lines(xvals, dnorm(xvals, param$estimate[1],  param$estimate[2]), 
         lwd=2, col="blue")

###################################################
# Step 7: Correlation
###################################################
with(ds, cor(income, rooms))
with(ds, cor(log(income), rooms) ) #This will give a better correlation
n = length(ds$income)
with(ds, cor(runif(n), rooms)) 

###################################################
# Step 8: Plotting
###################################################
with(ds, 
  boxplot(income ~ as.factor(rooms), data=ds, range=0, outline=F, log="y",
          xlab="# rooms", ylab="Income")
)

with( ds, 
boxplot(rooms ~ wealth, data = ds, main="Room by Wealth", Xlab="Category",
        ylab="# rooms")
) 

###################################################
# Step 9: Exit R
###################################################
#If time permits, please continue to Part 2 and skip the following line
q()


###################################################
# Lab 2 Part 2 – Graphics Package Plots and Hypothesis Tests
###################################################


###################################################
#Part 1: ANOVA 
###################################################

###################################################
#Step 2: Generate the data 
###################################################
offers = sample(c("noffer", "offer1", "offer2"), size=500, replace=T)
head(offers)
purchasesize = ifelse(offers=="noffer", rlnorm(500, meanlog=log(25)), ifelse(offers=="offer1", rlnorm(500, meanlog=log(50)), rlnorm(500, meanlog=log(55))))
head(purchasesize)
offertest = data.frame(offer=offers, purchase_amt=purchasesize)

###################################################
#Step 3: Examine the data. 
###################################################
summary(offertest)
aggregate(x=offertest$purchase_amt, by=list(offertest$offer), FUN="mean")

###################################################
#Plot how purchase size varies within the three groups. The log="y" argument plots the y
#axis on the log scale. Does it appear that making offers increases purchase amount?
boxplot(purchase_amt ~ as.factor(offers), data=offertest, log="y") 

###################################################
#Step 5: Use lm() to do the ANOVA
###################################################
model = lm(log10(purchase_amt) ~ as.factor(offers), data=offertest)
summary(model)

###################################################
#Step 6: Use Tukey?s test to check all the differences of means.
###################################################
TukeyHSD(aov(model))

###################################################
#Step 7: Plotting with ggplot() and lattice() 
###################################################
library(lattice)
densityplot(~ purchase_amt, group=offers, data=offertest, auto.key=T)

#Because the data is so left-skewed, we may want to plot the logged data to see more clearly the # differences in the distributions, and the different locations of the modes.
densityplot(~ log10(purchase_amt), group=offers, data=offertest, auto.key=T)
densityplot(~purchase_amt | offers, data=offertest)
densityplot(~log10(purchase_amt) | offers, data=offertest)

###################################################
#Step 10: The ggplot() function
###################################################
library(ggplot2)
ggplot(data=offertest, aes(x=as.factor(offers), y=purchase_amt)) +  
  geom_point(position="jitter", alpha=0.2) +  
  geom_boxplot(alpha=0.1, outlier.size=0) +   
  scale_y_log10()

# You need to plot at least one geom_... to get a graph. 
# Try adding and removing the different lines of the graphing command 
# to create simpler scatterplots or box-and-whisker plots, with and 
# without log scaling.
# Here's how you would create the densityplots that you created in 
# lattice:

ggplot(data=offertest) + geom_density(aes(x=purchase_amt, 
      colour=as.factor(offers))) 
ggplot(data=offertest) + geom_density(aes(x=purchase_amt, 
     colour=as.factor(offers))) + scale_x_log10()

#
# Section II. Doing a Hypothesis Test by Hand
#
###################################################
# Step 11: Generate the example data.
###################################################
x = rnorm(10) # distribution centered at 0
y = rnorm(10,2) # distribution centered at 2

###################################################
# Step 12: Create a function to calculate the pooled variance,
# which is used in the Student's t statistic
###################################################
pooled.var = function(x, y) {
  nx = length(x)
  ny = length(y)
  stdx = sd(x)
  stdy = sd(y)
  num = (nx-1)*stdx^2 + (ny-1)*stdy^2
  denom = nx+ny-2  # degrees of freedom
  (num/denom) * (1/nx + 1/ny)
}

###################################################
# Step 13: Examine the data
###################################################
mx = mean(x)
my = mean(y)
mx - my
pooled.var(x,y)

###################################################
# Step 14: Calculate the t statistic for student's t-test
###################################################
tstat = (mean(x) - mean(y))/sqrt(pooled.var(x,y))
tstat

###################################################
#Step 15: Calculate the degrees of freedom for out problem.
###################################################
dof = length(x) + length(y) - 2 
dof

###################################################
# Step 16: The function pt(x, dof) gives the area under the curve 
###################################################
tailarea = pt(tstat, dof)
pvalue = 2 * tailarea

###################################################
# Step 17: Do Student?s t-test directly, and compare the results
###################################################
t.test(x, y, var.equal=T)

# Does t.test() give the same results?
q()
