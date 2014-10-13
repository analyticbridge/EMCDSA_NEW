# Module 4 Lab 7: ARIMA
# from "Data Science and Big Data Analytics" 

###################################################
#Step 2: Set the Working Directory and Install Packages
###################################################
setwd("~/LAB10")
library('RODBC')

###################################################
#Step 3-5: Read input data from the database
###################################################

ch <- odbcConnect("Greenplum",uid="gpadmin",
      case="postgresql",pwd="changeme")
sqlDrop(ch,"ddemo.weekly_sales")
sqlQuery(ch,
"CREATE TABLE
     ddemo.weekly_sales (
       sale INTEGER,
       Y1 INTEGER,
       W1 INTEGER )
       DISTRIBUTED BY (sale);
INSERT INTO ddemo.weekly_sales 
SELECT 
   SUM((o.item_price*o.item_quantity)) as sale ,  
   EXTRACT(YEAR FROM o.order_datetime) as y1, 
   CASE 
     WHEN (EXTRACT(WEEK FROM o.order_datetime) = 53) THEN 52
     ELSE
     EXTRACT(WEEK FROM o.order_datetime)
   END w1
FROM  
   ddemo.order_lineitems o
group 
   by y1,w1
;       
"  
)
msales <- (sqlFetch(ch,"ddemo.weekly_sales"))
odbcClose(ch)

###################################################
#Step 6: Review, Update and Prepare DataFrame ”sales” for ARIMA Modeling
###################################################
attach(msales)
msales <- msales[order(y1,w1),]
detach(msales)

# Let us just take 300 observations for the model and the remaining 
# 12 for predicting purposes
sales <- c(rep(0,300))
csales <- c(rep(0,12))
sales[1:300] <- msales[1:300,1]
csales[1:12] <- msales[301:312,1]

###################################################
#Step 7: Convert “sales” into Time Series Type Data
###################################################
sales <- ts(sales,start=2005,frequency=52)

###################################################
#Step 8: Plot the Time Series
###################################################
plot(sales,type="l")

###################################################
#Step 9-11: Analyze the ACF and PACF
###################################################
acf(sales)
pacf(sales)

#Difference the series and plot it
sales1 <- diff(sales)
m <- length(sales1)
par(mfrow=c(1,1))
plot(1:m,sales1,type="l")

#Replot ACF and PACF on the same graph
par(mfrow=c(1,1))
acf(sales1)
pacf(sales1)

###################################################
#Step 12: Fit the ARIMA Model
###################################################
sales.fit <- arima (sales,
                    order=c(1,1,0), 
                    seasonal = list(order=c(1,1,0),period=52),
                    include.mean=FALSE)
sales.fit

###################################################
#Step 13: Generate Predictions
###################################################
sales.predict <- predict (sales.fit, n.ahead=12)
plot (sales,xlim=c(2010,2011), ylim=c(800000,2100000))
lines (sales.predict$pred,col="blue")
lines (sales.predict$pred+2*sales.predict$se,col="red")
lines (sales.predict$pred-2*sales.predict$se,col="red")

###################################################
#Step 15: Comparing the predictions with the actual values
###################################################
x <- c(rep(0,24))
x[1:12] <- csales[1:12]
x[13:24] <- as.numeric(sales.predict$pred)
forbar <- matrix(x,ncol=12,byrow=TRUE)
colnames(forbar)<- msales[1:12,3]
rownames(forbar)<- c("Actual","Predicted")
barplot(forbar,beside=TRUE,
        main="Actual Vs Predicted",
        ylab="Weekly Sales",
        col=rainbow(2),
        xlab="Weeks 2010")