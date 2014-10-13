# Module 4 Lab 1: K-means Clustering
# from "Data Science and Big Data Analytics" 
#
setwd("~/LAB04")

#First up let us load the library RODBC
library('RODBC')
ch <- odbcConnect("Greenplum",uid="gpadmin",
      case="postgresql",pwd="changeme")
sqlDrop(ch,"income_state")
sqlQuery(ch,
"CREATE TABLE income_state AS
SELECT 
   f.name AS state,  
   round(avg(h.hinc),0) AS income  
FROM
   housing AS h 
JOIN
   fips AS f
ON
   h.state = f.code
WHERE 
   (h.hinc > 0 )
GROUP
    by f.name 
    DISTRIBUTED BY (income);"
)

#Read input data from the database
sqlColumns(ch,"income_state")
income <- as.matrix(sqlFetch(ch,"income_state",
          rownames="state"))
summary(income)

#Sort the Income
income <- sort(income)

#Take a look at the k-means function 
help(kmeans)
#Fit the k-means cluster with 3 initial cluster centers
km <- kmeans (income,3,15)
km

#Plot clusters
plot(income, col = km$cluster)

#Plot centers
points(km$centers, col = 1:3, pch = 8)

#Plot the within-group-sum of squares and 
#look for an "elbow" of the plot. The elbow 
#(if you can find one) tells you what the 
#appropriate number of clusters probably is.
wss <- numeric(15) 
for (i in 1:15) wss[i] <- sum(kmeans(income, 
     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
  ylab="Within groups sum of squares")
#What do you notice from this graph?

odbcClose(ch)