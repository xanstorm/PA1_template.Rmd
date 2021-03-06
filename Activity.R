setwd("/Users/stephenhobbs1/Data science/5-Reproducible Research")
# Question 1
data <- read.csv("activity.csv", stringsAsFactors=FALSE, header = TRUE)
# Load plyr because it will be used later.
library(plyr)
#Summarize dataframe by date, total steps
tmp<-ddply(data,.(date),summarise, totalSteps=sum(steps, na.rm=TRUE)) 
#Question 2
hist(tmp$totalSteps, breaks = 10, xlab = "Total Steps",
     main = "Histogram of Total Steps Taken Each Day")

#Question 3
mean(tmp$totalSteps, na.rm=TRUE) # 9354 is average steps.
median(tmp$totalSteps, na.rm=TRUE) # 10,395 is median steps.
#Question 4
sum(is.na(data)) # Code shows number of "NA" values, 2304 "NA" values.
tmp2<-ddply(data,.(interval),summarise, AverageSteps=mean(steps, na.rm=TRUE)) #summarize df by date, average steps/interval
plot(tmp2, type="l", main = "Average steps per interval", ylab="Average Steps", xlab="Interval, minute units")
#Question 5 - Answer is 206.17 steps.
tmp2[which.max(tmp2$AverageSteps),]
#Question 6 - Replacing missing NA value with average of average of 5 min valuues
datanew<-merge(data, tmp2, by="interval", sort=TRUE)
index <- is.na(datanew$steps) #Returns logical vector, true = missing value
#Take out rows of missing values and replace with AverageSteps where there was NA, missing value
datanew$steps[index] <- datanew$AverageSteps[index] 
tmp3<-ddply(datanew, .(date), summarise, totalsteps=sum(steps, na.rm = TRUE)) #Group on date, total steps
#Question 7
hist(tmp3$totalsteps, breaks=10, main = "Histogram of total steps taken each day, Impute NA", xlab="Total Steps") #Histogram with imputed missing values

mean(tmp3$totalsteps) # Imputed mean of steps, NA removed. Average steps = 10,766.19 steps
median(tmp3$totalsteps) # Imputed median of steps, NA removed. Median steps  = 10,766.19

#Question 8
 # Make dates into weekdays and weekends
tmp4<-transform(data, date = strptime(date, format="%Y-%m-%d"))
weekdays(tmp4$date) %in% c("Saturday", "Sunday") # Separate days of week, Sat-Sun. Return log vector, Sat-Sun=True, M-F=false
# Add factor variable
factor(weekdays(tmp4$date) %in% c("Saturday", "Sunday"), level=c(TRUE, FALSE), labels=c("weekend", "weekday"))
# Assign factor variable to day
day1<-factor(weekdays(tmp4$date) %in% c("Saturday", "Sunday"), level=c(TRUE, FALSE), labels=c("weekend", "weekday"))
#Create new column, weekend, weekend using mutate from plyr
tmp4<-mutate(tmp4, day=day1)
#Plot weekend and weekday averages
tmp4<-transform(tmp4, day=as.character((day)))  # Change day from POSIXlt format to character format bc ddply does not work on posixlt; only works on char & num vector
tmp5<-ddply(tmp4,.(interval,day), summarize, averagesteps=mean(steps, na.rm = TRUE))
xyplot(averagesteps~interval|day, data = tmp5, 
       ylab="Average Steps", 
       type = "l",
       main="Comparison of average steps between weekday and weekend")
#Weekend average is higher on average but max value was during weekday.

