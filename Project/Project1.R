#Project 1 - Reproducible Research
#Scott Davis

#Step 1 - import and pre-process data

#Import Data 
Sensor <- read.csv('activity.csv')
#Convert csv to dataframe
Sensor_activity <- data.frame(Sensor)

#Data preprocessing
#Datatypes
str(Sensor_activity)
#Summary statistics
summary(Sensor_activity)

#Step 2 - histogram of total number of steps taken each day 
#Group by date and take total
steps_calc <- aggregate(Sensor_activity$steps, by = list(Sensor_activity$date), FUN = 'sum')
#Histogram total steps per day
hist(steps_calc$x, main = "Histogram of total steps per day", xlab = "total steps", 
     ylim = c(0,30), col = "green")

#Step 3 - mean and median of steps taken each day 
summary(steps_calc$x)

#Step 4 - time series plot of average activity pattern
#Group by interval and take average
steps_calc_avg <- aggregate(steps ~ interval, Sensor_activity, mean)
#Plot of average steps per day
plot(steps_calc_avg$interval, steps_calc_avg$steps, type = "l", main = "Average activity patterns",
         xlab = "Interval", ylab = "Steps") 

#Step 5 - maximum number of steps in 5min interval across daily average
steps_calc_avg[which.max(steps_calc_avg$steps), 1]

#Step 6 - impute missing values
#Number of missing values in steps variable
sum(is.na(Sensor_activity$steps))

#Imputation of data using MICE package
library(mice)
#Apply MICE algorithm to NA dataset
imputed_sensor <- mice(Sensor_activity, m = 5, maxit = 50, method = "pmm", seed = 5)
#Get completed dataset
completeData <- complete(imputed_sensor, 4)

#Step 7 - histogram of total number of steps per day after imputation
#Histogram of total number of steps per day
na_steps_calc <- aggregate(completeData$steps, by = list(completeData$date), FUN = 'sum')
#Histogram total steps per day
hist(na_steps_calc$x, main = "Histogram of total steps per day", xlab = "total steps", 
     ylim = c(0,30), col = "green")

#Mean and median of imputed dataset
summary(na_steps_calc$x)

#Step 8 - Compare average number of steps per interval across weekdays and weekends
#Install timeDate package
library(timeDate)

#Create new column for weekday or weekend
completeData$Weekday <- isWeekday(completeData$date)
#Average steps for weekday
weekday <- subset(completeData, completeData$Weekday == "TRUE")
weekdayMean <- aggregate(steps ~ interval, data = weekday, mean)
#Average steps for weekend
weekend <- subset(completeData, completeData$Weekday == "FALSE")
weekendMean <- aggregate(steps ~ interval, data = weekend, mean)

#Plot of weekday and weekend
#Plot of weekday
plot(weekdayMean$interval, weekdayMean$steps,
     xlab = "interval", ylab = "Number of steps",
     main = "Weekday", col = "blue", type = "l") 
#Plot of weekend
plot(weekendMean$interval, weekendMean$steps,
     xlab = "interval", ylab = "Number of steps",
     main = "Weekend", col = "red", type = "l")


