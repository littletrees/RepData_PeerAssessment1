setwd("/Users/littletrees/Documents/coursera_r")

#Loading the data
activity <- read.csv("activity.csv")

#transforming the variable "date" to Date type varialbe
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")

#Calculate mean of total number of steps taken per day
DailyTotalSteps <- aggregate(steps ~ date, activity, sum)
hist(DailyTotalSteps$steps, xlab = "Number of Steps", main = "Total number of steps taken per day", col = "red")
MeanTotalSteps <- mean(DailyTotalSteps$steps)
MeanTotalSteps
MedianTotalSteps <- median(DailyTotalSteps$steps)
MedianTotalSteps
dev.copy(png, file="MeanTotalSteps.png", height=480, width=480)
dev.off()

#Calculate the average daily activity pattern
#calculate the average steps at the same time point each day all months
MeanDailyInterval <- aggregate(steps ~ interval, activity, mean)
plot(MeanDailyInterval$interval, MeanDailyInterval$steps, type = "l", xlab = "Daily 5-min interval", ylab = "number of steps", main = "Average number of steps per 5-min interval")
MeanDailyInterval[which.max(MeanDailyInterval$steps),]
dev.copy(png, file="MeanDailyInterval.png", height=480, width=480)
dev.off()



#Imputing missing values
#calculating the total number of missing values in the dataset
MissingRows <- sum(!complete.cases(activity))
MissingRows

#creating a new dataset
MeanDailyIntervalList <- as.character(MeanDailyInterval$steps)
names(MeanDailyIntervalList) <- MeanDailyInterval$interval
activityXNa <- activity
naSelect <- is.na(activity$steps)
activityXNa$steps[naSelect] <- as.numeric(MeanDailyIntervalList[as.character(activity$interval[naSelect])])

#creating a histogram of the total number of steps taken each day using the new dataset
DailyTotalStepsXNa <- aggregate(steps ~ date, activityXNa, sum)
hist(DailyTotalStepsXNa$steps, xlab = "Number of Steps", main = "Total number of steps taken per day", col = "red")
MeanTotalStepsXNa <- mean(DailyTotalStepsXNa$steps)
MeanTotalStepsXNa
MedianTotalStepsXNa <- median(DailyTotalStepsXNa$steps)
MedianTotalStepsXNa
dev.copy(png, file="MeanTotalStepsXNa.png", height=480, width=480)
dev.off()

#create a new variable
activityXNa$days <- weekdays(activityXNa$date)
weekendSelect <- activityXNa$days == 'Sunday' | activityXNa$days == 'Saturday'
weekdaySelect <- activityXNa$days == 'Monday' | activityXNa$days == 'Tuesday' | activityXNa$days == 'Wednesday' | activityXNa$days == 'Thursday' | activityXNa$days == 'Friday'
activityXNa$days[weekendSelect] <- 'weekend'
activityXNa$days[weekdaySelect] <- 'weekday'
daysnew <- as.factor(activityXNa$days)

#creating a 2 panel plots
MeanDailyInterval2 <- aggregate(steps ~ interval+daysnew, activityXNa, mean)
MeanDailyInterval2
library(ggplot2)
z <-ggplot(MeanDailyInterval2, aes(x = interval, y = steps, color = daysnew)) + geom_line() + facet_wrap(~daysnew, ncol = 1, nrow = 2)
print(z)
ggsave("MeanDailyInterval2.png")