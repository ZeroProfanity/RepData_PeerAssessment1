# Setting the working directory
setwd("~/DataScienceCourse/RepData_PeerAssessment1/")

# Load library# Preprocessing the data
activity <- read.csv("activity.csv", stringsAsFactors = FALSE)
activity$date <- as.Date(strptime(activity$date, "%Y-%m-%d")) 

# Calculating the total steps per day 
# Histogram
# Calculate the mean and median steps per day
steps.per.day <- aggregate(activity$steps, by = list(Date = activity$date), FUN = sum)
steps.per.day
hist(steps.per.day$x, breaks = 16, xlab = "Total steps per day", main = "Total steps per day")
mean(steps.per.day$x, na.rm = TRUE)
median(steps.per.day$x, na.rm = TRUE)

# Calculate the mean steps per time interval
steps.per.interval <- aggregate(activity$steps, by = list(Interval = activity$interval), FUN = mean, na.rm = TRUE)
plot(y = steps.per.interval$x, x = steps.per.interval$Interval, type = "l", 
     xlab = "Interval", ylab = "Mean number of steps", main = "Mean amount of steps per 5 minute interval")
steps.per.interval[which.max(steps.per.interval$x),]$Interval

# Calculate the number of NAs in the data set
sum(is.na(activity$steps))

# Replace missing time intervals with interval mean
sum(is.na(activity$steps))
activity.no.na <- activity
missing.entries <- is.na(activity$steps)
missing.intervals <- activity.no.na[is.na(activity$steps),]$interval
myfunc <- function(x) {steps.per.interval[steps.per.interval$Interval == x, "x"]}
missing.steps <- sapply(missing.intervals, myfunc)
activity.no.na[is.na(activity$steps),]$steps <- missing.steps

# Make a histogram and calculate mean and median of the data set without missing values
# Histogram
steps.per.day.no.na <- aggregate(activity.no.na$steps, by = list(Date = activity.no.na$date), FUN = sum)
hist(steps.per.day.no.na$x, breaks = 16, xlab = "Total steps per day", main = "Total steps per day")
mean(steps.per.day.no.na$x, na.rm = TRUE)
median(steps.per.day.no.na$x, na.rm = TRUE)

# Activities in weekend and during weekdays
library(ggplot2)
Sys.setlocale("LC_TIME", "English")
activity.no.na$in.weekend <- as.factor(weekdays(activity.no.na$date) %in% c("Saturday", "Sunday"))
levels(activity.no.na$in.weekend) <- c("Weekday","Weekend")
steps.per.interval.no.na <- aggregate(activity.no.na$steps, 
                                by = list(Interval = activity.no.na$interval, 
                                          In.Weekend = activity.no.na$in.weekend), 
                                FUN = mean, 
                                na.rm = TRUE)
g <- ggplot(data = steps.per.interval.no.na, aes(x = Interval, y = x))
g + geom_point() + facet_grid(In.Weekend~.) + ylab("Steps")
 