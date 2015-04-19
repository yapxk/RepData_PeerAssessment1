# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
setwd("C:/Users/Skynet/Documents")
activity <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?

```r
#Calculate the total number of steps taken per day
#If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

totalstepsperday <- tapply(activity$steps, activity$date, sum)
hist(totalstepsperday, main = "Total number of steps per day", 
    xlab = "Number of steps per day", ylab = "Interval", col = "blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
#Calculate and report the mean and median of the total number of steps taken per day

#Mean total number of steps taken per day:
mean(totalstepsperday, na.rm = T)
```

```
## [1] 10766.19
```

```r
#Median total number of steps taken per day:
median(totalstepsperday, na.rm = T)
```

```
## [1] 10765
```

## What is the average daily activity pattern?


```r
#Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

steps.interval <- aggregate(steps ~ interval, data = activity, FUN = mean)
plot(steps.interval, type = "l", main = ("Average daily activity"), 
    ylab = "Interval", xlab ="Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

#Interval with the maximum number of steps:
steps.interval <- aggregate(steps ~ interval, data = activity, FUN = mean)
steps.interval$interval[which.max(steps.interval$steps)]
```

```
## [1] 835
```

## Imputing missing values

Total missing values in dataset:

```r
sum(as.numeric(is.na(activity$steps)))
```

```
## [1] 2304
```

```r
# Method for filling in all of the missing values in the dataset.
# Using the average of 5-minute intervals as fillers for missing values.

activity <- merge(activity, steps.interval, by = "interval", suffixes = c("", 
    ".y"))
nas <- is.na(activity$steps)
activity$steps[nas] <- activity$steps.y[nas]
activity <- activity[, c(1:3)]

#Histogram of the total number of steps taken each day
steps.date <- aggregate(steps ~ date, data = activity, FUN = sum)
hist(steps.date$steps, main = "Total number of steps per day", 
   xlab = "Number of steps per day", ylab = "Interval", col = "blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
#Mean total number of steps taken per day: 
mean(steps.date$steps)
```

```
## [1] 10766.19
```

```r
#Median total number of steps taken per day:
median(steps.date$steps)
```

```
## [1] 10766.19
```

The mean values are identical except the median which has a difference of 1 from the estimates from the first part of the assignment

The impact of the missing data on histogram is that the number of steps in the middle of histogram has increased as the missing data has been filled with new data with the mean.


## Are there differences in activity patterns between weekdays and weekends?


```r
# Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

days <- weekdays(as.Date(activity$date)) %in% c('Saturday','Sunday')
# Create the factor variable in the dataset in 2 levels
activity$weekday <- factor(days, labels = c("weekday", "weekend"))

# Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
par(mfrow = c(2, 1))
for (type in c("weekday", "weekend")) {
    steps.type <- aggregate(steps ~ interval, data = activity, subset = activity$weekday == 
        type, FUN = mean)
    par(mai = c(0, 1, 1, 0))
    plot(steps.type, type = "l", main = type)
}
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

On weekdays, it's apparant that more activities occurs in the morning period due to working lifestyle while on the weekends, the activities are more equally distributed throughout the day perhaps due to having more free time or non working days.

