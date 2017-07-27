---
title: "Reproducible research course project 1"
author: "Sreeya Sreevatsa"
date: "July 27, 2017"
output: html_document

## Loading and preprocessing the data
```{r echo = TRUE}
data <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?
```{r echo = TRUE}
library(ggplot2)
total.steps <- tapply(data$steps, data$date, FUN = sum, na.rm = TRUE)
qplot(total.steps, binwidth = 1000, xlab = "total number of steps taken each day")
```

##### Calculate and report the mean and median of the total number of steps taken per day
```{r echo=TRUE}
mean(total.steps, na.rm = TRUE)
median(total.steps, na.rm = TRUE)
```

## What is the average daily activity pattern?
```{r echo=TRUE}
library(ggplot2)
averages <- aggregate(x = list(steps = data$steps), by = list(interval = data$interval), 
    FUN = mean, na.rm = TRUE)
ggplot(data = averages, aes(x = interval, y = steps)) + geom_line() + xlab("5-minute interval") + 
    ylab("average number of steps taken")
```

##### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r echo=TRUE}
averages[which.max(averages$steps), ]
```

## Imputing missing values
##### Calculate and report the total number of missing values in the dataset 
```{r echo=TRUE}
missing <- sum(is.na(data$steps))
missing
```
##### Devise a strategy for filling in all of the missing values in the dataset. 
```{r echo=TRUE}
fill.value <- function(steps, interval) {
    filled <- NA
    if (!is.na(steps))
        filled <- c(steps)
    else
        filled <- (averages[averages$interval==interval, "steps"])
    return(filled)
}
```

##### Create a new dataset that is equal to the original dataset but with the missing data filled in.
```{r echo=TRUE}
filled.data <- data
filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)
```

##### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.
```{r echo=TRUE}
total.steps <- tapply(filled.data$steps, filled.data$date, FUN=sum)
qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")
mean(total.steps)
median(total.steps)
```
######## The mean and median increased because the missing data have a value now. Before imputing, these missing values were considered zero.

## Are there differences in activity patterns between weekdays and weekends?
```{r echo=TRUE}
weekday.or.weekend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else if (day %in% c("Saturday", "Sunday"))
        return("weekend")
    else
        stop("invalid date")
}
filled.data$date <- as.Date(filled.data$date)
filled.data$day <- sapply(filled.data$date, FUN=weekday.or.weekend)
```
##### Make a panel plot containing a time series plot
```{r echo=TRUE}
averages <- aggregate(steps ~ interval + day, data=filled.data, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) + facet_wrap(~day,ncol=1) +
    xlab("5-minute interval") + ylab("Number of steps")
```    