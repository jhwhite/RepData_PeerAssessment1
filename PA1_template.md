---
title: 'Reproducible Research: Peer Assessment 1'
author: "James White"
date: "July 19, 2014"
output: html_document
---

## Loading and preprocessing the data

```r
library(ggplot2)
library(plyr)
library(lubridate)

fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

download.file(fileUrl, destfile="./activity.zip", method="curl")

# unzips the downloaded file, if the file already exists will overwrite
unzip("activity.zip", overwrite=TRUE)

# reads in the csv and puts it in data frame called data
data <- read.csv("activity.csv")

# removes the NAs and stores in new data frame
data.no.na <- na.omit(data)

# gets total number of steps taken per day
steps_taken = ddply(data.no.na, .(date), summarize, steps.taken = sum(steps))
```

## What is mean total number of steps taken per day?

Creates histogram for steps taken

```r
steps_hist <- ggplot(steps_taken, aes(x=steps.taken))
steps_hist + geom_histogram()
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

Calculates mean

```r
mean(steps_taken$steps.taken)
```

```
## [1] 10766
```
Calculates median

```r
median(steps_taken$steps.taken)
```

```
## [1] 10765
```
## What is the average daily activity pattern?

```r
avg.daily.activity <- aggregate(.~interval, FUN=mean, data=data)

plot(avg.daily.activity$interval, avg.daily.activity$steps, type="l")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

Get the interval that has the highest amount of steps

```r
subset(avg.daily.activity, steps==max(avg.daily.activity$steps))
```

```
##     interval steps  date
## 104      835 206.2 30.72
```
Interval number 835 is the 5-minute interval with the most steps

## Imputing missing values

This will calculate the total number of NAs.

```r
sum(is.na(data))
```

```
## [1] 2304
```
Strategy for filling in all of the missing values in the dataset.

```r
# technique to replace NA with mean by subset in R and the impute.mean function 
# described at http://stackoverflow.com/a/9322975/3657371
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))

# create a new dataset that is equal to the original dataset, but with the 
# missing data filled in
# original dataset is first three variables of the [activity] dataframe
activity.imputed <- plyr::ddply(data[1:3], .(interval), transform,
                                steps = impute.mean(steps),
                                date = date,
                                interval = interval)
# sort by date and interval
activity.imputed <- activity.imputed[order(activity.imputed$date,
                                           activity.imputed$interval),]

# renumber rownames
row.names(activity.imputed) <- 1:nrow(activity.imputed)
```


```r
steps_taken_imputed = ddply(activity.imputed, .(date), summarize, steps.taken.imputed = sum(steps))

steps_imputed <- ggplot(steps_taken_imputed, aes(x=steps.taken.imputed))
steps_imputed + geom_histogram()
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 


```r
mean(steps_taken_imputed$steps.taken.imputed)
```

```
## [1] 10766
```

```r
median(steps_taken_imputed$steps.taken.imputed)
```

```
## [1] 10766
```

For the most part it doesn't seem the values differ from the previous estimates for mean and median. Nor was there a significant impact by replacing the NAs with the means.

## Are there differences in activity patterns between weekdays and weekends?

```r
activity.imputed["day.of.week"] <- wday(as.Date(activity.imputed$date))

activity.imputed.weekday <- subset(activity.imputed, activity.imputed$day.of.week == 2 | activity.imputed$day.of.week == 3 | activity.imputed$day.of.week == 4 | activity.imputed$day.of.week == 5 | activity.imputed$day.of.week == 6 )
activity.imputed.weekend <- subset(activity.imputed, activity.imputed$day.of.week == 1 | activity.imputed$day.of.week == 7)

activity.imputed.weekday.avg <- aggregate(.~interval, FUN=mean, data=activity.imputed.weekday)
activity.imputed.weekend.avg <- aggregate(.~interval, FUN=mean, data=activity.imputed.weekend)

par(mfrow=c(2,1)) 
plot(activity.imputed.weekend.avg$interval, activity.imputed.weekend.avg$steps, type="l")
title(main="Weekends")
plot(activity.imputed.weekday.avg$interval, activity.imputed.weekday.avg$steps, type="l")
title(main="Weekdays")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 
