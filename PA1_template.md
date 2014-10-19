# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
Sorry if my english is not good. 
Unzip the file in your working directory and load with R:

```r
library("lubridate")
data<-read.csv("activity.csv")
data$date<-ymd(data$date)

#steps per day

steps_day<-aggregate(steps~date,data,sum,na.rm=TRUE)
```




## What is mean total number of steps taken per day?

making a histogram for steps per day


```r
summary(steps_day$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10760   10770   13290   21190
```

```r
hist(steps_day$steps, col='green', breaks=10)
mean_step<-mean(steps_day$steps)
median_step<-median(steps_day$steps)
abline(v=mean_step, col='red', lwd=4) #mean of steps per day
```

![](./PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

* The mean of steps per day is 10766
* The median of the steps per day is: 10765


## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
