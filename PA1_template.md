# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
1. Load the data (i.e. read.csv())

```r
    activity <- read.csv("./activity.csv")
```

2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
    activity$date <- as.Date(as.character(activity$date))
```

## What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day

```r
    daily_step <-aggregate(activity$steps, by=list(activity$date), FUN=sum, na.rm= TRUE)
    names(daily_step) <- c("date","steps")
    head(daily_step)
```

```
##         date steps
## 1 2012-10-01     0
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```r
    library(ggplot2)
    g <- ggplot(daily_step, aes(steps)) + geom_histogram(bins = 30, na.rm = TRUE) 
    print (g)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day

```r
    mean(daily_step$steps, na.rm = TRUE)
```

```
## [1] 9354.23
```

```r
    median(daily_step$steps, na.rm = TRUE)
```

```
## [1] 10395
```


## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
    interval_step <-aggregate(activity$steps, by=list(activity$interval), FUN=mean, na.rm= TRUE)
    names(interval_step) <- c("interval","steps")
    ggplot(interval_step, aes(interval,steps)) + geom_line() 
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
    interval_step[which.max(interval_step$steps), ]
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
    sum(!complete.cases(activity))    
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

    I used th mice() function takes care of the imputing process.
    m=5 refers to the number of imputed datasets. Five is the default value.
    method='pmm' refers to the imputation method. In this case we are using predictive mean matching as imputation method. 

```r
    library(mice)
    tempData <- activity[, c(1,3)]
    imputingData <- mice(tempData, method="pmm", m = 5, maxit = 3, seed = 500, printFlag = FALSE)
    summary(imputingData)
```

```
## Multiply imputed data set
## Call:
## mice(data = tempData, m = 5, method = "pmm", maxit = 3, printFlag = FALSE, 
##     seed = 500)
## Number of multiple imputations:  5
## Missing cells per column:
##    steps interval 
##     2304        0 
## Imputation methods:
##    steps interval 
##    "pmm"    "pmm" 
## VisitSequence:
## steps 
##     1 
## PredictorMatrix:
##          steps interval
## steps        0        1
## interval     0        0
## Random generator seed value:  500
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
    new_activity <- activity
    new_activity$steps <- complete(imputingData,1)$steps
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
    daily_step <-aggregate(new_activity$steps, by=list(new_activity$date), FUN=sum)
    names(daily_step) <- c("date","steps")
    ggplot(daily_step, aes(steps)) + geom_histogram(bins = 30, na.rm = TRUE) 
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
    mean(daily_step$steps, na.rm = TRUE)
```

```
## [1] 10911.31
```

```r
    median(daily_step$steps, na.rm = TRUE)
```

```
## [1] 11015
```
As you can see, distribution changed especially in 0 frequency.

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels ??? “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
    library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
    wfind <- function (x) {if(wday(x) %in% c(2:6)) "weekday" else "weekend" }
    new_activity$wday <- factor(sapply(new_activity$date, wfind))
    str(new_activity)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps   : int  0 0 0 5 0 0 0 0 0 0 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ wday    : Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
    library(lattice)
   new_interval_step <-aggregate(new_activity$steps, by=list(new_activity$interval,new_activity$wday), FUN=mean, na.rm= TRUE)
    names(new_interval_step) <- c("interval","wday","steps")
    xyplot(steps ~ interval | wday, data = new_interval_step, type="l", layout = c(1, 2))
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
