# Reproducible Research: Peer Assessment 1

## Data

The variables included in this dataset are:

    steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

    date: The date on which the measurement was taken in YYYY-MM-DD format

    interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

### Required libraries

- library(lattice)
- library(plyr)
- library(data.table)

### Loading and preprocessing the data

The data for this assignment can be downloaded from the course web site:

    Dataset: Activity monitoring data [52K] - can be accessed from 
    https://github.com/ealmand/RepData_PeerAssessment1/blob/master/activity.zip

The following R script will load the data for processing.
   

```r
## set working directory
setwd("~/Data Science/Assignments/RepData_PA1")

## creating a data directory
if (!file.exists("data")) {
        dir.create("data")
}

## download a file and place in "data" directory
if (!file.exists("data")) {
        fileUrl <- "https://github.com/ealmand/RepData_PeerAssessment1/blob/master/activity.zip"
        zipfile="data/activity.zip"
        download.file(fileUrl, destfile=zipfile)
        unzip(zipfile, exdir="data")
}

## read data
data <- read.csv("./data/activity.csv")
```

Explore the data


```r
## take a look at some of the data
head(data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
## identify the dimension of the dataset
dim(data)
```

```
## [1] 17568     3
```

Transform the column classes for date and interval and remove NA values


```r
## transform date column values to date class
data$date <- as.Date(data$date, format="%Y-%m-%d")

## transform interval column values to numeric class
data$interval <- as.numeric(data$interval)

noNA <- data[!is.na(data$steps),]
```

## What is mean total number of steps taken per day?

### Make a histogram of the total number of steps taken each day


```r
## plot histogram
TotStepsDay <- sapply(split(data$steps, data$date), sum, na.rm=TRUE)
hist(TotStepsDay, main = 'total steps per day', xlab='steps')
```

![plot of chunk unnamed-chunk-4](./PA1_template_files/figure-html/unnamed-chunk-4.png) 

### Calculate and report the mean and median total number of steps taken per day


```r
## install library
library(plyr)
```

```
## Warning: package 'plyr' was built under R version 3.1.1
```

```r
total <- ddply(noNA, ~date, summarise, sum=sum(steps))

## calcluate mean and median steps per day
totmean <- mean(total$sum)
totmedian <- median(total$sum)
```

The mean of number of steps is **1.0766 &times; 10<sup>4</sup>** and median is **10765**.

## What is the average daily activity pattern?

### Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
meansteps <- ddply(noNA ,~interval, summarise, mean=mean(steps))
plot(meansteps$interval, meansteps$mean, ylab = "average steps", xlab = "Interval", type = "l")
```

![plot of chunk unnamed-chunk-6](./PA1_template_files/figure-html/unnamed-chunk-6.png) 

### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
max <- max(meansteps$mean)
maxint <- meansteps[meansteps$mean == max, 1]
```

The maximum interval is **835**.

## Imputing missing values

### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
totNA <- sum(is.na(data))
sum(is.na(data$steps))
```

```
## [1] 2304
```

The number of NA's in the entire data set is equal to the number of NA's in the steps column; therefore, no other columns in the dataset had NA's.

The total number of missing values in the dataset is **2304**.

### Devise a strategy for filling in all of the missing values in the dataset.

1. Create a new dataset
2. Replace NA value with the mean number of steps available in **'meansteps'**

### Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
## install library
(library(data.table))
```

```
## Warning: package 'data.table' was built under R version 3.1.1
```

```
## [1] "data.table" "plyr"       "stats"      "graphics"   "grDevices" 
## [6] "utils"      "datasets"   "methods"    "base"
```

```r
## create new data set
newdata <- data.table(data)

## replace NA values in the new data set
meansteps[,1] <- as.numeric(as.character(meansteps[,1]))
meansteps <- data.table(meansteps)
(newdata[is.na(steps), steps := round(meansteps[meansteps$interval==steps,meansteps$mean])])
```

```
## Warning: Coerced 'double' RHS to 'integer' to match the column's type; may
## have truncated precision. Either change the target column to 'double'
## first (by creating a new 'double' vector length 17568 (nrows of entire
## table) and assign that; i.e. 'replace' column), or coerce RHS to 'integer'
## (e.g. 1L, NA_[real|integer]_, as.*, etc) to make your intent clear and for
## speed. Or, set the column type correctly up front when you create the
## table and stick to it, please.
```

```
##        steps       date interval
##     1:     2 2012-10-01        0
##     2:     0 2012-10-01        5
##     3:     0 2012-10-01       10
##     4:     0 2012-10-01       15
##     5:     0 2012-10-01       20
##    ---                          
## 17564:     5 2012-11-30     2335
## 17565:     3 2012-11-30     2340
## 17566:     1 2012-11-30     2345
## 17567:     0 2012-11-30     2350
## 17568:     1 2012-11-30     2355
```

### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 


```r
## using the new data set with the NAs replaced by mean values calculate the total per day
totday <- aggregate(newdata$steps ~ newdata$date, newdata, FUN=sum)

## plot histogram
hist(totday[,2],xlab="Number of Steps",ylab="Frequency of Daily Total",main="Total Daily Steps\n(NA values imputed)")
```

![plot of chunk unnamed-chunk-10](./PA1_template_files/figure-html/unnamed-chunk-10.png) 

```r
## Calculate new mean and new median values
newmean <- mean(totday[,2])
newmedian <- median(totday[,2])
```

The mean of number of steps from the new dataset is **1.0766 &times; 10<sup>4</sup>** and median is **10762**.

### Do these values differ from the estimates from the first part of the assignment? 


```r
diffmean <-  totmean - newmean
diffmedian <- totmedian - newmedian
```

The difference in values of mean and median are **0.5493** and **3**, respectively.

### What is the impact of imputing missing data on the estimates of the total daily number of steps?

The impact of imputing missing data depends on the data, the methodology, and the ability for others to reproduce the research for the purpose of evaluating the methods by which the research was conducted. The reason for this course subject is to ensure that the research is reproducible should another method be evaluated in comparison to this.

## Are there differences in activity patterns between weekdays and weekends?

1.  Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

2.  Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
library(lattice)
```

```
## Warning: package 'lattice' was built under R version 3.1.1
```

```r
weekdays <- weekdays(as.Date(noNA$date))
wkday <- transform(noNA, day=weekdays)
wkday$wk <- ifelse(wkday$day %in% c("Saturday", "Sunday"),"weekend", "weekday")
meanweek <- ddply(wkday, .(interval, wk), summarise, steps=mean(steps))
xyplot(steps ~ interval | wk, data = meanweek, layout = c(1, 2), xlab = "5-minute interval", ylab = "average number of steps taken", type="l")
```

![plot of chunk unnamed-chunk-12](./PA1_template_files/figure-html/unnamed-chunk-12.png) 
