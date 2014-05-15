Reproducible Research: Peer Assessment 1
=========================================


## Loading and preprocessing the data

```r
library(lubridate)
activity <- read.csv("activity.csv",
                 stringsAsFactors = FALSE) # insure data is not converect to a factor
activity$date <- as.Date(activity$date)
activity$interval <- as.factor(activity$interval)
str(activity) #show structure of data frame
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...
```

## What is mean total number of steps taken per day?



## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
