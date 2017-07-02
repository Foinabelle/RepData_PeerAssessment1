Reproducible Data - Project 1
==============================

Loading and Preparing the Data


```r
FIT<- read.csv("activity.csv", colClass = c('integer', 'Date', 'integer'))
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.3.3
```
##Question 1: What is mean total number of steps taken per day?##

Create a data.frame called "Daily" using the aggregate function to show the total number of steps per day.


```r
Daily<- aggregate(steps~date, sum, data = FIT, na.rm=TRUE)
```
Use Daily  to create a histogram:


```r
qplot(Daily$steps, geom = "histogram", binwidth = 500,  main = "Histogram for total daily steps", xlab = "Steps", ylab = "Count", fill=I("blue"), col = I("red"))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->


The histogram shows that the subject's step count ranges from 0 to 20000 per day and that he/she most frequently does between 9000 and 15000 steps per day.

*Calculate the mean and median number of steps per day:*


```r
mean(Daily$steps)
```

```
## [1] 10766.19
```

```r
median(Daily$steps)
```

```
## [1] 10765
```


##Question 2: What is the average daily activity pattern?##

*Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).*

Create the database StepbyInt using the aggregate function to show the mean number of steps taken in each 5 min time interval. Use this database to create the line graph required.

```r
StepbyInt<- aggregate(steps~interval, mean, data=FIT)
qplot(interval, steps, data = StepbyInt, geom = "line", main = "Time Series plot of Steps / 5 minute Interval(Average)", xlab = "5 minute time interval", ylab = "Average Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

*Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?*


```r
max<- max(StepbyInt$steps)
StepbyInt$interval[StepbyInt$steps==max]
```

```
## [1] 835
```

##Part 3: Imputing Missing Values##

1/ Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(FIT$steps))
```

```
## [1] 2304
```

2/Devise a strategy for filling in all of the missing values in the dataset and create a new dataset that is equal to the original dataset but with the missing data filled in.

The strategy I used was to replace the NA data with the mean data for that 5 minute time interval. The database created is called FITclean.


```r
FITclean<- merge(FIT, StepbyInt, by = "interval", suffixes = c("", ".mean"))
nas<- is.na(FITclean$steps)
FITclean$steps[nas]<-FITclean$steps.mean[nas]
```


3/ Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

First create a data frame using aggregate to get the total number of steps each day.  Then plot this data into a histogram.


```r
AggFITclean<- aggregate(steps~date, FITclean, sum)
qplot(AggFITclean$steps, geom = "histogram", binwidth = 500, main = "Histogram of Total number of steps per day (NA replaced with means", xlab = "Total Steps", ylab = "Frequency", fill = I("blue"), col = I("red"))
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
mean(AggFITclean$steps)
```

```
## [1] 10766.19
```

```r
median(AggFITclean$steps)
```

```
## [1] 10766.19
```


Replacing the NA with mean scores decreases the variance in the data. The histogram shows a shift upwards in the total number of steps.

##Part 4: Are there differences in activity patterns between weekdays and weekends?##

*Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.*


```r
FITclean$daytype<- ifelse(weekdays(FITclean$date)%in% c("Saturday", "Sunday"), "weekend", "weekday")
FITclean$daytype<- as.factor(FITclean$daytype)
```

* Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).* 

First create a data frame with the average number of steps taken in each 5 min interval and then plot it in two plots, one for weekdays and the other for weekends.


```r
WkdFIT<- aggregate(steps~interval + daytype, data = FITclean, mean)

ggplot(WkdFIT, aes(interval, steps))+geom_line()+ facet_grid(.~daytype)+ ggtitle("Time Series plot of Average number of steps\ntaken by 5-minute interval")+ xlab("Time Interval")+ ylab("Average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->


