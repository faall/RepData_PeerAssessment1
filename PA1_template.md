---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

Data file is known to be a CVS file and is provided in the git repo zipped. READ.CSV should handle compressed files but it is not working in my system (Linux). So let me do it the old way using UNZ command to extract the CSV file and pass it to READ.CSV.

Dates are provided as texts, I am using `stringsAsFactors = FALSE` to preserve them as text and process it later into Date class since it will make things easier when plotting.


```r
df1<-read.csv(unz("./activity.zip","activity.csv"), stringsAsFactors = FALSE)
df1$date<- as.Date(df1$date)
str(df1)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


## What is mean total number of steps taken per day?

I will use **dplyr** to perform the per day analysis.


```r
library(dplyr)
```

First thing is to convert the data frame and group data per day.


```r
df2<-tbl_df(df1)
df2_date<-group_by(df2, date)
```

And finally compute the sum per day (ignoring NA's)


```r
df2_sum<-summarise(df2_date, sum(steps, na.rm = TRUE))
names(df2_sum)<-c("date","sum")
```

A histogram for the total number of steps.


```r
hist(df2_sum$sum, main = "Histogram of total steps by day", xlab = "Steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

The mean steps per day:


```r
mea1 <- mean(df2_sum$sum)
mea1
```

```
## [1] 9354
```

And the median steps per day:


```r
med1 <- median(df2_sum$sum)
med1
```

```
## [1] 10395
```


## What is the average daily activity pattern?

Now group the data by **interval** and get the average (mean) for the intervals


```r
df2_inter<-group_by(df2, interval)
df2_avg<-summarise(df2_inter, mean(steps, na.rm = TRUE))
names(df2_avg)<-c("interval","average")
```

The time series for average steps per interval:


```r
plot(df2_avg, type = "l", main = "Average steps per 5 min. intervals")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 

And the interval with maximum activity:


```r
df2_avg[which.max(df2_avg$average),]
```

```
## # A tibble: 1 × 2
##   interval average
##      <int>   <dbl>
## 1      835   206.2
```


## Imputing missing values

The number of `NA` values in the data set:


```r
sum(is.na(df1$steps))
```

```
## [1] 2304
```

The strategy to fill in the missing values will be to use the mean value for the interval over all days. A new variable `isMissing` separates the original data set into two subsets. A `for` loop goes thru the subset with missing values and get the mean steps for that interval from the means data set.   


```r
isMissing<-is.na(df1$steps)
df1_mis<-df1[isMissing,]
for(i in 1:nrow(df1_mis)){
      inter<-df1_mis[i,3]
      meanstep <- df2_avg[df2_avg$interval==inter,2]
      df1_mis[i,1]<-round(meanstep)
      }
```

Finally both subsets are combined again to form a new data set that has no `NA` values.


```r
df3 <- rbind(df1[!isMissing,], df1_mis)
sum(is.na(df3))
```

```
## [1] 0
```

Finally lets repeat the data analysis with the new data set.


```r
df4<-tbl_df(df3)
df4_date<-group_by(df4, date)
df4_sum<-summarise(df4_date, sum(steps, na.rm = TRUE))
names(df4_sum)<-c("date","sum")
par(mfcol=c(1,2), oma = c(1,1,3,1))
hist(df4_sum$sum, main = "After", xlab = "Steps", ylim = c(0, 40))
hist(df2_sum$sum, main = "Before", xlab = "Steps", ylim = c(0, 40))
title("Histogram of total steps by day", outer = TRUE)
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14.png) 

The mean steps per day:


```r
mean(df4_sum$sum)
```

```
## [1] 10766
```

And the median steps per day:


```r
median(df4_sum$sum)
```

```
## [1] 10762
```

After imputing values the mean (before 9354.2295) and median (before 10395) have increased and the histogram has a more normal shape.

## Are there differences in activity patterns between weekdays and weekends?

First get the weekday from the `date` column using `weekday()`. In Spanish, weekend is either "sábado" or "domingo", that creates a logical vector with TRUE = weekend and FALSE = weekday. Manipulating the factor levels we end with the desired column indicating the type of day in the week.   


```r
wend <- c("sábado", "domingo")
wdays <- weekdays(df3[,2])
wdays <- wdays %in% c("sábado", "domingo")
wdays <- factor(wdays, c("FALSE", "TRUE"))
levels(wdays) <- c("weekday", "weekend")
df3$wday <- wdays
df5 <- tbl_df(df3)
df5_wday <- group_by(df5, wday, interval)
df5_avg <- summarise(df5_wday, mean(steps)) 
str(df5_avg)
```

```
## Classes 'grouped_df', 'tbl_df', 'tbl' and 'data.frame':	576 obs. of  3 variables:
##  $ wday       : Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval   : int  0 5 10 15 20 25 30 35 40 45 ...
##  $ mean(steps): num  2.2889 0.4 0.1556 0.1778 0.0889 ...
##  - attr(*, "vars")=List of 1
##   ..$ : symbol wday
##  - attr(*, "drop")= logi TRUE
```


```r
par(mfrow=c(2,1), mar = c(3,1,1,1), oma = c(1,1,3,1))
plot(df5_avg[df5_avg$wday=="weekday",2:3], type = "l", main = "Weekday")
plot(df5_avg[df5_avg$wday=="weekend",2:3], type = "l", main = "Weekend")
title("Average steps per 5 min. intervals", outer = TRUE)
```

![plot of chunk unnamed-chunk-18](figure/unnamed-chunk-18.png) 

