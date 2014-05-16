Peer Assessment 1 for Reproducible research 
========================================================

## Submitted on May 16, 2014 by M. S. Ravi



Reading and cleaning up data:


```r
activity <- read.csv("activity.csv")
head(activity)
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


Creating a new table which only has the rows without NA in the steps column.


```r
good <- complete.cases(activity)
goodactivity <- activity[good, ]
```

Initializing a new array to store data about the steps per day and initializing the day.

```r
dates <- numeric(length = 100)
steps <- numeric(length = 100)
stepsinday <- data.frame(dates, steps)
day <- as.Date("2012-10-01")
```

Updating the table stepsinday, so that it counts the number of steps in each day.

```r
j<-1
for (i in 1:nrow(goodactivity)){
      #if we are in the same day, we won't change the day, 
      # but just add the number of steps
      if (as.Date(goodactivity[i,2])==day){
            stepsinday[j,2]<-stepsinday[j,2]+goodactivity[i,1]
            }
      #if the day has changed, we go to the next row in stepsinday and start
      #ading the number of steps
      else {j<-j+1
            day<-as.Date(goodactivity[i,2])
            stepsinday[j,1]<-day
            stepsinday[j,2]<-stepsinday[j,2]+goodactivity[i,1]
            }
      }
```

Since we intialized a larger table than needed, we have a buch of irrelevant zeros in the table stepsinday. We remove them by first converting the zeros to NA, and then getting rid of the NAs.


```r
stepsinday[stepsinday == 0] <- NA
stepsinday <- na.omit(stepsinday)
stepsinday
```

```
##    dates steps
## 2  15615   126
## 3  15616 11352
## 4  15617 12116
## 5  15618 13294
## 6  15619 15420
## 7  15620 11015
## 8  15622 12811
## 9  15623  9900
## 10 15624 10304
## 11 15625 17382
## 12 15626 12426
## 13 15627 15098
## 14 15628 10139
## 15 15629 15084
## 16 15630 13452
## 17 15631 10056
## 18 15632 11829
## 19 15633 10395
## 20 15634  8821
## 21 15635 13460
## 22 15636  8918
## 23 15637  8355
## 24 15638  2492
## 25 15639  6778
## 26 15640 10119
## 27 15641 11458
## 28 15642  5018
## 29 15643  9819
## 30 15644 15414
## 31 15646 10600
## 32 15647 10571
## 33 15649 10439
## 34 15650  8334
## 35 15651 12883
## 36 15652  3219
## 37 15655 12608
## 38 15656 10765
## 39 15657  7336
## 40 15659    41
## 41 15660  5441
## 42 15661 14339
## 43 15662 15110
## 44 15663  8841
## 45 15664  4472
## 46 15665 12787
## 47 15666 20427
## 48 15667 21194
## 49 15668 14478
## 50 15669 11834
## 51 15670 11162
## 52 15671 13646
## 53 15672 10183
## 54 15673  7047
```

Note that the days are in the numeric format, of the number of days from Jan.1, 1970. Couldn't do too much about this, so I left it alone. I will now plot the histogram of the steps per day and get the mean and the median of this column.

```r
hist(stepsinday[, 2], main = "Histogram of steps per day", xlab = "Number of Steps in a day", 
    ylab = "frequency")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r
mean(stepsinday[, 2])
```

```
## [1] 10766
```

```r
median(stepsinday[, 2])
```

```
## [1] 10765
```

## Mean daily activity

Now we will go on to calculating the mean over each time interval. Here we are ignoring the NA values.

```r
timesofday <- aggregate(activity$steps, list(timeofday = activity$interval), 
    mean, na.rm = TRUE)
timesofdayts <- ts(timesofday[, 2])
max(timesofday[, 2])
```

```
## [1] 206.2
```

The maximum number of steps is 206.2. We now plot these average steps:

```r
plot.ts(timesofdayts)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

The maximum of the average steps, seems to be around the 105th data point, which corresponds to the time 104*5=520 minutes after midnight, which would be between 8:40 and 8:45 a.m. Now we willplot the 

## Inputting Missing values
We first calculate the number of rows in the original data with NA's in them:

```r
missing <- nrow(activity) - nrow(goodactivity)
(missing/nrow(activity)) * 100
```

```
## [1] 13.11
```

This is the percentage of the number of rows with NAs in the original data.
In the code below, we replace each NA in the original data, by the average number of steps for that time period.

```r
for (i in 1:nrow(activity)) {
    if (is.na(activity[i, 1])) {
        time <- activity[i, 3]
        j <- which(timesofday[, 1] == time)
        activity[i, 1] <- timesofday[j, 2]
    }
}
```

Now we will count the number of steps in a day with this fixed up data:


```r
dates<-numeric(length=100)
steps<-numeric(length=100)
stepsinday <- data.frame(dates,steps)
day<-as.Date("2012-10-01")
j<-1
stepsinday[j,1]<-day
for (i in 1:nrow(activity)){
      #if we are in the same day, we won't change the day, 
      # but just add the number of steps
      if (as.Date(activity[i,2])==day){
            stepsinday[j,2]<-stepsinday[j,2]+activity[i,1]
            }
      #if the day has changed, we go to the next row in stepsinday and start
      #ading the number of steps
      else {j<-j+1
            day<-as.Date(activity[i,2])
            stepsinday[j,1]<-day
            stepsinday[j,2]<-stepsinday[j,2]+activity[i,1]
            }
      }
stepsinday[stepsinday==0] <- NA
stepsinday<-na.omit(stepsinday)
```

Here are the mean and median of the number of steps per day with the fixed up data:

```r
mean(stepsinday[, 2])
```

```
## [1] 10766
```

```r
median(stepsinday[, 2])
```

```
## [1] 10766
```

```r
hist(stepsinday[, 2], main = "Histogram of steps per day with modified data", 
    xlab = "Number of Steps in a day", ylab = "frequency")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 

There does not seem to be much difference between replacing the NA values by the mean for their time interval, and ignoring the NAs. This is mainly because there was a large amount of data, and the number of missing data was just about 13%. Further the NA times would normally correspond to times with very low activity.
##Splitting the data into weekdays and weekends
We are going to create a new column in our activity table that will keep track of whether the day in question was a weekday or not. We first find the day of the week for each date.


```r
dates <- activity[, 2]
weekday <- weekdays(as.Date(dates))
wkend <- weekday == "Saturday" | weekday == "Sunday"
weekday[wkend] <- "weekend"
weekday[!wkend] <- "workingday"
newactivity <- cbind(activity, weekday)
```

Now, we will calculate the average number of steps for  working days:

```r
timesofdaywkg <- aggregate(activity$steps, list(timeofday = activity$interval, 
    weekday = newactivity[, 4] == "workingday"), mean)
timesofweekts <- ts(timesofdaywkg[, 3])
plot.ts(timesofweekts)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 

And now for the weekends:

```r
timesofdaywkend <- aggregate(activity$steps, list(timeofday = activity$interval, 
    weekday = newactivity[, 4] == "weekend"), mean)
timesofwkendts <- ts(timesofdaywkend[, 3])
plot.ts(timesofwkendts)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 

