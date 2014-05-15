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


