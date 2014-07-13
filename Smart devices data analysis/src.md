## Analysis of data collected from smart devices.


## Loading and preprocessing the data

```r
dat <- read.csv("activity.csv", header = TRUE)
stepSum <- tapply(dat$steps, dat$date, sum, na.rm = TRUE)
```



## Mean total number of steps taken per day.


```r
hist(stepSum)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1.png) 


```r
m <- mean (stepSum)
med <- quantile(stepSum, probs = 0.5)
```
Mean is 9354.2295 and median is 1.0395 &times; 10<sup>4</sup>.



## The average daily activity pattern


```r
meanSteps <- tapply(dat$steps, dat$interval, mean, na.rm =T)
intervals <- dat$interval[1:length(meanSteps)]
plot (intervals, meanSteps, type = "l")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r
maxInd <- which.max (meanSteps)
ans <- intervals[maxInd]
```

The max value is for interval 835



## Inputing missing values



```r
mxNA <- sum(is.na(dat$steps))

newSteps <- dat$steps
newSteps[is.na(dat$steps)] <- 0
newDat <- cbind (newSteps, dat$date, dat$interval)
newDat <- as.data.frame(newDat)
stepSum <- tapply(newDat[,"newSteps"], dat$date, sum)
hist(stepSum)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

```r
m <- mean (stepSum)
med <- quantile(stepSum, probs = 0.5)
```

Mean is 9354.2295 and median is 1.0395 &times; 10<sup>4</sup>.

NA values are replaces by 0, simply due to the fact that unavailabe can be interpreted for the value in one way.
Because the NA values are replaced by 0, asn the original stepSum was calculated using na.rm = T for mean argument(in tapply), there is no chage in the result.
Hence mean, median and the total daily steps values are unchanged.






## Differences in activity patterns between weekdays and weekends.




```r
dt <- as.Date(dat[,"date"], "%Y-%m-%d")
Days <- weekdays(dt)
lWEnd <- Days %in% c("Saturday", "Sunday")
Days[lWEnd] <- "weekend"

Days[!lWEnd] <- "weekday"
newDat$wkDayEnd <- Days
newDat$fact <- factor(Days)
library(lattice) 
xyplot(step ~ interval | fact, data = newDat, type = "l")
```

```
## Error: object of type 'closure' is not subsettable
```


