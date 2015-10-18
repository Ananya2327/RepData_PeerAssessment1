---
title: "PA1_template"
output: html_document
---
#Loading and preprocessing the data


```r
activityData <- read.csv("C:\\Users\\e571237\\Desktop\\activity.csv")
```

#What is mean total number of steps taken per day?

##Calculate the total number of steps taken per day


```r
#splitting the data by date 

indDate <- split( x= activityData, f= activityData$date)

#creating a data frame that has the day no. and the no. of steps taken per day

df <- data.frame(Day = numeric(),
                 Total_Steps_taken_per_day= numeric(), 
                 stringsAsFactors=FALSE) 


for (i in 1:length(indDate))
{
  df <- rbind(df,sum(indDate[[i]]$steps))
}

df <- cbind(1:61, df)
names(df) <- c('Day','Total Steps taken per day')
```

##Make a histogram of the total no. of steps taken per day


```r
hist(df$`Total Steps taken per day`, main = "Total number of steps taken each day", xlab = "Steps", ylab = "Frequency")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-15-1.png" title="" alt="" width="672" />

##Calculate and report the mean and median of the total number of steps taken per day


```r
#created a data frame that has the day no, mean no of steps for that day and median no. of steps 
#for that day

meanSteps <- numeric(0)

for(i in 1:length(indDate))
{
  meanSteps <- append( meanSteps , mean(indDate[[i]]$steps, na.rm = TRUE))
}

medianSteps <- numeric(0)

for(i in 1:length(indDate))
{
  medianSteps <- append(medianSteps, median(indDate[[i]]$steps, na.rm = TRUE))
}

df <- cbind(df, meanSteps, medianSteps)
```

#What is the average daily activity pattern?

##Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
indInt <- split( x= activityData, f= activityData$interval)

meanStepsByInt <- numeric(0)

for(i in 1:length(indInt))
{
  meanStepsByInt <- append( meanStepsByInt , mean(indInt[[i]]$steps, na.rm = TRUE))
}

intervalList <- unique(activityData$interval)

#time series plot of the average daily pattern of steps for 5-minute intervals
plot(x= intervalList, y = meanStepsByInt, type = "l", main = "Average daily pattern of steps", xlab = "Interval", ylab = "Average no. of steps")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-17-1.png" title="" alt="" width="672" />

##Which 5-minute interval on average across all the days in the dataset, contains the maximum number of steps?

```r
index <- match(max(meanStepsByInt), meanStepsByInt)

intervalList[index]
```

```
## [1] 835
```

#Imputing missing values

##1. Calculate and report the total number of missing values in the dataset(the total no. of rows with NAs)


```r
sum(is.na(activityData$steps))
```

```
## [1] 2304
```

```r
###calculation part:
unique(is.na(activityData$interval)) 
```

```
## [1] FALSE
```

```r
unique(is.na(activityData$date)) 
```

```
## [1] FALSE
```

```r
#the above two statements returned false which means that there are no missing values for date or interval. Therefore, the missing values are present only in the steps column


###reporting part:

NaDF<-activityData[is.na(activityData$steps), ]     # this subset contains all the NA data 
unique(NaDF$date) #lists down all the dates that have all NA values
```

```
## [1] 2012-10-01 2012-10-08 2012-11-01 2012-11-04 2012-11-09 2012-11-10
## [7] 2012-11-14 2012-11-30
## 61 Levels: 2012-10-01 2012-10-02 2012-10-03 2012-10-04 ... 2012-11-30
```

##Devise a strategy for filling in all of the missing values in the dataset.

IMPUTATION STRATEGY: Filling NAs with 0s
There are no NA values for "interval" or "date". So we need to fill in NA in "steps".

##Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
##filling NA values with 0s and creating a new dataset that is equal to the original dataset but with missing data filled in

activityData2 <- activityData
activityData2[is.na(activityData2)] <- 0
```

##Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day


```r
df2 <- data.frame(Day = numeric(),
                 Total_Steps_taken_per_day= numeric(), 
                 stringsAsFactors=FALSE) 

indDate2 <- split( x= activityData2, f= activityData2$date)

for (i in 1:length(indDate2))
{
  df2 <- rbind(df2,sum(indDate2[[i]]$steps))
}

df2 <- cbind(1:length(indDate2), df2)
names(df2) <- c('Day','Total Steps taken per day')
```

## Make a histogram of the total no. of steps taken per day


```r
hist(df2$`Total Steps taken per day`, main = "Total no of steps", xlab = "Steps", ylab = "Frequency")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-22-1.png" title="" alt="" width="672" />

```r
###Filling NAs with 0s has had no effect on the total daily no. of steps
```

#Are there differences in activity patterns between weekdays and weekends?

##Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or a weekend day.

```r
library(timeDate)

LogicWeekday <- isWeekday(as.Date(activityData2$date), wday = 1:5)

Day <- character(0)

for ( i in 1:length(LogicWeekday))
{
  if (LogicWeekday[i] == TRUE)
    Day <- append(Day, "Weekday")
  else
    Day <- append(Day, "Weekend")
}

activityData3 <- cbind(activityData2, Day)
        #created a dataframe that has a column showing if a day is a weekday or a weekend
```


##Make a panel plot containing a time series plot (type = "l") of the 5-minute interval (x-axis) and the average no.of steps taken, averaged across all weekday days or weekend days (y-axis)


