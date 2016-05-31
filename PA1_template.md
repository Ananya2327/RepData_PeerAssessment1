Loading and preprocessing the data
==================================

    df <- read.csv("activity.csv")

What is the mean total number of steps taken per day?
=====================================================

1. Calculate the total number of steps taken per day
----------------------------------------------------

    tsteps <- aggregate(steps ~ date, df, sum, na.rm = TRUE)

    tsteps

    ##          date steps
    ## 1  10/10/2012  9900
    ## 2  10/11/2012 10304
    ## 3  10/12/2012 17382
    ## 4  10/13/2012 12426
    ## 5  10/14/2012 15098
    ## 6  10/15/2012 10139
    ## 7  10/16/2012 15084
    ## 8  10/17/2012 13452
    ## 9  10/18/2012 10056
    ## 10 10/19/2012 11829
    ## 11  10/2/2012   126
    ## 12 10/20/2012 10395
    ## 13 10/21/2012  8821
    ## 14 10/22/2012 13460
    ## 15 10/23/2012  8918
    ## 16 10/24/2012  8355
    ## 17 10/25/2012  2492
    ## 18 10/26/2012  6778
    ## 19 10/27/2012 10119
    ## 20 10/28/2012 11458
    ## 21 10/29/2012  5018
    ## 22  10/3/2012 11352
    ## 23 10/30/2012  9819
    ## 24 10/31/2012 15414
    ## 25  10/4/2012 12116
    ## 26  10/5/2012 13294
    ## 27  10/6/2012 15420
    ## 28  10/7/2012 11015
    ## 29  10/9/2012 12811
    ## 30 11/11/2012 12608
    ## 31 11/12/2012 10765
    ## 32 11/13/2012  7336
    ## 33 11/15/2012    41
    ## 34 11/16/2012  5441
    ## 35 11/17/2012 14339
    ## 36 11/18/2012 15110
    ## 37 11/19/2012  8841
    ## 38  11/2/2012 10600
    ## 39 11/20/2012  4472
    ## 40 11/21/2012 12787
    ## 41 11/22/2012 20427
    ## 42 11/23/2012 21194
    ## 43 11/24/2012 14478
    ## 44 11/25/2012 11834
    ## 45 11/26/2012 11162
    ## 46 11/27/2012 13646
    ## 47 11/28/2012 10183
    ## 48 11/29/2012  7047
    ## 49  11/3/2012 10571
    ## 50  11/5/2012 10439
    ## 51  11/6/2012  8334
    ## 52  11/7/2012 12883
    ## 53  11/8/2012  3219

2. Make a histogram of the total number of steps taken each day
---------------------------------------------------------------

    hist(tsteps$steps, xlab = "Steps", main = "Total no. of steps taken each day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-3-1.png)

3. Calculate and report the mean and median of the total number of steps per day
--------------------------------------------------------------------------------

    Mean_steps <- mean(tsteps$steps) 
    Mean_steps

    ## [1] 10766.19

    Median_steps <- median(tsteps$steps) 
    Median_steps

    ## [1] 10765

What is the average daily activity pattern?
===========================================

1. Make a time series plot(i.e, type = "l) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
-----------------------------------------------------------------------------------------------------------------------------------------------------

    dsteps <- aggregate(steps ~ interval, df, mean)

    plot(x = dsteps$interval, y = dsteps$steps,  type = 'l', xlab = "Interval", ylab = "Average Steps",
         main = "Time-Series plot of average steps per 5-min interval")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-5-1.png)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum no. of steps?
-------------------------------------------------------------------------------------------------------------

    x <- dsteps[dsteps$steps == max(dsteps$steps),]

    int <- x$interval 

    int

    ## [1] 835

Imputing Missing values
=======================

1. Calculate and report the total number of missing values in the dataset (i.e., the total number of rows with NAs)
-------------------------------------------------------------------------------------------------------------------

    sum(is.na(df$date))

    ## [1] 0

    sum(is.na(df$interval)) 

    ## [1] 0

    sum(is.na(df$steps))

    ## [1] 2304

2. Devise a strategy for filling in all of missing values in the dataset.
-------------------------------------------------------------------------

### Replace NAs with 0.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
--------------------------------------------------------------------------------------------------

The strategy is to substitute NAs with 0s.

    df1 <- df

    df1$steps <- replace(df1$steps, is.na(df1$steps), 0)

4. Make a histogram of the total number of steps taken per day. Do these values differ from the estimates in the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

    tsteps1 <- aggregate(steps ~ date, df1, sum)

    hist(tsteps1$steps, xlab = "Steps", main = "Total no. of steps taken each day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-9-1.png)

    Mean_steps1 <- mean(tsteps1$steps) #9354.23

    Median_steps1 <- median(tsteps1$steps) #10395

Imputing missing data reduces the mean and median values.

Are there differences in activity patterns between weekdays and weekends?
=========================================================================

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
-----------------------------------------------------------------------------------------------------------------------------------------------------

Yes, there are differences between the actvity patterns of weekdays and
weekends.

    d_col <- as.Date(df1$date, format = "%m/%d/%Y")

    w_col <- weekdays(d_col)

    day <- character(length(w_col))

    for (i in 1:length(w_col))
    {
      if (w_col[i] == "Saturday" || w_col[i] == "Sunday")
      {day[i] <- "weekend"}
      
      else
      {day[i] <- "weekday"}
      
    }

    df2 <- cbind(df1, day)

2. Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average no. of steps taken, averaged across all weekdays or weekend days (y-axis)
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

    library(lattice)

    df3 <- split(df2, f = as.factor(df2$day) )

    df3_weekday <- data.frame(df3$weekday, stringsAsFactors = TRUE)

    df3_weekend <- data.frame(df3$weekend, stringsAsFactors = TRUE)

    df3_weekday_steps <- aggregate(steps ~ interval, df3_weekday, mean)

    plot(x = df3_weekday_steps$interval, y = df3_weekday_steps$steps,  type = 'l', xlab = "Interval", ylab = "Average Steps",
         main = "Average steps per 5-min interval for weekdays")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-11-1.png)

    df3_weekend_steps <- aggregate(steps ~ interval, df3_weekend, mean)

    plot(x = df3_weekend_steps$interval, y = df3_weekend_steps$steps,  type = 'l', xlab = "Interval", ylab = "Average Steps",
         main = "Average steps per 5-min interval for weekdays")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-11-2.png)

    final_df <- rbind(df3_weekday, df3_weekend) 

    xyplot(steps~interval|factor(day), final_df,
           type='l',layout=c(1,2),
           xlab='Interval',ylab='Number of Steps')

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-11-3.png)
