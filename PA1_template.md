## Introduction

It is now possible to collect a large amount of data about personal
movement using activity monitoring devices such as a Fitbit, Nike
Fuelband, or Jawbone Up. These type of devices are part of the
“quantified self” movement – a group of enthusiasts who take
measurements about themselves regularly to improve their health, to find
patterns in their behavior, or because they are tech geeks. But these
data remain under-utilized both because the raw data are hard to obtain
and there is a lack of statistical methods and software for processing
and interpreting the data.

This assignment makes use of data from a personal activity monitoring
device. This device collects data at 5 minute intervals through out the
day. The data consists of two months of data from an anonymous
individual collected during the months of October and November, 2012 and
include the number of steps taken in 5 minute intervals each day.

    #Downloading and preprocessing the data

    if(!file.exists("./data")){dir.create("./data")}
    fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    download.file(fileUrl,destfile="./data/activity.zip",method="curl")
    unzip(zipfile="./data/activity.zip",exdir="./data")
    activity <- read.csv("./data/activity.csv")
    activity$date <- as.Date(activity$date)

    #Omitting NA from the data
    activity_clean <-na.omit(activity)

    #What is mean total number of steps taken per day?
    #Calculate the total number of steps taken per day

    steps_per_day <- aggregate(activity_clean$steps, by = list(Steps.Date = activity_clean$date), FUN = "sum")

    #Make a histogram of the total number of steps taken each day
    hist(steps_per_day$x, col = "pink", breaks = 20,main = "Total number of steps taken per day",xlab = "Number of steps taken per day")

![](Week-2-Project-1_files/figure-markdown_strict/unnamed-chunk-1-1.png)

    #Calculate and report the mean and median of the total number of steps taken per day
    mean_steps <- mean(steps_per_day[,2])
    print (mean_steps)

    ## [1] 10766.19

    median_steps <- median(steps_per_day[,2])
    print (median_steps)

    ## [1] 10765

    #What is the average daily activity pattern?
    #Make a time series plot (i.e. type = "l"\color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

    average_perday <- aggregate(activity_clean$steps, by = list(Interval = activity_clean$interval), FUN = "mean")

    plot(average_perday$Interval, average_perday$x, type = "l", 
         main = "Average daily activity pattern", 
         ylab = "Avarage number of steps taken", 
         xlab = "5-min intervals")

    #Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
    interval_row <- which.max(average_perday$x)
    max_interval <- average_perday[interval_row,1]
    print (max_interval)

    ## [1] 835

    #Imputing missing values
    #Calculate and report the total number of missing values in the dataset
    NA_number <- length(which(is.na(activity$steps)))
    print (NA_number)

    ## [1] 2304

    #Created a new dataset with the missing data filled in.

    library(Hmisc)

    ## Loading required package: lattice

    ## Loading required package: survival

    ## Loading required package: Formula

    ## Loading required package: ggplot2

    ## 
    ## Attaching package: 'Hmisc'

    ## The following objects are masked from 'package:base':
    ## 
    ##     format.pval, units

![](Week-2-Project-1_files/figure-markdown_strict/unnamed-chunk-1-2.png)

    activity_filled <- activity
    activity_filled$steps <- impute(activity$steps, fun=mean)

    #Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.
    steps_per_day_2 <- aggregate(activity_filled$steps, by = list(Steps.Date = activity_filled$date), FUN = "sum")
    hist(steps_per_day_2$x, col = "light blue", breaks = 20,main = "Total number of steps taken each day (filled data)",xlab = "Number of steps per day")

![](Week-2-Project-1_files/figure-markdown_strict/unnamed-chunk-1-3.png)

    mean_steps_2 <- mean(steps_per_day_2[,2])
    print (mean_steps_2)

    ## [1] 10766.19

    median_steps_2 <- median(steps_per_day_2[,2])
    print (median_steps_2)

    ## [1] 10766.19

    #Are there differences in activity patterns between weekdays and weekends?
    activity_filled$date <- as.Date(activity_filled$date)
    activity_filled$weekday <- weekdays(activity_filled$date)
    activity_filled$day_type <- ifelse(activity_filled$weekday=="Saturday" |
                                         activity_filled$weekday=="Sunday","Weekend","Weekday")
    activity_filled$day_type <- factor(activity_filled$day_type)

    day_types_data <- aggregate(steps ~ interval + day_type, data=activity_filled, mean)

    library(ggplot2)
    ggplot(day_types_data, aes(interval, steps)) + 
      geom_line() + 
      facet_grid(day_type ~ .) +
      xlab("5-minute intervals") + 
      ylab("Avarage number of steps taken") +
      ggtitle("Weekdays and weekends activity patterns")

![](Week-2-Project-1_files/figure-markdown_strict/unnamed-chunk-1-4.png)
