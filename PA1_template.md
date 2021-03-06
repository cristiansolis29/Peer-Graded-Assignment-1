Loading and preprocessing the data
==================================

### 0. Loading required packages

    library(ggplot2)  
    library(knitr)
    library(plyr)
    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:plyr':
    ## 
    ##     arrange, count, desc, failwith, id, mutate, rename, summarise,
    ##     summarize

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(Hmisc)

    ## Loading required package: lattice

    ## Loading required package: survival

    ## Loading required package: Formula

    ## 
    ## Attaching package: 'Hmisc'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     combine, src, summarize

    ## The following objects are masked from 'package:plyr':
    ## 
    ##     is.discrete, summarize

    ## The following objects are masked from 'package:base':
    ## 
    ##     format.pval, round.POSIXt, trunc.POSIXt, units

### 1. Load the data (i.e. read.csv())

Extracting the data from the URL, check for existence in the current
working directory and read the data from the csv file.

    url1 <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    file1 <- "activity_monitoring_data.zip"

    if(!file.exists(file1)) {
      download.file(url1, file1)
      file <- file1
      file.un <- unzip(file)
      unlink(file)
    }

    AMD <- read.csv("activity.csv")

Starting to explore the data.

    head(AMD, 10)

    ##    steps       date interval
    ## 1     NA 2012-10-01        0
    ## 2     NA 2012-10-01        5
    ## 3     NA 2012-10-01       10
    ## 4     NA 2012-10-01       15
    ## 5     NA 2012-10-01       20
    ## 6     NA 2012-10-01       25
    ## 7     NA 2012-10-01       30
    ## 8     NA 2012-10-01       35
    ## 9     NA 2012-10-01       40
    ## 10    NA 2012-10-01       45

    str(AMD)

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

### 2. Process/transform the data (if necessary) into a format suitable for your analysis

Changing the $date variable from factor type to date type.

    AMD$date <- as.Date(AMD$date)
    str(AMD)

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

What is mean total number of steps taken per day?
=================================================

### 1. For this part of the assignment, you can ignore the missing values in the dataset.

Subsetting for only complete cases.

    AMDC <- subset(AMD, complete.cases(AMD))

### 2. Calculate the total number of steps taken per day.

Performing a calculation of the number of steps from the subsetted data.

    nsteps <- aggregate(steps ~ date, data = AMDC, FUN = sum)

### 3. Calculate and report the mean and median of the total number of steps taken per day

    meansteps <- mean(nsteps$steps)
    meansteps

    ## [1] 10766.19

    mediansteps <- median(nsteps$steps)
    mediansteps

    ## [1] 10765

### 4. Make a histogram of the total number of steps taken each day

Plotting the steps per day and showing the mean/median by the use of a
line indicator.

    hist(nsteps$steps, breaks = 20, col = "antiquewhite", xlab = "Steps per day", ylab = "Number of days", main = "Histogram of steps per day")
    abline(v=mean(nsteps$steps), lty=1, col="blue1")                   # draw a blue line thru the mean  
    abline(v=median(nsteps$steps), lty=3, col="red")                   # draw a red line thru the median
    text(mean(nsteps$steps),25,labels="mean", pos=4, col="blue1")      # label the mean  
    text(median(nsteps$steps),23,labels="median", pos=4, col="red")   # label the median  
    rug(nsteps$steps, col="darkmagenta")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-9-1.png)

What is the average daily activity pattern?
===========================================

### 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

Plotting a time series plot.

    tsp <- aggregate(steps ~ interval, AMDC, mean)

    plot(tsp$interval, tsp$steps, type = "l", xaxt = "n", col = "darkorange1", main = "Mean number of average steps over all days", xlab = "5 minute interval", ylab = "Average number of steps")
    axis(1, at = seq(100, 2300, by = 100), las = 2)
    text(which.max(tsp$steps),max(tsp$steps),  
         labels=paste("max = ",as.character(round(max(tsp$steps)))), 
         pos=4, col="firebrick1")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-10-1.png)

### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

Calculating the max steps - 5 minute interval.

    max_steps_interval <- which.max(tsp$steps)
    print(signif(tsp[max_steps_interval,]), 1)

    ##     interval  steps
    ## 104      835 206.17

Imputing missing values
=======================

### 1. Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

### 1.1 Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

Plotting a bar graph with the missing values count.

    originalValue <- complete.cases(AMD)  
    nMissing <- length(originalValue[originalValue == FALSE])                      # number of records with NA  
    nComplete <- length(originalValue[originalValue == TRUE])                      # number of complete records

    title="Complete Cases Examination"  
    barplot(table(originalValue),main=title,xaxt='n', col="darkslategray1")      # render Complete Cases barplot  
    axis(side=1,at=c(.7,1.9),labels=c("Missing","Complete"),tick=FALSE)          # render axis  
    text(.7,0,labels=nMissing, pos=3)                                            # label the NA's bar  
    text(1.9,0,labels=nComplete, pos=3) 

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-12-1.png)

Calculating the missing values with a function.

    paste("The number of missing values is",nMissing)

    ## [1] "The number of missing values is 2304"

### 2. Devise a strategy for filling in all of the missing values in the dataset.

### 2.1 Create a new dataset that is equal to the original dataset but with the missing data filled in.

Imputing the data.

    AMDI <- AMD
    AMDI$steps <- impute(AMD$steps, fun=mean)

### 3. Make a histogram of the total number of steps taken each day

Plotting a histogram from the imputed data.

    nsteps2 <- aggregate(steps ~ date, data = AMDI, FUN = sum)

    hist(nsteps2$steps, breaks = 20, col = "antiquewhite", xlab = "Steps per day", ylab = "Number of days", main = "Histogram of steps per day")
    abline(v=mean(nsteps$steps), lty=1, col="blue1")                   # draw a blue line thru the mean  
    abline(v=median(nsteps$steps), lty=3, col="red")                   # draw a red line thru the median
    text(mean(nsteps$steps),25,labels="mean", pos=4, col="blue1")      # label the mean  
    text(median(nsteps$steps),23,labels="median", pos=4, col="red")   # label the median  
    rug(nsteps$steps, col="darkmagenta")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-15-1.png)

Are there differences in activity patterns between weekdays and weekends?
=========================================================================

### 1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

Creating a factor variable.

    AMDI$dateType <-  ifelse(as.POSIXlt(AMDI$date)$wday %in% c(0,6), 'weekend', 'weekday')

### 2. Make a panel plot containing a time series plot

    averagedAMDI <- aggregate(steps ~ interval + dateType, data=AMDI, mean)
    ggplot(averagedAMDI, aes(interval, steps, color = dateType)) + 
      geom_line() + 
      facet_grid(dateType ~ .) +
      xlab("5-minute interval") + 
      ylab("avarage number of steps")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-17-1.png)

The graph looks less skewed and more uniform on weekends, probably due
to an increase in outdoor activities during weekends.
