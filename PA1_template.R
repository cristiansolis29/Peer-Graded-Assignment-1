# Loading and preprocessing the data

## Loading required packages

library(ggplot2)
library(dplyr)
library(knitr)

## 1. Load the data (i.e. read.csv())

url1 <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
file1 <- "activity_monitoring_data.zip"

if(!file.exists(file1)) {
  download.file(url1, file1)
  file <- file1
  file.un <- unzip(file)
  unlink(file)
}

AMD <- read.csv("activity.csv")
head(AMD, 10)
str(AMD)


## 2. Process/transform the data (if necessary) into a format suitable for your analysis

AMD$date <- as.Date(AMD$date)
str(AMD)


##--------------------------------------------------------------------------------------

# What is mean total number of steps taken per day?

## For this part of the assignment, you can ignore the missing values in the dataset.

AMDC <- subset(AMD, complete.cases(AMD))

## Calculate the total number of steps taken per day.

nsteps <- aggregate(steps ~ date, data = AMDC, FUN = sum)

## Calculate and report the mean and median of the total number of steps taken per day

meansteps <- mean(nsteps$steps)
meansteps
mediansteps <- median(nsteps$steps)
mediansteps

## Make a histogram of the total number of steps taken each day

hist(nsteps$steps, breaks = 20, col = "antiquewhite", xlab = "Steps per day", ylab = "Number of days", main = "Histogram of steps per day")
abline(v=mean(nsteps$steps), lty=1, col="blue1")                   # draw a blue line thru the mean  
abline(v=median(nsteps$steps), lty=3, col="red")                   # draw a red line thru the median
text(mean(nsteps$steps),25,labels="mean", pos=4, col="blue1")      # label the mean  
text(median(nsteps$steps),23,labels="median", pos=4, col="red")   # label the median  
rug(nsteps$steps, col="darkmagenta")



##--------------------------------------------------------------------------------------

# What is the average daily activity pattern?

## Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

tsp <- aggregate(steps ~ interval, AMDC, mean)

plot(tsp$interval, tsp$steps, type = "l", xaxt = "n", col = "darkorange1", main = "Mean number of average steps over all days", xlab = "5 minute interval", ylab = "Average number of steps")
axis(1, at = seq(100, 2300, by = 100), las = 2)
text(which.max(tsp$steps),max(tsp$steps),  
     labels=paste("max = ",as.character(round(max(tsp$steps)))), 
     pos=4, col="firebrick1")

## Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

max_steps_interval <- which.max(tsp$steps)
print(signif(tsp[max_steps_interval,]), 1)



##--------------------------------------------------------------------------------------

# Imputing missing values

## Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

## Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

originalValue <- complete.cases(AMD)  
nMissing <- length(originalValue[originalValue == FALSE])                      # number of records with NA  
nComplete <- length(originalValue[originalValue == TRUE])                      # number of complete records

title="Complete Cases Examination"  
barplot(table(originalValue),main=title,xaxt='n', col="darkslategray1")      # render Complete Cases barplot  
axis(side=1,at=c(.7,1.9),labels=c("Missing","Complete"),tick=FALSE)          # render axis  
text(.7,0,labels=nMissing, pos=3)                                            # label the NA's bar  
text(1.9,0,labels=nComplete, pos=3) 

paste("The number of missing values is",nMissing)


## Devise a strategy for filling in all of the missing values in the dataset.
## Create a new dataset that is equal to the original dataset but with the missing data filled in.


AMDI <- AMD
AMDI$steps <- impute(AMD$steps, fun=mean)

## Make a histogram of the total number of steps taken each day

nsteps2 <- aggregate(steps ~ date, data = AMDI, FUN = sum)

hist(nsteps2$steps, breaks = 20, col = "antiquewhite", xlab = "Steps per day", ylab = "Number of days", main = "Histogram of steps per day")
abline(v=mean(nsteps$steps), lty=1, col="blue1")                   # draw a blue line thru the mean  
abline(v=median(nsteps$steps), lty=3, col="red")                   # draw a red line thru the median
text(mean(nsteps$steps),25,labels="mean", pos=4, col="blue1")      # label the mean  
text(median(nsteps$steps),23,labels="median", pos=4, col="red")   # label the median  
rug(nsteps$steps, col="darkmagenta")

##-----------------------------------------------------------------------------

# Are there differences in activity patterns between weekdays and weekends?

## Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

AMDI$dateType <-  ifelse(as.POSIXlt(AMDI$date)$wday %in% c(0,6), 'weekend', 'weekday')

##  Make a panel plot containing a time series plot

averagedAMDI <- aggregate(steps ~ interval + dateType, data=AMDI, mean)
ggplot(averagedAMDI, aes(interval, steps, color = dateType)) + 
  geom_line() + 
  facet_grid(dateType ~ .) +
  xlab("5-minute interval") + 
  ylab("avarage number of steps")
