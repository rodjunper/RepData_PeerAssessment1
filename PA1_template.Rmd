---
title: "Reproducible Research - Course Project 1"
author: "RJP"
date: "8/17/2020"
output: html_document
---

### Course Project Instructions

*1. Code for reading in the dataset and/or processing the data*
*2. Histogram of the total number of steps taken each day*
*3. Mean and median number of steps taken each day*
*4. Time series plot of the average number of steps taken*
*5. The 5-minute interval that, on average, contains the maximum number of steps*
*6. Code to describe and show a strategy for imputing missing data*
*7. Histogram of the total number of steps taken each day after missing values are imputed*
*8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends*
*9. All of the R code needed to reproduce the results (numbers, plots, etc.) in the report*

### 1. Loading and preprocessing the data set

```{r, echo = TRUE, message=FALSE, warning=FALSE}
activity<-read.csv("activity.csv")

# Libraries
library(ggplot2)
library(dplyr)
```

Exploring the basic structure of the data set
```{r, echo=TRUE}
# Structure
    dim(activity)
    names(activity)
    head(activity)
    str(activity)

# Verifying the number of NA records
    mean(is.na(activity$steps))
```

### 2. Total number of steps taken per day 

**2.1. Number of steps per day**

```{r echo=TRUE}
# Creating and printing the number of steps per day
    StepsPerDay <- aggregate(activity$steps, list(activity$date), FUN=sum)
    colnames(StepsPerDay) <- c("Date", "Steps")
    StepsPerDay
```

**2.2. Histogram of the total number of steps taken each day**

```{r histogram1, echo=TRUE, fig.width=10, warning=FALSE}
# Plotting the histogram
    hist1 <- ggplot(StepsPerDay, aes(Steps))
    hist1 + geom_histogram(boundary = 0, binwidth = 2000, col = "black", fill = "lightgray") + 
    ggtitle("Histogram of total number of steps per day") + xlab("Steps") + ylab("Frequency")
```


**2.3. Mean and median of total number of steps taken per day**

```{r echo=TRUE}
    # Mean
    print(paste("Mean of total number of steps per day: ",mean(StepsPerDay$Steps, na.rm=TRUE)))
    #Median
    print(paste("Median of total number of steps per day: ",median(StepsPerDay$Steps, na.rm=TRUE)))
```

### 3. Average daily activity pattern

**3.1. Time series plot of the 5 minute interval (x-axis) and average number of steps taken, averaged across all days (y-axis)**

```{r timeplot1, echo=TRUE, fig.width=10, warning=FALSE}
    # Creating a table with steps per 5-minute interval (Time)
    StepsPerTime <- aggregate(steps ~ interval, data = activity, FUN = mean, na.action = na.omit)
    # The line plot
    time1 <- ggplot(StepsPerTime, aes(interval, steps))
    time1 + geom_line() + 
    ggtitle("Average steps per 5-minute interval (Time)") + xlab("Time") + ylab("Steps")
```

**3.2. 5-minute interval (on average across all the days) with the maximum number of steps**

```{r echo=TRUE, fig.width=10, warning=FALSE}
    StepPerTimeTable <- tbl_df(StepsPerTime)
    StepPerTimeTable %>% select(interval, steps) %>% filter(steps==max(StepPerTimeTable$steps))
```
   
### 4. Imputing missing values

**4.1. Total number of missing values in the data set**

```{r echo=TRUE}
    sum(is.na(activity$steps))
```

**4.2. Replace missing values with the values of the average 5-minute interval**  
  
*CompleteSteps* is the new column without missing values.

```{r echo=TRUE}
    # Creating new column named "CompleteSteps"
    activity$CompleteSteps <- ifelse(is.na(activity$steps),
                                     StepsPerTime$steps[match(activity$interval, StepsPerTime$interval)],                                             activity$steps)
```

**4.3. New data set that is equal to the original data set but with the missing data filled in**   

```{r echo=TRUE}
    # New data set activityComplete
    activityComplete <- data.frame(steps=activity$CompleteSteps, interval=activity$interval, date=activity$date)
    print("First 6 lines form activityComplete data set:")
    head(activityComplete)
```

**4.4.1. Histogram of the total number of steps taken each day with missing data filled in**  

```{r histogram2, echo=TRUE, fig.width=10, warning=FALSE}
    # Data
    StepsPerDayComplete <- aggregate(activityComplete$steps, list(activityComplete$date), FUN=sum)
    colnames(StepsPerDayComplete) <- c("Date", "Steps")
    # Plotting the histogram
    hist2 <- ggplot(StepsPerDayComplete, aes(Steps))
    hist2 + geom_histogram(boundary=0, binwidth=2000, col="black", fill="lightgray") + 
    ggtitle("Histogram of total number of steps per day (with imputed NA)") + xlab("Steps") + ylab("Frequency")
```

**4.4.2. Mean and median total number of steps taken per day**

```{r echo=TRUE}
    # Mean
    print(paste("Mean of total number of steps per day (imputed NA): ",mean(StepsPerDayComplete$Steps)))
    #Median
    print(paste("Median of total number of steps per day (imputed NA): ",median(StepsPerDayComplete$Steps)))
```

**4.4.3. Do these values (mean and median) differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?**  

```{r echo=TRUE}
    print(paste("Difference in Mean of total number of steps per day (not-imputed vs imputed NA): ",
    mean(StepsPerDay$Steps, na.rm = TRUE) - mean(StepsPerDayComplete$Steps)))
    #Median
    print(paste("Difference in Median of total number of steps per day (not-imputed vs imputed NA): ",
    median(StepsPerDay$Steps, na.rm = TRUE) - median(StepsPerDayComplete$Steps)))
```

There was no impact of the missing data in the mean and a little impact in the median. However, it is important to note that the method used to impute missing values can change these results.

### 5. Are there differences in activity patterns between weekdays and weekends?

**5.1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.**

```{r echo=TRUE}
    # Date in correct format
    activityComplete$date <- as.Date(activityComplete$date, format = "%Y-%m-%d")
    # Creating the variable with weekdays name
    activityComplete$WeekDay <- weekdays(activityComplete$date)
    # Creating the variable indicating weekday or weekend
    activityComplete$DayType <- ifelse(activityComplete$WeekDay == 'Saturday' |
                                       activityComplete$WeekDay == 'Sunday',
                                       'Weekend',
                                       'Weekday')
    # The first 15 records in data set
    head(activityComplete, n=15)
```

**5.2. Two time series plot of the 5-minute interval (x-axis) and the average number of steps taken averaged, across weekday days or weekend days (y-axis).**  

```{r timeplot2, echo=TRUE, fig.width=10, warning=FALSE}
    # Creating table with total steps per time across weekday days or weekend days
    SPTCompTable <- aggregate(steps ~ interval + DayType, data = activityComplete, FUN = mean, na.action=na.omit)
    # The line plot
    time2 <- ggplot(SPTCompTable, aes(interval, steps))
    time2 + geom_line() + 
    ggtitle("Average steps per 5-minute interval (Time): Weekdays vs. Weekends") + xlab("Time") + ylab("Steps") + facet_grid(DayType ~ .)
```
