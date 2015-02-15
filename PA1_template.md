# Reproducible Research: Peer Assessment 1

Common options for the knitr output document are set in the first line of R code.


```r
library(knitr)
opts_chunk$set(echo=TRUE, results="hide")
```






## Loading and preprocessing the data

Asumming that the data file activity.csv is downloaded and avaialble in the working directory, below R code loads the data in R workspace. 


```r
data_df <- read.csv("activity.csv")
```





## What is mean total number of steps taken per day?

Following steps are implemented in below R code:

1. Compute dataframe stotal_df,  with two columns date and total_steps_per_day
2. Create the .png file for storing the plot output
3. Create the historgram for total_steps_per_day using stotal_df dataframe
4. Turn off the device
5. Compute mean of total_steps_per_day and store it to stotal_mean
6. Compute median of total_steps_per_day and store it to stotal_median


```r
library(plyr)

#Calculate the total number of steps taken per day
stotal_df <- ddply(data_df,.(date),summarize, total_steps_per_day=sum(steps, na.rm = T) )

#Make a histogram of the total number of steps taken each day
png(filename = "figures/hist_na.png", width = 480, height = 480, units = "px")

hist(stotal_df[,'total_steps_per_day'], xlab='total steps per day', ylab='frequency', main='Histogram of total number of steps taken each day')

dev.off()
```




```r
#Calculate and report the mean and median of the total number of steps taken per day
stotal_mean <- mean(stotal_df[,'total_steps_per_day'], na.rm=TRUE)
stotal_median <- median(stotal_df[,'total_steps_per_day'], na.rm=TRUE)

stotal_mean
stotal_median
```

 Mean=9354.2295082 and median=10395 for total number of steps taken per day.




## What is the average daily activity pattern?

To plot 5-intervals vs. average number of steps (for the interval across all days), first compute the dataframe imean_df with two columns steps and interval_avg. Use imean_df to plot interval vs interval_avg to the output file.



```r
imean_df <- ddply(data_df,.(interval),summarize, interval_avg=mean(steps, na.rm=TRUE) )

png(filename = "figures/imean_plot.png", width = 480, height = 480, units = "px")

with(imean_df, plot( interval, interval_avg, 
                     type='l', xlab='Interval', ylab='Average number of steps',
                     main="Average daily activity pattern"))

dev.off()
```


Below R code computes the row in dataframe with max of 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps? 

Answere: 835 (avg steps=206.1698)


```r
result <-  subset(imean_df, interval_avg==max(imean_df[,'interval_avg']), select=c('interval','interval_avg'))

result
```

    interval interval_avg
104      835     206.1698





## Imputing missing values

Below R code computes the total number of missing values in the dataset


```r
#Calculate and report the total number of missing values in the dataset
nrow(data_df[complete.cases(data_df)==F,])
```

[1] 2304



The NA values are being replaced by the mean for that corresponding 5-minute interval. The newly computed dataset is stored in stotal_df dataframe.



```r
#strategy for filling in all of the missing values in the dataset
data_df[,'steps'] <- ifelse(is.na(data_df$steps),imean_df$interval_avg, data_df$steps)


#Create a new dataset that is equal to the original dataset but with the missing data filled in
stotal_df <- ddply(data_df,.(date),summarize, total_steps_per_day=sum(steps) )
```





Below R code creates histogram using the new dataframe stotal_df, which has all NA values replaced.


```r
#histogram of the total number of steps taken each day
png(filename = "figures/hist_wo_na.png", width = 480, height = 480, units = "px")

hist(stotal_df[,'total_steps_per_day'], xlab='total steps per day', ylab='frequency', main='Histogram of total number of steps taken each day')

dev.off()
```





Due to replacing the NA values with mean of steps of corresponsind interval across all days, leads to increase in frequency of the values of around average number of steps per interval.

Below R code computes the new stotal_mean and stotal_median from the new stotal_df.


```r
#mean and median total number of steps taken per day
stotal_mean <- mean(stotal_df[,'total_steps_per_day'], na.rm=TRUE)

stotal_median <- median(stotal_df[,'total_steps_per_day'], na.rm=TRUE)

stotal_mean
```

[1] 10766.19

```r
stotal_median
```

[1] 10766.19


Mean=1.0766189\times 10^{4} and median=1.0766189\times 10^{4} for total number of steps taken per day for the case where all the NA values are replaced.






## Are there differences in activity patterns between weekdays and weekends?

Below R code creates a new factor column called day, with two levels "Weekday" and "Weekend". Next the resulting dataframe is grouped by day and interval indicating average number of steps takes per day per interval. This information in than plotted on a lattice.


```r
library(timeDate)
library(lattice)

#a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day
data_wk_df <- cbind(data_df, day=ifelse(isWeekday(data_df$date,wday=1:5), "Weekday", "Weekend"))
data_wk_mean_df <-ddply(data_wk_df,.(day,interval),summarize, interval_avg=mean(steps) )


#plot 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)
png(filename = "figures/wkmean_plot.png", width = 480, height = 480, units = "px")

xyplot((interval_avg/50) ~ interval | day, data=data_wk_mean_df, layout=c(1,2), type='l',xlab='Interval', ylab='Number of steps')

dev.off();
```
