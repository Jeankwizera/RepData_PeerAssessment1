Course project using **Activity Monitoring Data**
=================================================

set the default values for *echo*

``` r
knitr::opts_chunk$set(echo = TRUE)
```

First we download the data

``` r
url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url, destfile = "activity")
```

Read the data into R

``` r
activity = read.csv("./activity.csv")
```

**What is the mean total number of steps taken per day?**

first, remove the missing values

``` r
activity_complete = activity [complete.cases(activity),]
```

1.  Calculate the total number of steps taken per day

``` r
total_steps = aggregate(activity_complete$steps, by = list(activity_complete$date), sum)
colnames(total_steps) = c("date", "steps")
head(total_steps)
```

    ##         date steps
    ## 1 2012-10-02   126
    ## 2 2012-10-03 11352
    ## 3 2012-10-04 12116
    ## 4 2012-10-05 13294
    ## 5 2012-10-06 15420
    ## 6 2012-10-07 11015

1.  Make a histogram of the total number of steps taken each day

``` r
library(ggplot2)

ggplot(total_steps, aes(as.factor(date),steps))+geom_bar(fill="blue", stat="identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("date") + ylab("total steps") + ggtitle("total steps per day")
```

![](PA1_template_files/figure-markdown_github/histogram-1.png)

1.  Calculate and report the mean and median of the total number of steps taken per day

``` r
mean_steps = mean(activity_complete$steps)
median_steps = median(activity_complete$steps)
```

The mean and median total number of steps taken per day are 37.3825996 and 0 respectively.

**What is the average daily activity pattern?**

1.  Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

first we find the average number of *steps* aggregated by *interval*

``` r
activity_complete = aggregate(steps ~ interval, activity_complete, FUN = mean)
```

make the plot

``` r
ggplot(activity_complete, aes(x = interval, y = steps )) + geom_line(col = "red") + xlab("interval") + ylab("steps") + ggtitle("time series plot of Average number of \nsteps against interval ")
```

![](PA1_template_files/figure-markdown_github/time%20series%20plot-1.png)

1.  Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

``` r
most_steps = subset(activity_complete, activity_complete$steps == max(activity_complete$steps))[1,1]
```

The 5-minute interval with the highest number of steps is 835.

**Imputing missing values**

1.  Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NA's

``` r
total = nrow(activity) - nrow(activity [complete.cases(activity),])
```

There are 2304 rows with NA values

1.  Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

2.  Create a new dataset that is equal to the original dataset but with the missing data filled in.

``` r
nas_replaced = activity
nas_replaced[is.na(nas_replaced$steps),][1] = median(nas_replaced$steps, na.rm = T)
```

The new dataset is called *nas\_replaced*

1.  Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

first we find the total number of steps per day

``` r
new_total_steps = aggregate(nas_replaced$steps, by = list(nas_replaced$date), sum)
colnames(new_total_steps) = c("date", "steps")
```

now make a histogram

``` r
ggplot(new_total_steps, aes(as.factor(date),steps))+geom_bar(fill="blue", stat="identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("date") + ylab("total steps") + ggtitle("total steps per day")
```

![](PA1_template_files/figure-markdown_github/new%20histogram-1.png)

calculate the mean and median steps taken per day where NA values were replaced by median

``` r
new_mean = mean(nas_replaced$steps)
new_median = median(nas_replaced$steps)
```

The mean with NAs removed is 37.3825996 and 32.4799636 when replaced with median. Similarly, the median is 0 and 0 respectively.

The mean is therefore greater by 4.902636 when NAs are removed. The median however, remains the same at 0.

**Are there differences in activity patterns between weekdays and weekends?**

1.  Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

lets convert the date variable to a date format

``` r
nas_replaced$date = as.Date(nas_replaced$date)
```

using chron package, let us check if date is weekend then add a new variable to our dataframe showing this values

``` r
library(chron)

nas_replaced$day = chron::is.weekend(nas_replaced$date)

for (i in 1:nrow(nas_replaced)) {
  if (nas_replaced$day [i] == T) {
    nas_replaced$day [i] = "weekend"
  } else {
    nas_replaced$day [i] = "weekday"
  }
}
```

1.  Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

first we find the average number of *steps* aggregated by *interval*

``` r
nas_replaced = aggregate(steps ~ interval + day, nas_replaced, FUN = mean)
```

make the plot

``` r
ggplot(nas_replaced, aes(x = interval, y = steps, color=day)) + geom_line( ) + xlab("interval") + ylab("steps") + ggtitle("panel plot of Average number of \nsteps against interval for \nweekday and weekend") +
  facet_wrap(~ day, ncol=1)
```

![](PA1_template_files/figure-markdown_github/panel%20plot-1.png)

Based on the plot above, the *pattern* of activity remains generally unchanged during the weekdays and weekend.
