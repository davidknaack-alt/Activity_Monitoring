Activity Monitoring Analysis
Loading and preprocessing the data
AM <- read.csv("~/Coursera/activity.csv")
What is mean total number of steps taken per day?
#loading in all packages needed for this exploratory analysis
library(dplyr)
library(tidyr)
library(ggplot2)
library(knitr)
head(AM)
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
tail(AM)
##       steps       date interval
## 17563    NA 2012-11-30     2330
## 17564    NA 2012-11-30     2335
## 17565    NA 2012-11-30     2340
## 17566    NA 2012-11-30     2345
## 17567    NA 2012-11-30     2350
## 17568    NA 2012-11-30     2355
Calculate the total number of steps taken per day.

sum.na <- function(x) sum(x, na.rm = TRUE)
steps.per.day <- AM %>% filter(!is.na(steps)) %>%  group_by(date) %>% mutate_at(., "steps", sum.na) %>% select("steps", "date") %>% distinct()
Make a histogram of the total number of steps taken each day.

#The argument 'type = "h"' is used to create a histogram from the plot function.
plot(seq_along(steps.per.day$steps), steps.per.day$steps, type = "h", main = "Number of steps per day", xlab = "Day", ylab = "Number of steps")


Calculate and report the mean and median of the total number of steps taken per day.

mean(steps.per.day$steps)
## [1] 10766.19
median(steps.per.day$steps)
## [1] 10765
What is the average daily activity pattern?
steps.per.interval <- AM %>% filter(!is.na(steps)) %>%  group_by(interval) %>% mutate_at(., "steps", sum.na) %>% select("steps", "interval") %>% distinct()
Make a time series plot (i.e. type=“l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).

plot(steps.per.interval$interval, steps.per.interval$steps/length(steps.per.day$date), type = "l", main = "Average steps per 5-minute interval", xlab = "5-minute time intervals", ylab = "Average steps")


Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

steps.per.interval$steps <- steps.per.interval$steps/length(steps.per.day$steps)
steps.per.interval[which.max(steps.per.interval$steps),] #835 - 840
## # A tibble: 1 x 2
## # Groups:   interval [1]
##   steps interval
##   <dbl>    <int>
## 1  206.      835
Imputing missing values
Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs).

AM$steps %>% is.na() %>% sum()
## [1] 2304
Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

My strategy will be to use the mean of the 5-minute interval to replace any NA values.

fill.na <- c()
for(i in 1:length(AM$steps)){
    if(is.na(AM$steps[i])){
        index <- i - (as.integer(i/288) * 288) + 1
        fill.na <- c(fill.na, steps.per.interval$steps[index])
    }
    else{
        fill.na <- c(fill.na, AM$steps[i])
    }
}
Create a new dataset that is equal to the original dataset but with the missing data filled in.

AM.fill <- AM
AM.fill$steps <- fill.na
head(AM.fill)
##       steps       date interval
## 1 0.3396226 2012-10-01        0
## 2 0.1320755 2012-10-01        5
## 3 0.1509434 2012-10-01       10
## 4 0.0754717 2012-10-01       15
## 5 2.0943396 2012-10-01       20
## 6 0.5283019 2012-10-01       25
tail(AM.fill)
##           steps       date interval
## 17563 4.6981132 2012-11-30     2330
## 17564 3.3018868 2012-11-30     2335
## 17565 0.6415094 2012-11-30     2340
## 17566 0.2264151 2012-11-30     2345
## 17567 1.0754717 2012-11-30     2350
## 17568 1.7169811 2012-11-30     2355
Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

steps.per.day.fill <- AM.fill %>% filter(!is.na(steps)) %>%  group_by(date) %>% mutate_at(., "steps", sum.na) %>% select("steps", "date") %>% distinct()
Make a histogram of the total number of steps taken each day.

#The argument 'type = "h"' is used to create a histogram from the plot function.
plot(seq_along(steps.per.day.fill$steps), steps.per.day.fill$steps, type = "h", main = "Number of steps per day (filled data)", xlab = "Day", ylab = "Number of steps")


Calculate and report the mean and median total number of steps taken per day.

mean(steps.per.day.fill$steps)
## [1] 10766.19
median(steps.per.day.fill$steps)
## [1] 10766.19
Do these values differ from the estimates from the first part of the assignment?

The mean does not differ from the original estimate, but the median does differ from the original estimate. The original median was 10765 and the median after filling NA values with mean of the 5-minute interval was 10766.19. The original and filled medians have an absolute difference of 1.19.
Are there differences in activity patterns between weekdays and weekends?
Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

weekday.end <- function(data){
    weekday.end <- c()
    for(i in 1:nrow(data)){
        weekday <- weekdays(as.Date(data$date[i]), abbreviate = TRUE)
        if(weekday == "Sat" | weekday == "Sun"){
            weekday.end <- c(weekday.end, "weekend")
        }
        else{
            weekday.end <- c(weekday.end, "weekday")
        }
    }
    return(weekday.end)
}
AM.fill$weekday <- weekday.end(AM.fill)
Make a panel plot containing a time series plot (i.e. type=“l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

steps.per.int.fill <- AM.fill %>% group_by(interval, weekday) %>% mutate_at(., "steps", sum.na) %>% select("steps", "interval", "weekday") %>% distinct()

df <- aggregate(steps ~ interval + weekday, steps.per.int.fill, mean)
qplot(interval, steps, data = df, geom = c("line"), xlab = "Interval", 
      ylab = "Number of steps", main = "") +
    facet_wrap(~ weekday, ncol = 1)
