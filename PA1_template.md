---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
--- 


### Introduction

The numbers were rounded to the next integer.


### Loading and preprocessing the data


```r
library(magrittr)
library(tidyverse) # useful packages for data science
library(knitr)     
library(lubridate) # makes handle dates easy
# shown code, and cache results the default
knitr::opts_chunk$set(echo = TRUE, cache = TRUE)
```

```r
# readr::read_csv can read zip files
steps_raw <- read_csv("activity.zip") %>% as.tibble()
```

```
## Parsed with column specification:
## cols(
##   steps = col_integer(),
##   date = col_date(format = ""),
##   interval = col_integer()
## )
```

### What is mean total number of steps taken per day?

The data contains missing values, I will ignore them for the moment.


```r
# total steps per day
steps_date <- steps_raw %>% group_by(date) %>% summarise(steps = sum(steps, na.rm = TRUE))
# first get total steps by date, then average them by weekday
steps_day_mean <- steps_raw %>% 
  count(date, wt = steps) %>%
  mutate(day = wday(date, label = TRUE, abbr = F)) %>% 
  mutate(day = fct_shift(day)) %>% 
  group_by(day) %>% summarise(mean_day = mean(n, na.rm = TRUE))
```

```r
plot(steps_date, type = "b", pch = 19, cex = .5, ann = FALSE)
title(xlab = "Date", ylab = "Steps", main = "Total steps taken per day")
grid()
```

![](PA1_template_files/figure-html/viz_steps_day_total-1.png)<!-- -->

```r
ggplot(steps_date, aes(steps)) + geom_histogram(binwidth = 500, fill = "firebrick") + 
  labs(x = "Steps", y = "Days", title = "Total number of steps taken each day")
```

![](PA1_template_files/figure-html/viz_steps_day_histogram-1.png)<!-- -->

#### Mean and median of the total number of steps taken each day

The mean of steps taken each day is 9355, and the median is 10395


### What is the average daily activity pattern?


```r
steps_interval <- steps_raw %>% 
  group_by(interval) %>% summarise(steps = mean(steps, na.rm = TRUE) %>% ceiling)
# most active interval
top_interval <- steps_interval %>% arrange(desc(steps)) %>% head(1) %>% extract2(c(1, 1))
```

```r
steps_interval %>% plot(type = "l", ann = FALSE)
title(xlab = "Interval", ylab = "Steps taken", main = "Mean of steps take by interval")
grid()
```

![](PA1_template_files/figure-html/viz_steps_interval-1.png)<!-- -->



#### The 5-minute interval that, on average, contains the maximum number of steps
The interval 835 contains the maximum number of steps.


### Imputing missing values

#### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)



```r
number_na <- steps_raw %>% filter(is.na(steps)) %>% nrow
```

The grand total of NA's in the dataset is 2304.  


#### Devise a strategy for filling in all of the missing values in the dataset


I will use the median for the 5-minute interval of the same weekday across the whole period to impute missing values.  

#### Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
# imputed values
steps_median <- steps_raw %>% 
  mutate(day = wday(date, label = TRUE, abbr = FALSE)) %>% group_by(interval, day) %>%
  summarise(steps_median = median(steps, na.rm = TRUE) %>% ceiling %>% as.integer()) %>% ungroup()
# replace them
steps_imputed <- steps_raw %>% mutate(day = wday(date, label = TRUE, abbr = FALSE)) %>% 
  left_join(steps_median) %>% mutate(steps = ifelse(is.na(steps), steps_median, steps)) %>% 
  select(-day, -steps_median)
```

```
## Joining, by = c("interval", "day")
```

#### Make a histogram of the total number of steps taken each day


```r
steps_imputed_day <- steps_imputed %>% 
  count(date, wt = steps) 
steps_imputed_day%>% ggplot(aes(n)) + geom_histogram(binwidth = 500, fill = "forestgreen")
```

![](PA1_template_files/figure-html/steps_day_imputed_histogram-1.png)<!-- -->

#### Calculate and report the mean and median total number of steps taken per day


```r
number_steps_day_imputed_mean <- steps_imputed_day$n %>% mean %>% ceiling()
number_steps_day_imputed_median <- steps_imputed_day$n %>% median
```

The mean of the steps taken per day of the imputed data is 9706, 
and the median of the steps taken per day of the imputed data is 10395.  

The median of the imputed data is the same than the original, but the mean increased from 
9355 to 10395.  

The imputation has no effect in the median, but increased the mean.  


### Are there differences in activity patterns between weekdays and weekends?

#### Create a new factor variable in the dataset with two levels – “weekday” and “weekend”


```r
steps_imputed_enriched <- steps_imputed %>% 
  mutate(day     = wday(date, label = TRUE)) %>% 
  mutate(weekend = ifelse(day %in% c("Sat", "Sun"), "weekend", "weekday")) %>% 
  select(-day)
```

#### Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
# aggregation
steps_imputed_weekday <- steps_imputed_enriched %>% 
  group_by(interval, weekend) %>% 
  summarise(steps = mean(steps, na.rm = TRUE) %>% ceiling())
```

```r
# viz 
steps_imputed_weekday %>% 
  ggplot(aes(interval, steps, color = weekend)) +
  geom_line() + facet_grid(weekend ~ .) +
  theme(legend.position = "none") +
  labs(x = "Interval", y = "Steps", title = "Mean of steps taken by interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->


### Conclusions  




