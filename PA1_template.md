---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
--- 


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

The mean of steps taken each day is 9354.2295082, and the median is 10395


### What is the average daily activity pattern?



```r
steps_interval <- steps_raw %>% 
  group_by(interval) %>% summarise(steps = mean(steps, na.rm = TRUE) %>% ceiling)
# most active interval
top_interval <- steps_interval %>% arrange(desc(steps)) %>% head(1) %>% extract2(c(1, 1))
```

```r
steps_interval$steps %>% plot(type = "l", ann = FALSE)
title(xlab = "Interval", ylab = "Steps taken", main = "Mean of steps take by interval")
grid()
```

![](PA1_template_files/figure-html/viz_steps_interval-1.png)<!-- -->



#### The 5-minute interval that, on average, contains the maximum number of steps
The interval 835 contains the maximum number of steps.


### Imputing missing values

I will use the median to impute missing values

```r
steps_raw %>% filter(is.na(steps)) %>% count(date)
```

```
## # A tibble: 8 x 2
##         date     n
##       <date> <int>
## 1 2012-10-01   288
## 2 2012-10-08   288
## 3 2012-11-01   288
## 4 2012-11-04   288
## 5 2012-11-09   288
## 6 2012-11-10   288
## 7 2012-11-14   288
## 8 2012-11-30   288
```



### Are there differences in activity patterns between weekdays and weekends?



### Conclusions  




