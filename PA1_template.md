---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
--- 


### Loading and preprocessing the data


```r
library(tidyverse)
library(knitr)
library(lubridate)
# shown code, and cache results the default
knitr::opts_chunk$set(echo = TRUE, cache = TRUE)
```

```r
# read_csv can read zip files
data_raw <- read_csv("activity.zip")
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


```r
# first get total steps by date, then average them by weekday
steps_day_mean <- data_raw %>% 
  count(date, wt = steps) %>%
  mutate(day = wday(date, label = TRUE, abbr = F)) %>% 
  group_by(day) %>% summarise(mean_day = mean(n, na.rm = TRUE))
# time series
data_raw %>% count(date, wt = steps) %>% plot(type = "b", pch = 19, cex = .5); grid()
```

![](PA1_template_files/figure-html/viz_steps_day-1.png)<!-- -->

```r
# barplot of the mean of the total steps taken each day
ggplot(steps_day_mean, aes(day, mean_day)) +
  geom_bar(stat = "identity", fill = "forestgreen") +
  labs(x = "Day of the week", y = "Steps", 
       title = "Mean of steps taken per day")
```

![](PA1_template_files/figure-html/viz_steps_day-2.png)<!-- -->


### What is the average daily activity pattern?



### Imputing missing values

I will use the median to impute missing values

```r
data_raw %>% filter(is.na(steps)) %>% count(date)
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




