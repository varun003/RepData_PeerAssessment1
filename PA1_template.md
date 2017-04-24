---
title: "reproducible week 2"
author: "Varun Negi"
date: "24 April 2017"
output: html_document
---




```r
setwd("C:/Users/VARUN/Desktop/s")
```




```r
dat <- fread("activity.csv")

dat$date <- as.Date(dat$date, format = "%Y-%m-%d")
```

# Calculate the total number of steps per day 

```r
steps <- dat %>%
  filter(!is.na(steps)) %>%
  group_by(date) %>%
  summarize(steps = sum(steps))
```

1. Using ggplot for making histogram

```r
ggplot(steps, aes(x = steps)) +
  geom_histogram(fill = "firebrick", binwidth = 1000) +
  labs(title = "Histogram of Steps per day", x = "Steps per day", y = "Frequency")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

2. Calculate the mean and median of the total number of steps taken per day

```r
mean_steps <- mean(steps$steps, na.rm = TRUE)
median_steps <- median(steps$steps, na.rm = TRUE)

median_steps
```

```
## [1] 10765
```

```r
mean_steps
```

```
## [1] 10766.19
```

## Average daily activity pattern

1. Calculate the average number of steps taken in each 5-minute interval per day 


```r
interval <- dat %>%
  filter(!is.na(steps)) %>%
  group_by(interval) %>%
  summarize(steps = mean(steps))

ggplot(interval, aes(x=interval, y=steps)) +
  geom_line(color = "firebrick")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps


```r
interval$interval[which.max(interval$steps)]
```

```
## [1] 835
```
## Imputing missing values


```r
dat2 <- dat

dat2$steps[is.na(dat2$steps)] <- mean(dat2$steps,na.rm = T)

sum(is.na(dat2$steps))
```

```
## [1] 0
```
1. Calculate the number of steps taken in each 5-minute interval per day 


```r
steps_full <- dat2 %>%
  filter(!is.na(steps)) %>%
  group_by(date) %>%
  summarize(steps = sum(steps))

ggplot(steps_full, aes(x = steps)) +
  geom_histogram(fill = "firebrick", binwidth = 1000) +
  labs(title = "Histogram of Steps per day, including missing values", x = "Steps per day", y = "Frequency")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)

```r
mean_steps_full <- mean(steps_full$steps, na.rm = TRUE)
median_steps_full <- median(steps_full$steps, na.rm = TRUE)

mean_steps_full
```

```
## [1] 10766.19
```

```r
median_steps_full
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends


```r
dat2 <- mutate(dat2, weektype = ifelse(weekdays(dat2$date) == "Saturday" | weekdays(dat2$date) == "Sunday", "weekend", "weekday"))
dat2$weektype <- as.factor(dat2$weektype)
head(dat2)
```

```
##     steps       date interval weektype
## 1 37.3826 2012-10-01        0  weekday
## 2 37.3826 2012-10-01        5  weekday
## 3 37.3826 2012-10-01       10  weekday
## 4 37.3826 2012-10-01       15  weekday
## 5 37.3826 2012-10-01       20  weekday
## 6 37.3826 2012-10-01       25  weekday
```

```r
interval_full <- dat2 %>%
  group_by(interval, weektype) %>%
  summarise(steps = mean(steps))

s <- ggplot(interval_full, aes(x=interval, y=steps, color = weektype)) +
  geom_line() +
  facet_wrap(~weektype, ncol = 1, nrow=2)
print(s)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)


