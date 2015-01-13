---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
unzip("activity.zip")
df <- read.csv("activity.csv", stringsAsFactors = FALSE)
```

## What is mean total number of steps taken per day?

The following codes generates and displays a Histogram 
of the number of the number of steps taken each day.

 

```r
drawHist <- function(dat = df){
	require(ggplot2)    
	g <- ggplot(data = dat, aes(x=date)) 
	g <- g + geom_histogram(aes(weights=steps),
			fill="blue")
	g <- g + labs(x = "Date", y = "Number of Steps")
	g <- g + labs(title = "Number of Steps per day")
	g <- g + scale_x_discrete(labels="")
	g
	}
	
drawHist(df)
```

![plot of chunk histogram](figure/histogram-1.png) 

The following code calculates the mean and median of
the total number of steps taken per day.


```r
# Gets a vector of unique dates.
dates = unique(df$date)

# Function that gets the total number of
# steps for each day.
sumD <- function(d, dat=df)
	sum (dat$steps[dat$date == d],
		 na.rm = TRUE)

#Vector containing the totals.
totals <- sapply(dates, 
			function(x) sumD(x, df))

mn <- mean(totals)
md <- median(totals)
```

The mean and median of total number of steps taken each day are
 9354.2295082 and 10395 respectively.  
 

## What is the average daily activity pattern?

The following code generates and displays a time series 
plot containing the average daily number of steps
for each 5-minutes interval over all the days.



```r
# Gets a vector of unique dates.
intervals = unique(df$interval)

# Function that gets the mean of
# each Interval.
meaI <- function(i) 
		mean (df$steps[df$interval == i],
			 na.rm = TRUE)

# Generates a new data frame with
# each mean.
Mdf = data.frame(intervals,
	mean = sapply(intervals, meaI)
		)
		
# Draws time series plot.
g <- ggplot(Mdf,
	aes(x = intervals,
		y = mean)
	)

g <- g + geom_line(color="red")
g <- g + labs(x = "Interval", y = "Number of Steps")
g <- g + labs(title = "Average Number of Steps per 5 min. Interval.")
g <- g + scale_x_discrete(labels="")
g
```

![plot of chunk medians2](figure/medians2-1.png) 

* The five minute interval that, on average, contains the maximum number
of steps is 835.

## Imputing missing values

The following code calculates the number of missing values and fills them
with the average for that 5-minute interval.


```r
# Generates Vector of NAs.
nAs = is.na(df$steps)

# Function that returns the average number of steps
# for a given interval if it's NA.
avSteptGI <- function (i) {
	j = 1
	
	for (intv in Mdf$intervals) {
		if (intv == i) return (Mdf$mean[j])
		j = j + 1
	}
}

# Creates a new data frame with the original
# information.
dfImp <- df

# Substitutes NAs for the average number of 
# steps at the corresponding interval.
j = 1
for (i in dfImp$interval) {
	if (nAs[j]) dfImp$steps[j] <- avSteptGI(i)
	j <- j + 1
}
```
* The total number of missing values is 2304.  

The following codes displays the histogram of the number of the number of steps taken each day
 with the corresponding imputed values.
 

```r
drawHist(dfImp)
```

![plot of chunk HistImputed](figure/HistImputed-1.png) 

The following computes the mean and median of
total number of steps taken each day.    
 

```r
#Vector containing the totals.
totalsImp <- sapply(dates, 
				function(x) sumD(x, dfImp))
				
mnImp <- mean(totalsImp)
mdImp <- median(totalsImp)
```

The mean and median of total number of steps taken each day are
 1.0766189 &times; 10<sup>4</sup> and 1.0766189 &times; 10<sup>4</sup> respectively.  
 
The difference between the mean and mean of the two data sets are 
1411.959171 and 371.1886792 respectively. As we can see, without surprise,
 adding (positive) values to the data increases both the mean and mean.
 From the histogram we can see that the differences are mainly concentrated 
 on a few days.  

## Are there differences in activity patterns between weekdays and weekends?

The following R script creates a new factor variable with two values (levels): 
"weekday" and "weekend" and adds it to the data set.


```r
i = 1
days <- c()
df$date <- as.POSIXct(df$date) 
df$day <- weekdays(df$date)
	
# The following function decides between 
# weekend and weekday 
# Note: Weekdays names are in Spanish.
dayWeek <- function (d) {
	if( d == "domingo" | d == "sábado") 
		return ("weekend")
	"weekday"
}
	
df$day <- as.factor(sapply(df$day, dayWeek))
```

The following creates and displays a time series plot 
 of the 5-minute interval (x-axis) and the average number 
of steps taken, averaged across all weekday days or weekend days (y-axis).  


```r
# Function that computes the mean of
# each date.
meanD <- function(i, w){
		mean (
			df$steps[
				df$interval == i & df$day == w],
				na.rm = TRUE)
	}
	
# Vectors of means.
meansWD <- sapply(intervals, 
			function (x) meanD(x, "weekday") )
names(meansWD) <- intervals

meansWE <- sapply(intervals, 
			function (x) meanD(x, "weekend") )
names(meansWE) <- intervals

df$date <- as.character(df$date)

# Adds the means to the data frame.
j = 1
for (i in df$interval){
	if (df$day[j] == "weekday")
		df$mean[j] <- meansWD[as.character(i)]
	else
		df$mean[j] <- meansWE[as.character(i)]
	j = j + 1
	}

# Draws time series plot.
g <- ggplot(df,
	aes(x = interval,
		y = mean)
	)
g <- g + geom_line(color="blue")
g <- g + labs(x = "Interval", y = "Average Number of Steps")
g <- g + labs(title = "Average Number of Steps per 5-minute interval.")
g <- g + facet_grid(day~.)
g
```

![plot of chunk weekdayPlot](figure/weekdayPlot-1.png) 
