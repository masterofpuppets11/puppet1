getwd()   #first check the directory
list.files()

library(ggplot2)

# Creating a name for the table
HSENG <- read.table(file = "DHSI.csv", header = T, sep = ",")

# Generating descriptive stats first
attach(HSENG); summary(HSENG); names(HSENG)

# Setting the variables and extracting columns
HSI <- rev(HSENG$Close)
HSI_Time <- seq(from=1986, to=2017, length.out = length(HSI))

# Computing Log Returns
DHSILR <- diff(log(HSI))

# 1. Plotting the log returns of HSI
qplot(HSI_Time[0:length(DHSILR)], DHSILR,xlab ="Date",main="Daily log return of HSI from Jan 1986 to July 2017") + geom_line()

# 2. Plotting Histogram
qplot(DHSILR, main = "Histogram of Log Returns for HSI") + geom_histogram(bins = 20)
qplot(DHSILR, main = "Histogram of Log Returns for HSI") + geom_histogram(bins = 50)
qplot(DHSILR, main = "Histogram of Log Returns for HSI") + geom_histogram(bins = 500)
qplot(DHSILR, main = "Histogram of Log Returns for HSI") + geom_histogram(bins = 5000)

# 3. Histogram with superimposed normal curve
qplot(DHSILR, main = "Histogram of Log Returns for HSI") + geom_histogram(bins = 100, binwidth=0.01)+geom_density(aes(DHSILR), color="gray", na.rm = TRUE)

