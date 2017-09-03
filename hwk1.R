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

# 4. KDE with different kernals
par(mfrow=c(2,2))
KD5 <- density(DHSILR, kernel = "gaussian", bw = .005) 
plot(KD5, type="l",ylim=c(0,55), main="KDE of DSPLR with Gaussian Kernel")
KD6 <- density(DHSILR, kernel = "rectangular", bw = .005) 
plot(KD6, type="l",ylim=c(0,55), main="KDE of DSPLR with rectangular Kernel")
KD7 <- density(DHSILR, kernel = "triangular", bw = .005) 
plot(KD7, type="l",ylim=c(0,55), main="KDE of DSPLR with triangular Kernel")
KD8 <- density(DHSILR, kernel = "cosine", bw = .005) 
plot(KD8, type="l", ylim=c(0,55),main="KDE of DSPLR with cosine Kernel")

qplot(DHSILR, main = "Different Kernals", geom="density")+stat_density(kernal="gaussian", bw=0.005, show.legend = TRUE, position = "stack", adjust=1)
qplot(DHSILR, main = "Different Kernals", geom="density")+stat_density(kernal="rectangular", bw=0.005, show.legend = TRUE, position = "stack", adjust=1)


# 5. KDE with different
par(mfrow=c(2,2))
KD1 <- density(DSPLR, kernel = "gaussian", bw = .01) 
plot(KD1, type="l",ylim=c(0,65), main="KDE of DSPLR with BW=.01")
KD2 <- density(DSPLR, kernel = "gaussian", bw = .005) 
plot(KD2, type="l",ylim=c(0,65), main="KDE of DSPLR with BW=.005")
KD3 <- density(DSPLR, kernel = "gaussian", bw = .001) 
plot(KD3, type="l",ylim=c(0,65), main="KDE of DSPLR with BW=.001")
KD4 <- density(DSPLR, kernel = "gaussian", bw = .00001) 
plot(KD4, type="l", main="KDE of DSPLR with BW=.00001")
