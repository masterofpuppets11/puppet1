getwd()   #first check the directory
list.files()

library(ggplot2)
load("multiplot.Rdata")


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
qplot(DHSILR, main = "Histogram of Log Returns for HSI") + geom_histogram(bins = 100, binwidth = 0.01)+geom_density(aes(DHSILR), color="gray", na.rm = TRUE)

# 4. KDE with different kernals
#-----------par(mfrow=c(2,2))
#class------KD5 <- density(DHSILR, kernel = "gaussian", bw = .005) 
#ref--------plot(KD5, type="l",ylim=c(0,55), main="KDE of DSPLR with Gaussian Kernel")
#code->-----KD6 <- density(DHSILR, kernel = "rectangular", bw = .005) 
#-----------plot(KD6, type="l",ylim=c(0,55), main="KDE of DSPLR with rectangular Kernel")
#-----------KD7 <- density(DHSILR, kernel = "triangular", bw = .005) 
#-----------plot(KD7, type="l",ylim=c(0,55), main="KDE of DSPLR with triangular Kernel")
#-----------KD8 <- density(DHSILR, kernel = "cosine", bw = .005) 
#-----------plot(KD8, type="l", ylim=c(0,55),main="KDE of DSPLR with cosine Kernel")

kdek1 <- qplot(DHSILR, main = "Gaussian Kernal", geom="density")+stat_density(kernal="gaussian", bw=0.005, show.legend = TRUE, adjust=1)
kdek2 <- qplot(DHSILR, main = "Rectangular Kernal", geom="density")+stat_density(kernal="rectangular", bw=0.005, show.legend = TRUE, position = "stack", adjust=1)
kdek3 <- qplot(DHSILR, main = "Triangular Kernal", geom="density")+stat_density(kernal="triangular", bw=0.005, show.legend = TRUE, position = "stack", adjust=1)
kdek4 <- qplot(DHSILR, main = "Cosine Kernal", geom="density")+stat_density(kernal="cosine", bw=0.005, show.legend = TRUE, position = "stack", adjust=1)

multiplot(kdek1,kdek2,kdek3,kdek4, cols=2)

# 5. KDE with different
#-----------par(mfrow=c(2,2))
#-----------KD1 <- density(DSPLR, kernel = "gaussian", bw = .01) 
#class------plot(KD1, type="l",ylim=c(0,65), main="KDE of DSPLR with BW=.01")
#ref--------KD2 <- density(DSPLR, kernel = "gaussian", bw = .005) 
#code->-----plot(KD2, type="l",ylim=c(0,65), main="KDE of DSPLR with BW=.005")
#-----------KD3 <- density(DSPLR, kernel = "gaussian", bw = .001) 
#-----------plot(KD3, type="l",ylim=c(0,65), main="KDE of DSPLR with BW=.001")
#-----------KD4 <- density(DSPLR, kernel = "gaussian", bw = .00001) 
#-----------plot(KD4, type="l", main="KDE of DSPLR with BW=.00001")

kdeb1 <- qplot(DHSILR, main = "Different binwidth test BW = 0.01")+stat_density(kernal="gaussian", bw=0.01, show.legend = TRUE, adjust=1)+ylim(0,65)
kdeb2 <- qplot(DHSILR, main = "Different binwidth test BW = 0.005")+stat_density(kernal="gaussian", bw=0.005, show.legend = TRUE, adjust=1)+ylim(0,65)
kdeb3 <- qplot(DHSILR, main = "Different binwidth test BW = 0.001")+stat_density(kernal="gaussian", bw=0.001, show.legend = TRUE, adjust=1)+ylim(0,65)
kdeb4 <- qplot(DHSILR, main = "Different binwidth test BW = 0.00001")+stat_density(kernal="gaussian", bw=0.00001, show.legend = TRUE, adjust=1)+ylim(0,65)

multiplot(kdeb1,kdeb2,kdeb3,kdeb4, cols=2)
