getwd()   #first check the directory
list.files()

# Creating a name for the table
HSENG <- read.table(file = "DHSI.csv", header = T, sep = ",")

# Generating descriptive stats
attach(HSENG); summary(HSENG); names(HSENG)

# Setting the variables and extracting columns
HSI <- rev(HSENG$Close)
HSI_Time <- seq(from=1986, to=2017, length.out = length(HSI))

# Computing Log Returns
DHSILR <- diff(log(HSI))

# 1. Plotting the log returns of HSI
plot(DSP_time[2:length(DSP)], DSPLR,type="l",xlab="Date",main="Daily log return of S&P500 from Jan 1960 to July 2017")

# 2. Plotting Histogram
par(mfrow=c(2,2))  # to divide the plotting area into 2 by 2  
hist(DHSILR, breaks = 20, freq = F, main="Histogram of DHSILR, #bins = 20")   
# try help(hist) to learn its options
hist(DHSILR, breaks=50, freq = F,main="Histogram of DHSILR, #bins = 50") 
hist(DHSILR,breaks=500,  freq = F,main="Histogram of DHSILR, #bins = 500")   
hist(DHSILR,breaks=5000, freq = F,main="Histogram of DHSILR, #bins = 5000")

# 3. Histogram with superimposed normal curve
par(mfrow=c(1,1)) 
hist(DHSILR,breaks=100,  freq = F,main="Histogram of DSPLR vs Fitted Normal Density")   

mu_DHSILR <- mean(DHSILR)
sd_DHSILR <- sd(DHSILR)

x<-seq(-0.2,0.1,by=0.001)
y<-dnorm(x,mean=mu_DHSILR,sd = sd_DHSILR)
points(x,y,type="l",col="red")

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

# 6. Computing Empirical VaR and VaR under normal Assumptions for q=0.01 and 0.001
par(mfrow=c(2,2))
# based on empirical
q1 <- 0.01
q2 <- 0.001
VaR_emp_01 <- -quantile(DHSILR,q1)
VaR_emp_001 <- -quantile(DHSILR,q2)
VaR_emp_01
VaR_emp_001
# based on normal
mu_DHSILR <- mean(DHSILR)
sd_DHSILR <- sd(DHSILR)

VaR_normal_01 <- qnorm(q1,mu_DHSILR, sd_DHSILR) # Generating a quantile function (for q=0.01) of normal dist. with mu & SD similar to data
VaR_normal_001 <- qnorm(q2,mu_DHSILR, sd_DHSILR) # Generating a quantile function (for q=0.001) of normal dist. with mu & SD similar to data
VaR_normal_01
VaR_normal_001

# 7. Computing empirical expected shortfall and expected shortfall under normal assumptions (q=0.01 and q=0.001)

# Empirical expected shortfall for q=0.01 and q=0.001
ES_emp_01 <- mean(- DHSILR[- DHSILR > VaR_emp_01])   # Mean of (Extracting all the negative values of DHSILR whichare greater than VaR_emp)
ES_emp_001 <- mean(- DHSILR[- DHSILR > VaR_emp_001])


# Generating normal distribution similar to DHSILR
N<-100000  # variable for number of observations
X<-rnorm(N,mu_DHSILR,sd_DHSILR) # vector random normal distribution with mu & SD same as data
ES_normal_01 <- mean( - X[- X > VaR_normal_01]) # Mean of Values of vector X whose negative values exceed VaR normal for q-quantile of 0.01
ES_normal_001 <- mean( - X[- X > VaR_normal_001]) # Mean of Values of vector X whose negative values exceed VaR normal for q-quantile of 0.01

c(ES_emp_01, ES_normal_01) # tells us that the there is 1% probablility that we lose more than 7.12% 
c(ES_emp_001, ES_normal_001) # tells us that the there is 0.1% probablility that we lose more than 17.1% 

# Part II Problem from book (p 1.2, page 63)
    # 1. Generating exp distribution
    N <- 1024
    X <- rexp(N, rate = 0.2)
    mu_X <- mean(X)
    sd_X <- sd(X)
    x1<-seq(-0.2,0.1,by=0.001)
    y2<-dnorm(X,mean=mu_X,sd = sd_X)
    
    help("dnorm")

    # 2. Creating histograms with Density Estimations
    par(mfrow=c(2,2))  # to divide the plotting area into 2 by 2  
    
    hist(X, breaks = 20, freq = F, main="Histogram of DHSILR, #bins = 20")  
    lines(y2,type="l",col="red")
    hist(X, breaks=50, freq = F,main="Histogram of DHSILR, #bins = 50") 
    lines(y2,type="l",col="red")
    hist(X,breaks=500,  freq = F,main="Histogram of DHSILR, #bins = 150")   
    lines(y2,type="l",col="red")
    hist(X,breaks=5000, freq = F,main="Histogram of DHSILR, #bins = 500")
    lines(y2,type="l",col="red")
    
    #3. Creating histograms with KDE
    par(mfrow=c(2,2))  # to divide the plotting area into 2 by 2  
    
    hist(X, breaks = 20, freq = F, main="Histogram of DHSILR, #bins = 20")   
    lines(y2,type="l",col="blue", lty='dotted')
    lines(density(X), kernal='gaussian', bw=0.01, col='red')
    
    
    hist(X, breaks=50, freq = F,main="Histogram of DHSILR, #bins = 50") 
    lines(y2,type="l",col="blue", lty='dotted')
    lines(density(X), kernal='gaussian', bw=0.01, col='red')
    
    hist(X,breaks=500,  freq = F,main="Histogram of DHSILR, #bins = 150")   
    lines(y2,type="l",col="blue", lty='dotted')
    lines(density(X), kernal='gaussian', bw=0.01, col='red')
    
    hist(X,breaks=5000, freq = F,main="Histogram of DHSILR, #bins = 500")
    lines(y2,type="l",col="blue", lty='dotted')
    lines(density(X), kernal='gaussian', bw=0.01, col='red')
    
    # The normal density understimates the density of the data, and KDE is a more accurate fit to the data
    





