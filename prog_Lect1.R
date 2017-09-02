rm(list = ls())
setwd(...)  # set directory


SP500 <- read.table("DSP500.csv",header = T, sep=",")

attach(SP500)
summary(SP500)
names(SP500)

DSP<-rev(SP500$Close)

DSP_time<- seq(from=1960,to=2017,length.out=length(DSP)) 

plot(DSP_time,DSP,type="l",xlab="Date",main="Daily S&P500 index from Jan 1960 to July 2017")

DSPLR <- diff(log(DSP))  # compute log returns; 
                         # try help(diff) for the usage of diff

plot(DSP_time[2:length(DSP)], DSPLR,type="l",xlab="Date",main="Daily log return of S&P500 from Jan 1960 to July 2017")

par(mfrow=c(1,2))  
# divide the plotting area into 1 by 2 to facilitate comparison 
plot(DSP_time,DSP,type="l",xlab="Date",main="Daily S&P500 index from Jan 1960 to July 2017")
plot(DSP_time[1:length(DSPLR)], DSPLR,type="l",xlab="Date",main="Daily log return of S&P500 from Jan 1960 to July 2017")

qqnorm(DSPLR, main="Normal QQ plot of DSLPR")

###################
Citi<-read.table("Citi_13Jan.txt",head=T)
attach(Citi)
head(Citi)

price <- as.numeric(as.character(PRICE))
t<-as.character(TIME)
t<-strptime(t,"%H:%M:%S")

summary(t)
summary(price)

plot(t,price,type="l",xlab="Time",main="Price of Citi on Jan 2, 2013")

# next compute minute-by-minute returns
Dt<-60 #every 6 sec

tn<-as.numeric(difftime(t,t[1],units="secs")) #t numeric, tn[1]=0
b<-floor(tn/Dt)
FirstPerDt<- b - c(-1,b[-length(b)]) #nonzero entries are the first ones per Dt

price_60sec <- price[FirstPerDt != 0]
tn1_60sec<-tn[FirstPerDt != 0]

Citi_LR60sec <- diff(log(price_60sec))  # compute log returns; 

plot(tn1_60sec[-1], Citi_LR60sec,type="l",xlab="Time",main="Intraday minute-by-minute returns of Citi on Jan 2, 2013")

par(mfrow=c(1,1))  
hist(Citi_LR60sec[Citi_LR60sec < 0.004], freq = F, main="Histogram of Citi minute-by-minute returns")  

qqnorm(Citi_LR60sec[Citi_LR60sec < 0.004], main="Normal QQ plot of Citi minute-by-minute returns")


################# Coffee data
library(Rsafd)

data(BCofLRet)
data(CCofLRet)

CofLR <- cbind(BCofLRet,CCofLRet)

summary(BCofLRet)
summary(CCofLRet)

plot(BCofLRet,CCofLRet)

##### BH vs SP
BH_SP <- read.table("SP_BH.CSV",head=T, sep=",")
attach(BH_SP)
head(BH_SP)
SP <- BH_SP$SP
BH <- BH_SP$BH

SP_rtn <- diff(log(SP))
BH_rtn <- diff(log(BH))

par(mfrow=c(1,1))
plot(SP, BH,main="BH price vs SP500 index")
plot(SP_rtn, BH_rtn,main="BH return vs SP500 return")

##################
n<-1000
rtn1 <- rnorm(n,0.1,1)
rtn2 <- rnorm(n,0.1,1)

rw1 <- cumsum(rtn1)
rw2 <- cumsum(rtn2)

par(mfrow=c(1,2))
plot(rw1,rw2,main="Price")
plot(rtn1,rtn2,main="return")


##### common distributions

# normal
x<-seq(-4,4,by=0.01)
plot(x,pnorm(x),type="l",lwd=2 )

plot(x,dnorm(x),type="l",lwd=2 )

x<-seq(-5,5,by=0.01)
plot(x,dnorm(x),type="l",lwd=3)
lines(x,dnorm(x,mean=2),lty=2,lwd=3,col="red")
lines(x,dnorm(x,mean=0,sd=2),lty=5,lwd=3,col="blue")
legend("topright", c("N(0,1)","N(2,1)","N(0,4)"),lty=c(1,2,5),lwd=c(3,3,3),col=c("black","red","blue"),text.col=c("black","red","blue"))

# uniform
x<-seq(-0.5,1.5,by=0.01)
par(mfrow=c(1,2))
plot(c(0,1),c(1,1),xlim=c(-0.5,1.5),ylim=c(0,1.1),type="l",lwd=2,ylab="dunif(x)",xlab="x")
lines(c(-0.5,0),c(0,0),lwd=2)
lines(c(1,1.5),c(0,0),lwd=2)

plot(x,punif(x),type="l",lwd=2)

# exponential
x<-seq(0,5,by=0.01)
par(mfrow=c(1,1))
plot(x,dexp(x),type="l",lwd=3)
lines(x,dexp(x,rate=2),lty=2,lwd=3,col="red")
lines(x,dexp(x,rate=0.5),lty=5,lwd=3,col="blue")
legend("topright", c("Exp(1)","Exp(2)","Exp(0.5)"),lty=c(1,2,5),lwd=c(3,3,3),col=c("black","red","blue"),text.col=c("black","red","blue"))


# Cauchy
x<-seq(-5,5,by=0.01)
plot(x,dcauchy(x),type="l",lwd=3)
lines(x,dcauchy(x,location=2),lty=2,lwd=3,col="red")
lines(x,dcauchy(x,scale=2),lty=5,lwd=3,col="blue")
legend("topright", c("C(0,1)","C(2,1)","C(0,2)"),lty=c(1,2,5),lwd=c(3,3,3),col=c("black","red","blue"),text.col=c("black","red","blue"))

# Normal vs Cauchy
x<-seq(-5,5,by=0.01)
plot(x,dnorm(x),type="l",lwd=3,ylab="")
lines(x,dcauchy(x),lty=2,lwd=3,col="red")
legend("topright", c("N(0,1)","C(0,1)"),lty=c(1,2),lwd=c(3,3),col=c("black","red"),text.col=c("black","red"))


################# 

# histogram
par(mfrow=c(2,2))  # to divide the plotting area into 2 by 2  
hist(DSPLR, breaks = 20, freq = F, main="Histogram of DSPLR, #bins = 20")   
   # try help(hist) to learn its options
hist(DSPLR, breaks=50, freq = F,main="Histogram of DSPLR, #bins = 50") 
hist(DSPLR,breaks=500,  freq = F,main="Histogram of DSPLR, #bins = 500")   
hist(DSPLR,breaks=5000, freq = F,main="Histogram of DSPLR, #bins = 5000")

# histogram
par(mfrow=c(1,1)) 
hist(DSPLR,breaks=100,  freq = F,main="Histogram of DSPLR vs Fitted Normal Density")   

mu_DSPLR <- mean(DSPLR)
sd_DSPLR <- sd(DSPLR)

x<-seq(-0.2,0.1,by=0.001)
y<-dnorm(x,mean=mu_DSPLR,sd = sd_DSPLR)
points(x,y,type="l",col="red")


# kernel density


# different kernel
par(mfrow=c(2,2))
KD5 <- density(DSPLR, kernel = "gaussian", bw = .005) 
plot(KD5, type="l",ylim=c(0,55), main="KDE of DSPLR with Gaussian Kernel")
KD6 <- density(DSPLR, kernel = "rectangular", bw = .005) 
plot(KD6, type="l",ylim=c(0,55), main="KDE of DSPLR with rectangular Kernel")
KD7 <- density(DSPLR, kernel = "triangular", bw = .005) 
plot(KD7, type="l",ylim=c(0,55), main="KDE of DSPLR with triangular Kernel")
KD8 <- density(DSPLR, kernel = "cosine", bw = .005) 
plot(KD8, type="l", ylim=c(0,55),main="KDE of DSPLR with cosine Kernel")


# different bandwidth
par(mfrow=c(2,2))
KD1 <- density(DSPLR, kernel = "gaussian", bw = .01) 
plot(KD1, type="l",ylim=c(0,65), main="KDE of DSPLR with BW=.01")
KD2 <- density(DSPLR, kernel = "gaussian", bw = .005) 
plot(KD2, type="l",ylim=c(0,65), main="KDE of DSPLR with BW=.005")
KD3 <- density(DSPLR, kernel = "gaussian", bw = .001) 
plot(KD3, type="l",ylim=c(0,65), main="KDE of DSPLR with BW=.001")
KD4 <- density(DSPLR, kernel = "gaussian", bw = .00001) 
plot(KD4, type="l", main="KDE of DSPLR with BW=.00001")



# histogram and Kernel
par(mfrow=c(2,1))
hist(DSPLR,breaks=50,  freq = F,main="Histogram & KDE of DSPLR, #bin = 50, bw=0.01",ylim=c(0,65))   
KD4 <- density(DSPLR, kernel = "gaussian", bw = .01) 
points(KD4,type="l",col="red")

KD9 <- density(DSPLR, kernel = "gaussian", bw = .001) 
hist(DSPLR,breaks=50,  freq = F,main="Histogram & KDE of DSPLR, #bin = 50, bw=0.001",ylim=c(0,65))   
points(KD9,type="l",col="red")


# default choice of bw
KD0 <- density(DSPLR, kernel = "gaussian") 
hist(DSPLR,breaks=50,  freq = F,main="Histogram & KDE of DSPLR, #bin = 50, bw=default",ylim=c(0,65))   
points(KD0,type="l", col="red")


KD0
names(KD0)
KD0$bw


# QQ plots
q<-c(.01,.025,.05,.1,.15,.25,.5,0.75,0.85,0.9,0.95,0.975,0.99)
q_norm <- round(qnorm(q),3)
q_cauchy <- round(qcauchy(q),3)
q_exp <- round(qexp(q),3)
rbind(q,q_norm,q_cauchy,q_exp)

par(mfrow=c(1,2))
plot(q_norm,q_exp,main="Exp(1) Quantiles vs N(0,1) Quantiles")
plot(q_norm,q_cauchy,main="Cauchy(0,1) Quantiles vs N(0,1) Quantiles")

q <- seq(0.001,0.999,by=0.001)
q_norm1 <- qnorm(q)
q_norm2 <- qnorm(q,3,6)
q_cauchy <- round(qcauchy(q),3)
q_exp <- round(qexp(q),3)

par(mfrow=c(1,2))
plot(q_norm1,q_exp,main="QQ: Exp(1) vs N(0,1)",xlab="",ylab="")
plot(q_norm1,q_cauchy,main="QQ: Cauchy(0,1) vs N(0,1)",xlab="",ylab="")


# Heavy tail
par(mfrow=c(1,1))

x<-seq(3,6,by=0.01)
y_n <- dnorm(x)
y_c <- dcauchy(x)

plot(x,y_c,ylim=c(0,0.031), type="l", main="Right Tails of N(0,1) and Cauchy(0,1)", ylab="Densities",lty=1,lwd=2)
lines(x,y_n,lty=4,col="red",lwd=2)

legend("topright", c("N(0,1)","Cauchy(0,1)"), text.col=c("red","black"),
       lty = c(4,1),lwd=c(2,2), col=c("red","black"))

par(mfrow=c(1,1))
plot(q_exp,q_cauchy,main="QQ: Cauchy(0,1) vs Exp(1)",xlab="",ylab="")

plot(q_norm1,q_norm2,main="QQ: N(3,36) vs N(0,1)",xlab="",ylab="")
abline(3,6,col="red") # draw a straight line

