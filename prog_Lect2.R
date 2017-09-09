rm(list = ls())
setwd("...")  # set directory


# edf 
N <- c(10, 50, 100, 1000)

par(mfrow=c(1,1))
X <- rnorm(N[1])
sort(X)
edf_normal <- ecdf(X)
plot(edf_normal, xlim=c(-4,4), main = paste("EDF of N(0,1) with ", bquote(.(N[1])), " observations"))

par(mfrow=c(2,2))
for (i in 1:4)
{
  X <- rnorm(N[i])
  edf_normal <- ecdf(X)
  plot(edf_normal, xlim=c(-4,4), main = paste("EDF of N(0,1) with ", bquote(.(N[i])), " observations"))
}

x<-seq(-3,3,by=0.001)
y<-pnorm(x)
lines(x,y,col="red")


# empirical QQ
N <- c(10, 50, 100, 1000)

par(mfrow=c(2,2))
for (i in 1:4)
{
  X <- rnorm(N[i])
  qqnorm(X,  main = paste("Empirical Q-Q Plot of N(0,1) with ", bquote(.(N[i])), " observations"))
}
abline(0,1,col="red")


# empirical QQ for DSPLR
SP500 <- read.table("DSP500.csv",header = T, sep=",")

attach(SP500)
DSP<-rev(SP500$Close)
DSPLR <- diff(log(DSP))  # compute log returns; 
                         # try help(diff) for the usage of diff

n <- length(DSPLR)
q <- seq(1/(n+1),n/(n+1),by=1/n)
q_cauchy <- qcauchy(q)

par(mfrow = c(1,2))


qqnorm(DSPLR,main="Empirical Quantiles of DSPLR vs N(0,1)",xlab="N(0,1) quantitles", ylab="DSPLR quantiles")
qqplot(q_cauchy, DSPLR, main="Empirical Quantiles of DSPLR vs Cauchy(0,1)",xlab="Cauchy(0,1) quantitles", ylab="DSPLR quantiles")


# compare the right/left tails
par(mfrow=c(1,1))
DSPLR_N <- - DSPLR[DSPLR < 0]
DSPLR_P <- DSPLR[DSPLR > 0]
qqplot(DSPLR_P, DSPLR_N)
abline(0,1,col="red")
# negative returns have a bit heavier tail


# Compute VaR under different distribution assumptions
# based on empirical distribution
q <- 0.01 
VaR_emp <- -quantile(DSPLR,q)
VaR_emp

# if based on normal
mu_DSPLR <- mean(DSPLR)
sd_DSPLR <- sd(DSPLR)

VaR_normal <- - qnorm(q,mu_DSPLR, sd_DSPLR)
VaR_normal 

# if based on Cauchy
m_DSPLR <- median(DSPLR)
lam_DSPLR <- 1/2 * (quantile(DSPLR,3/4) - quantile(DSPLR,1/4))
c(m_DSPLR, lam_DSPLR)

# Or: MLE fitting
library("MASS") 
fitdistr(DSPLR,"cauchy") 

VaR_cauchy <- - qcauchy(q,m_DSPLR, lam_DSPLR)
VaR_cauchy

c(VaR_normal, VaR_cauchy)


# empirical VaR vs VaR_normal
q<-0.01
VaR_normal <- - qnorm(q,mu_DSPLR, sd_DSPLR)
VaR_emp <- -quantile(DSPLR,q)
c(VaR_normal, VaR_emp)

q <- seq(0.001,0.01,by=0.0002)
q_DSPLR <- quantile(DSPLR,q)
q_norm <- qnorm(q,mu_DSPLR,sd_DSPLR)

par(mfrow=c(1,2))

plot(q,q_DSPLR,ylim=c(-0.07,-0.02),main="Left Tails: DSPLR & Normal",xlab="q",ylab="Quantiles",pch=19)
points(q,q_norm,col="red",pch=23)
legend("bottomright", pch=c(19,23),c("DSPLR","Fitted Normal"), text.col=c("black","red"))

par(mfrow=c(1,2))
plot(q,-q_DSPLR,ylim=c(0.02,0.07),main="VaR based on normal and empirical VaR",xlab="q",ylab="",pch=19)
points(q,-q_norm,col="red",pch=23)
legend("topright", pch=c(19,23),c("DSPLR","Fitted Normal"), text.col=c("black","red"))
plot(q,q_norm/q_DSPLR, main="Ratios between VaR based on normal and empirical VaR ",ylab="Ratios")



# Monte-Carlo for computing expected shortall

q<-0.01
VaR_emp <- - quantile(DSPLR,q)
ES_emp <- mean(- DSPLR[- DSPLR > VaR_emp])

 
# to understand the usage of [ ], try
A<-c(1,2,3)
B<-A[A<=2]
B


mu_DSPLR <- mean(DSPLR)
sd_DSPLR <- sd(DSPLR)
VaR_normal <-  - qnorm(q,mu_DSPLR, sd_DSPLR)


N<-100000
X<-rnorm(N,mu_DSPLR,sd_DSPLR)
ES_normal <- mean( - X[- X > VaR_normal])

c(ES_emp, ES_normal)


q<-0.001

VaR_normal <- - qnorm(q,mu_DSPLR, sd_DSPLR)
VaR_emp <- - quantile(DSPLR,q)
ES_emp <- mean(- DSPLR[- DSPLR > VaR_emp])

N<-1000000
X<-rnorm(N,mu_DSPLR,sd_DSPLR)
ES_normal <- mean(- X[- X > VaR_normal])

c(ES_emp, ES_normal)



# Cauchy simulation
N<-100000
U <- runif(N,0,1)
X <- tan((U-0.5)*pi)

q <- seq(0.01,0.99,by=0.001)
q_Cauchy <- qcauchy(q)
q_emp <- quantile(X,q)

par(mfrow=c(1,1))
plot(q_Cauchy, q_emp,xlab="theoretical quantiles", ylab="Empirical Quantiles", 
 main=paste("Empirical Q-Q Plot of ", bquote(.(N)), " Simulated Cauchy(0,1) vs Cauchy(0,1)"))
abline(0,1,col="red")


# LLN
n <- 1000
Z <- rnorm(n)
sample_mean <- cumsum(Z)/seq(1:n)
head(Z)
head(sample_mean)

par(mfrow=c(2,1))
plot(sample_mean,type="l",xlab="n",ylab="sample mean", main="Sample Mean of N(0,1) -> 0 as n -> infinity")
abline(h=0,lty=2,col="red")

cond_mean <- cumsum(Z*(Z>0))/cumsum(Z>0)
plot(cond_mean,type="l",xlab="n",ylab="sample mean", main=expression(paste("Sample Mean of N(0,1)'s > 0 -> ", 2/sqrt(2*pi), " as n -> infinity")))
abline(h=2/sqrt(2*pi),lty=2,col="red")


# Cauchy(0,1) vs N(0,1)
N<- length(DSPLR)# = length(DSPLR)        
N
GWN <- rnorm(N)
CWN <- rcauchy(N)

par(mfrow=c(2,1))
ts.plot(GWN,main=paste("Plot of ", bquote(.(N)), " N(0,1)"),xlab="index",ylab="")
ts.plot(CWN,main=paste("Plot of ", bquote(.(N)), " C(0,1)"),xlab="index",ylab="")

# extreme values: DSPLR
par(mfrow=c(1,1))
DSP_time<- seq(from=1960,to=2017,length.out=length(DSP)) 
plot(DSP_time[2:length(DSP)], DSPLR,type="l",xlab="Date",main="Daily log returns of S&P500 from Jan 1960 to  July 2017")

mean(DSPLR)
sd(DSPLR)
min(DSPLR)
(min(DSPLR)-mean(DSPLR))/sd(DSPLR)


#### below about GPD, optional
# GPD as a location-scale family

library(Rsafd)   # load the package
SHAPE.XI <- T
n<-10000
X1 <- rgpd(n, m=0, lambda = 1, xi = 1)
X2 <- rgpd(n, m=2, lambda = 3, xi = 1)
par(mfrow=c(1,1))
qqplot(X1,X2,xlim=c(0,60),ylim=c(0,180),xlab="GPD(0,1,1)",ylab="GPD(2,3,1)",main="Empirical Q-Q Plot of GPD(2,3,1) vs GPD(0,1,1)")
abline(2,3,col="red")
legend("bottomright","y=3x+2", text.col="red")

# Tails of GPD 

xi <- c(2,1,0.5)

x <- seq(6,12,by=0.01)
N<-length(x)

Y <- matrix(0,4,N)

for (i in 1:3)
{
  Y[i,] <- (1+xi[i]*x)^(-1-1/xi[i])
}

Y[4,] <- dnorm(x)

par(mfrow=c(1,1))
plot(x,Y[1,],type="l",lty=1,lwd=2,ylim=c(0,0.023),ylab="",main = expression(paste("GPD(0,1,",xi,") with Different ", xi, "'s vs N(0,1)")))
lines(x,Y[2,],lty=2,col="red",lwd=2)
lines(x,Y[3,],lty=3,col="blue",lwd=2)
lines(x,Y[4,],lty=4,col="purple",lwd=2)

legend("topright", c(expression(paste(xi,"=2")), expression(paste(xi,"=1")),expression(paste(xi,"=0.5")),"N(0,1)"), text.col=c("black","red","blue","purple"), lty = c(1,2,3,4),lwd=c(2,2,2,2), col=c("black","red","blue","purple"))

# Use QQ to compare tails
n<-10000
GPD_2 <- rgpd(n, m=0, lambda = 1, xi = 2)
GPD_1 <- rgpd(n, m=0, lambda = 1, xi = 1)
GPD_05 <- rgpd(n, m=0, lambda = 1, xi = 0.5)
Cauchy <- abs(rcauchy(n))
Z <- abs(rnorm(n))

par(mfrow=c(2,2))
qqplot(GPD_1,GPD_2,xlab="GPD(0,1,1)",ylab="GPD(0,1,2)",xlim=c(0,400),ylim=c(0,8*10^4),main="Q-Q Plot of GPD(0,1,2) vs GPD(0,1,1)")
qqplot(GPD_1,GPD_05,xlab="GPD(0,1,1)",ylab="GPD(0,1,0.5)",xlim=c(0,400),ylim=c(0,40),main="Q-Q Plot of GPD(0,1,0.5) vs GPD(0,1,1)")
qqplot(Cauchy,GPD_1,xlab="Cauchy(0,1)",ylab="GPD(0,1,1)",,xlim=c(0,250),ylim=c(0,400),main="Q-Q Plot of GPD(0,1,1) vs Cauchy(0,1)")
abline(0,pi/2,col="red")
#legend("bottomright","y=pi/2*x", text.col="red")
qqplot(Z,GPD_1,xlab="N(0,1)",ylab="GPD(0,1,1)",,xlim=c(0,3),ylim=c(0,400),main="Q-Q Plot of GPD(0,1,1) vs N(0,1)")


# Compare right tail of DSPLR with GPD
par(mfrow=c(1,1))
KD9 <- density(DSPLR, bw = .001) 
hist(DSPLR,breaks=50,  freq = F,main="Histogram & KDE of DSPLR, #bin = 50, bw=0.001",ylim=c(0,65))   
points(KD9,type="l",col="red")

DSPLR_p <- DSPLR[DSPLR > 0.013]
N <- length(DSPLR_p)

par(mfrow=c(1,2))
GPD_1 <- rgpd(N, m=0, lambda = 1, xi = 0.5)
qqplot(GPD_1,DSPLR_p,xlab="GPD(0,1,0.5)",ylab="DSPLR_p",main="Q-Q Plot of Positive DSPLR vs GPD(0,1,0.5)")
GPD_2 <- rgpd(N, m=0, lambda = 1, xi = 0.005)
qqplot(GPD_2,DSPLR_p,xlab="GPD(0,1,0.005)",ylab="DSPLR_p",main="Q-Q Plot of Positive DSPLR vs GPD(0,1,0.005)")


###### Fit GPD to DSP
library(Rsafd) 
SHAPE.XI <- TRUE


DSP<-rev(SP500$Close)
n<-length(DSP)

DSPRET <- diff(DSP)/DSP[1:(n-1)] #compute raw returns

par(mfrow=c(1,1))
DSP_time<- seq(from=1960,to=2017,length.out=length(DSP)) 
plot(DSP_time[2:length(DSP)], DSPRET,type="l",xlab="Date",main="Daily raw returns of S&P500 from Jan 1960 to July 2017")

eda.shape(DSPRET)  # Empirical Data Analysis

# choose threshold
par(mfrow=c(1,2))
shape.plot(DSPRET,tail="upper")
shape.plot(DSPRET,tail="lower")


# fit 
DSPRET.est <- gpd.tail(DSPRET, upper=0.01,lower=-0.011)      
 # check its help file by typing in help(gpd.tail)

# xi
DSPRET.est$upper.par.ests[2]       
DSPRET.est$lower.par.ests[2]       


# goodness of fit

par(mfrow=c(2,1))

tailplot(DSPRET.est,tail="upper")
tailplot(DSPRET.est,tail="lower")

tailplot(DSPRET.est,tail="upper",optlog="")  # natural scale, difficult to read
tailplot(DSPRET.est,tail="lower",optlog="")  


DSPRET.est <- gpd.tail(DSPRET, upper=0.01,lower=-0.011)      
 # check its help file by typing in help(gpd.tail)

#### log log plot illustration
n<-seq(1,5)
x <- 10^(-n)
y<-10^(-2*n)
par(mfrow=c(2,1))
plot(x,y,main="Natural Scale Plot")
plot(x,y,log="xy",main="Log-log Scale Plot")

n<-seq(1,5)
x <- 10^(-n)
y<- 2*n
par(mfrow=c(2,1))
plot(x,y,main="Natural Scale Plot")
plot(x,y,log="x",main="Log-Natural Scale Plot")

n<-seq(1,5)
x <- exp(-n) # = 10^(-n/log(10))
y<- 2*n
par(mfrow=c(2,1))
plot(x,y,main="Natural Scale Plot")
plot(x,y,log="x",main="Log-Natural Scale Plot")
abline(0,-2*log(10)) # Logarithm to base 10 


# Another check
par(mfrow=c(1,1))
SDSPRET<-gpd.2q(runif(10000),DSPRET.est)    
 # check its help file

qqplot(DSPRET,SDSPRET,main = "Q-Q Plot of DSPRET vs Simulated")
abline(0,1)

qqnorm(DSPRET)
abline(0,1)

# compute VaR
VaR_emp <- - quantile(DSPRET,0.005)
VaR_N <- - qnorm(0.005,mean(DSPRET),sd(DSPRET))
VaR_GPD <- - gpd.2q(0.005,DSPRET.est)       
round(c(VaR_emp,VaR_N,VaR_GPD),3)


