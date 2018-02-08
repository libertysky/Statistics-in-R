
################################################################
########## Code for the R lab   ################################
################################################################
setwd("C:/Users/vli/Desktop/Spring 2017/FIN 6832 Num Stats Fri/datasets")
dat = read.csv("Stock_bond.csv",header=TRUE)
names(dat)
attach(dat)
par(mfrow=c(1,2))
plot(GM_AC,type="l")
plot(F_AC,type="b")

n = dim(dat)[1]
GMReturn = GM_AC[2:n]/GM_AC[1:(n-1)] - 1  #GMReturn = GM_AC[-1]/GM_AC[-n] - 1
FReturn = F_AC[2:n]/F_AC[1:(n-1)] - 1

#pdf("ReturnsLab01.pdf",width=6,height=5)
par(mfrow=c(1,1))
plot(GMReturn,FReturn)
cor(GMReturn,FReturn)
#graphics.off()

#########  problem 3  ##########
MSFTReturn = MSFT_AC[-1]/MSFT_AC[-n] - 1
MRKReturn = MRK_AC[-1]/MRK_AC[-n] - 1
plot(MSFTReturn,MRKReturn)
round(cor(MSFTReturn,MRKReturn),3)

################################################################
########## Code for simulations   ##############################
################################################################

niter = 1e5  # number of iterations (100k)
below = rep(0,niter)  #create a vector
set.seed(2009)   #reset the random number generator
for (i in 1:niter)
{
  r = rnorm(45,mean=.05/253,sd=.23/sqrt(253))   #generate a random value with normal dist
  logPrice = log(1e6) + cumsum(r)
  minlogP = min(logPrice)
  below[i] = as.numeric(minlogP < log(950000))
}
mean(below)         #prob of min price is less than $950,000


################################################################
########## Code for Simulating a geometric random walk   #######
################################################################

set.seed(2012)
n = 253
par(mfrow=c(3,3))
for (i in (1:9))
{
  logr = rnorm(n,0.05/253,0.2/sqrt(253))
  price = c(120,120*exp(cumsum(logr)))
  plot(price,type="l")
}


install.packages("Ecdat")
library(Ecdat)
summary(SP500)

#histogram of daily log returns on S&P500
par(mfrow=c(2,2))
hist(log(1+SP500$r500),breaks=30,xlab="return",main="(a) 30 cells, full range")
hist(log(1+SP500$r500),xlim=c(-0.04,0.04),breaks=30,xlab="return",main="(b) 30 cells, cental range")
hist(log(1+SP500$r500),xlim=c(-0.04,0.04),breaks=20,xlab="return",main="(c) 20 cells, cental range")
hist(log(1+SP500$r500),xlim=c(-0.04,0.04),breaks=50,xlab="return",main="(d) 50 cells, cental range")

#Kernel Density Estimator
hist(log(1+SP500$r500),prob=T,breaks=30, ylim=c(0,50),xlim=c(-0.04,0.04), xlab="return",main="S&P 500 daily returns")
lines(density(log(1+SP500$r500)), col="blue")
lines(density(log(1+SP500$r500),adjust=3),type="s",col="red")

##use GE daily returns in the data set CRSPday in R's Rcdat package
library(Ecdat)
View(CRSPday)
is.matrix(CRSPday)
ge<-CRSPday[,4]

##MLE estimate of GE daily returns
library(fitdistrplus)
library(MASS)
mle.est<-fitdistr(ge,"t")
mle.est$estimate
mle.est$sd

##Bootstrap estimates
library("boot")
mean.fun <- function(dat,idx) mean(dat[idx])
boot.mean <- boot(ge, mean.fun, R=1000)
boot.mean

sd.fun <- function(dat,idx) sd(dat[idx])
boot.sd <- boot(ge, sd.fun, R=1000)
boot.sd


  
  
