install.packages("car")
install.packages("lmtest")
install.packages("matrixStats")

library(MASS)
library(car)
library(lmtest)
library(ggplot2)
library(boot)
library(fitdistrplus)
library(matrixStats)


options(digits=4)
setwd("###")


##hw 1

dat.4stocks<-read.csv("FourStocks_Daily2013.csv")
attach(dat.4stocks)

#plot each stock over time

AAPL<-diff(log(AAPL_Price))
XOM<-diff(log(XOM_Price))
TGT<-diff(log(TGT_Price))
MCD<-diff(log(MCD_Price))


par(mfrow=c(4,1))
Date.new<-as.Date(Date[-1],"%m/%d/%Y")
plot(Date.new,AAPL, type="l",col="red", main="AAPL", xlab="Date", ylab="LogReturns")  
plot(Date.new,XOM, type="l",col="blue", main="XOM", xlab="Date", ylab="LogReturns")  
plot(Date.new,TGT, type="l",col="green", main="TGT", xlab="Date", ylab="LogReturns")  
plot(Date.new,MCD, type="l",col="black", main="MCD", xlab="Date", ylab="LogReturns")  

dat.ret<-cbind(AAPL, XOM, TGT, MCD)
colnames(dat.ret)<-c("AAPL", "XOM", "TGT", "MCD")
detach(dat.4stocks)
attach(as.data.frame(dat.ret))

#histograms of three stocks
plot.new()
par(mfrow=c(4,1))
hist(AAPL,breaks=50,prob=T,xlab="Return",main="Citi") 
lines(density(AAPL), col="red", lwd=2)
hist(XOM,breaks=50,prob=T,xlab="Return",main="GM") 
lines(density(XOM), col="blue", lwd=2)
hist(TGT,breaks=50,prob=T,xlab="Return",main="PFE")
lines(density(TGT), col="green", lwd=2)
hist(MCD,breaks=50,prob=T,xlab="Return",main="PFE")
lines(density(MCD), col="black", lwd=2)

#Q-Q plots of three stocks

par(mfcol=c(4,1))
qqnorm(AAPL,main="AAPL Q-Q Plot", col="red", pch=18, cex=1); qqline(AAPL, col="red", lwd=2)
qqnorm(XOM,main="XOM Q-Q Plot", col="blue", pch=18, cex=1); qqline(XOM,col="blue", lwd=2)  
qqnorm(TGT,main="TGT Q-Q Plot", col="green", pch=18, cex=1); qqline(TGT,col="green", lwd=2)
qqnorm(MCD,main="MCD Q-Q Plot", col="green", pch=18, cex=1); qqline(MCD,col="black", lwd=2)


#MLE estimates

mle.out<-matrix(rep(0,8),2,4)
mle.out.t<-matrix(rep(0,8),2,4)
for (i in 1:4)
{
mle.est<-fitdistr(dat.ret[,i],"normal")
mle.out[1,i]<-mle.est$estimate[1]
mle.out[2,i]<-mle.est$estimate[2]
mle.est.t<-fitdistr(dat.ret[,i],"t")
mle.out.t[1,i]<-mle.est.t$estimate[1]
mle.out.t[2,i]<-mle.est.t$estimate[2]
}
mle.out
mle.out.t

#bootstrap estimates of mean returns and std
stats<-matrix(rep(0,8),2,4)
boot.f <- function(data,indices) 
{
	dat<-data[indices,]
	stats[1,]<-colMeans(dat)
	stats[2,]<-colSds(dat)
	return(stats)
} 
boot.f(dat.ret)

boot.out<-boot(data=dat.ret,statistic=boot.f,R=1000) # bootstrapping with 1000 replications
boot.out
summary(boot.out)


# Bootstrap estimates of correlation coefficients 

# function to obtain stardard errors of regression coefficients 

cor.f <- function(data, indices) {
	dat <- data[indices,] # allows boot to select sample 
	cor.mat<-cor(dat)
	return(c(cor.mat[2,1],cor.mat[3,1],cor.mat[3,2],cor.mat[4,1],cor.mat[4,2],cor.mat[4,3])) 
} 
cor.f(dat.ret)

boot.cor<-boot(data=dat.ret,statistic=cor.f,R=1000) # bootstrapping with 1000 replications
boot.cor
summary(boot.cor)

#standard error of sample mean of AAPL

den=density(AAPL)
n =  length(AAPL)
sqrt(.25/n)/spline(den$x,den$y,xout=median(AAPL))$y
sd(AAPL)/sqrt(n)




