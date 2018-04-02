# Vincent Li
# VXL151030
# final 6382

install.packages("tseries")
install.packages("forecast")
install.packages("urca")
install.packages("aod")
install.packages("MASS")

library(tseries)
library(forecast)
library(urca)
library(MASS)
library(aod)

setwd("C:\\Users\\vli\\Desktop\\Spring 2017\\FIN 6832 Num Stats Fri\\datasets")

#1
ip<-read.table("production.txt", sep="\t", header=TRUE)
ip.ts<-ts(ip,start=1990,freq=12)
y=as.vector(ip.ts[,2])

#Time series plot of IP
plot(y,ylab="IPGMFN")
plot(diff(log(lag(as.ts(y),1))),ylab="log 1st diff IP")
#plot(ip[,2]~ip[,1],xlab="Date",ylab="IP",main="IPGMFN", type="o")

#ACF plot of IP
acf(y,main="IPGMFN")
#acf(diff(y),12) #same results with diff scale
acf(diff(y))

#PACF plot of IP
pacf(y,main="IPGMFN")
pacf(diff(y))

#ADF test of stationarity on IP
adf.test(y)
adf.test(diff(y))

library(forecast)
auto.arima(y,max.P=0,max.Q=0,ic="aic")
auto.arima(y,max.P=0,max.Q=0,ic="bic")

#remove approx
auto.arima(y,max.P=0,max.Q=0,ic="aic", stepwise=FALSE,approximation=FALSE)
auto.arima(y,max.P=0,max.Q=0,ic="bic", stepwise=FALSE,approximation=FALSE)

fit<- arima(y,order=c(0,1,5))
acf(residuals(fit),main="Residuals")
Box.test(residuals(fit),lag=12,type="Ljung")

fit <- arima(y, order=c(0,1,1), seasonal=c(0,1,1))
plot(forecast(fit), ylab="IPGMFN", xlab="Time")

#2
rates<-read.table("rates.txt", sep="\t", header=TRUE)
a<-rates[,2:9]

#Phillips-Ouliaris Cointegration Test
summary(ca.po(a[,1:6]))

#Johansen-Procedure: test # of cointegration
summary(ca.jo(a))

#VECM components
cajorls(ca.jo(a))

#3
house<-read.csv("houseprices.csv",header=TRUE)
fit1 = glm(prefer ~ ., family = "binomial",data = house)
summary(fit1)
library(MASS)
fit2 = stepAIC(fit1)
summary(fit2)

#calculate odds pref/no pref for driveway=1
exp(cbind(OR=coef(fit2),confint(fit2)))

#mean
summary(house) 
plogis(c(1,68122,1.29,1,1,1) %*% coef(fit2))