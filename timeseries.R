install.packages("FitAR")   #Box-Cox and fitting for time series
install.packages("Ecdat")
install.packages("moments")
install.packages("car")
install.packages("lmtest")
install.packages("forecast")
install.packages("MTS")    #multivariate time series
install.packages("tseries")  #choose mirror of USA(MO)
install.packages("xts")
install.packages("urca")
 
library(FitAR)
library(Ecdat)
library(moments)
library(MASS)
library(tseries)
library(MTS)
library(xts)
library(urca)
library(forecast)

setwd("C:\\Users\\vli\\Desktop\\Spring 2017\\FIN 6832 Num Stats Fri\\final")

data(Mishkin,package="Ecdat")
y = as.ts(Mishkin[,1], start=1990, frequency=12) 
y = ts(as.vector(Mishkin[,1]), start=1990, frequency=12)  
plot(Mishkin[,1])
#Time series plot of inflation rate and change in rate

#pdf("inflation.pdf", width=7, height=6)
par(mfrow=c(2,1))
plot(y,ylab="Inflation Rate",type="l",xlab="Year",cex.lab=1.5,
     cex.axis=1.5,cex.main=1.3,main="(a)")
plot(diff(y),ylab="Change in Rate",type="l",xlab="Year",cex.lab=1.5,
     cex.axis=1.5,cex.main=1.2,main="(b)")

#ACF and PACF plots of inflation

y = as.vector(Mishkin[,1]) 

#pdf("inflation_acf.pdf", width=8, height=4)
par(mfrow=c(2,2))
acf(ts(y),xlim=c(1,24),main="Inflation Rate")
pacf(ts(y),xlim=c(1,24),main="Inflation Rate")
acf(ts(diff(y)),xlim=c(1,24),main="Change of Inflation Rate")
pacf(ts(diff(y)),xlim=c(1,24),main="Change of Inflation Rate")

#Fit a AR(1)
fit.ar1 = arima(y, order = c(1,0,0))
fit.ar1
Box.test(fit.ar1$resid,lag=12, type = "Ljung-Box") #null hypothesis of independence
acf(fit.ar1$resid)

#ACF of residuals in AR(1)
par(mfrow=c(1,2))
acf(y,main="Inflation rate")
acf(fit$resid,main="Residuals from AR(1)")


#Stationary test
adf.test(y)

#first difference
y = as.vector(Mishkin[,1]) 
x = diff(y)
logn = log(length(x))
adf.test(x)


#Fitting AR(p) models  
resultsdiff = matrix(0,nrow=20,ncol=3)
for (i in 1:20){
  fit = arima(x,order=c(i,0,0))
  resultsdiff[i,1] = i  
  resultsdiff[i,2] = fit$aic
  resultsdiff[i,3] = resultsdiff[i,2] + (logn-2)*i
}
colnames(resultsdiff)<-c("p","AIC","BIC")
resultsdiff

#plot AIC and BIC of models AR(1)-AR(20)
plot(resultsdiff[,1],resultsdiff[,2],xlab="p",ylab="criterion",cex.lab=1.35,cex.axis=1.35,
     main="AIC and BIC for AR fits to changes in inflation rate",
     cex.main=1.35,cex=2,pch="*",ylim=c(2440,2560),type='b')
points(resultsdiff[,1],resultsdiff[,3],pch="o",cex=2,type='b')
legend(12,2565,c("AIC","BIC"),pch=c("*","o"),cex=2,box.lty=0)

#select a best model using AIC or BIC
auto.arima(y, max.p=12, max.q=0, d=1, ic="aic",seasonal=FALSE)
auto.arima(y, max.p=12, max.q=0, d=1, ic="bic",seasonal=FALSE)
auto.arima(y, max.p=0, max.q=12, d=1, ic="aic",seasonal=FALSE)
auto.arima(y, max.p=0, max.q=12, d=1, ic="bic",seasonal=FALSE)

auto.arima(y, max.p=12, max.q=12, max.d=1, ic="aic",seasonal=FALSE)
auto.arima(y, max.p=12, max.q=12, max.d=1, ic="bic",seasonal=FALSE)

fit<-auto.arima(y, max.p=0, max.q=12, d=1, ic="bic",seasonal=FALSE)
fit
Box.test(fit$resid, type = "Ljung-Box")


#Box-Cox transformation of the time series 

summary(y)
min.y<-min(y)
eps<-1e-5
y.new<-y-min(y)+eps    #transform y into positive values

par(mfrow=c(2,1))
boxcox(y.new~1,main="Box-Cox 1")
boxcox(y.new~1,lambda=seq(-0.1,1,0.1),main="Box-Cox 2")
bc<-boxcox(y.new~1,main="Box-Cox 1")
lambda.y<-bc$x[which.max(bc$y)]
lambda.y
y.bc<-(y.new^lambda.y-1)/lambda.y

ts.plot(y.bc)

fit1 = arima(y, order = c(1,0,0))
BoxCox.Arima(fit1)
fitnew = arima(y.bc, order = c(1,0,0))
fitnew
Box.test(fitnew$resid, type = "Ljung-Box") #null hypothesis of independence

auto.arima(y.bc, max.p=0, max.q=12, d=1, ic="bic",seasonal=FALSE)

#predict the monthly inflation in the next 4 years (48 months)

pred.infl = predict(fit, n.ahead = 49, se.fit = TRUE)
t1 = 300:491
t2 = 492:(492+48)
year = seq(1950 + 1/12,1991+48/12,1/12)

plot(year[t1],y[t1],ylim=c(-10,18),type="b",xlim=c(1975,1996),
     xlab="year",ylab="Inflation rate",cex.axis=1.15,cex.lab=1.15)
points(year[t2], pred.infl$pred,type="p",pch="*",col="red")
lines(year[t2], pred.infl$pred - 2*pred.infl$se,col="blue")
lines(year[t2], pred.infl$pred + 2*pred.infl$se,col="blue")
legend(1975,-3,c("data","predictions","lower CL","upper CL"),cex=1.2,
       box.lty=0,pch=c("o","*",NA,NA),lty=c(NA,NA,1,1))


#seasonal pattern of housing starts from Q1 of 1960 to Q4 of 2001


data(Hstarts, package="Ecdat")
x = ts(Hstarts[,1], start=1960, frequency=4) 

par(mfrow=c(1,3))
plot(x,ylab="log(starts)",type="l",xlab="year",main="Time Series Plot")
acf(x,main="ADF",xlab="lag")
quart = rep(1,42) %x% (1:4)   #quart=rep(1:4,42)
boxplot(x~quart,xlab="quarter",ylab="log(starts)",main="Box Plot")

#adf plots 
par(mfrow=c(3,3))
plot(diff(x),xlab="year",type="l",main="(a) nonseasonal differencing")
acf(ts(diff(x)),xlim=c(1,20),main="(b) nonseasonal differencing",xlab="lag")
pacf(ts(diff(x)),xlim=c(1,20),main="(b) nonseasonal differencing",xlab="lag")

plot(diff(x,4),type="l",xlab="year",main="(c) seasonal differencing")
acf(ts(diff(x,4)),xlim=c(1,20),main="(d) seasonal differencing",xlab="lag")
pacf(ts(diff(x,4)),xlim=c(1,20),main="(d) seasonal differencing",xlab="lag")

plot(diff(diff(x,1),4),type="l",xlab="year",
     main="(e) seasonal & nonseasonal differencing")
acf(ts(diff(diff(x,1),4)),xlim=c(1,20),main="(f) seasonal & nonseasonal differencing",xlab="lag")
pacf(ts(diff(diff(x,1),4)),xlim=c(1,20),main="(f) seasonal & nonseasonal differencing",xlab="lag")


#fit arima model
fit1 = arima(x, c(1,1,1), seasonal = list(order = c(1,1,1), period = 4))
fit1

Box.test(fit1$resid, type = "Ljung-Box",lag=12,fitdf=4) #null hypothesis of independence

fit2 = arima(x, c(1,1,1), seasonal = list(order = c(0,1,1), period = 4))
fit2

Box.test(fit2$resid, type = "Ljung-Box",lag=12,fitdf=4) #null hypothesis of independence


#predict housing starts for the next 16 months

pred = predict(fit2, n.ahead = 16, newxreg = NULL, se.fit = TRUE)
year = seq(1960.25,2006,0.25)
t1 = 121:168
t2 = 169:(169+15)

par(mfrow=c(1,1))
plot(year[t1],x[t1],ylim=c(8.25,10.3),xlim=c(1990.00,2006.00),type="b",lty=2,
     xlab="year",ylab="log(starts)",cex.axis=1.5,cex.lab=1.5)
points(year[t2], as.matrix(pred$pred),type="b",pch="*",lty=3,col="red")
lines(year[t2], pred$pred - 2*pred$se,col="blue")
lines(year[t2], pred$pred + 2*pred$se,col="blue")
legend("topleft",c("data","predictions","lower CL","upper CL"),cex=1.2,
       box.lty=1,pch=c("o","*",NA,NA),lty=c(2,3,1,1))

#marima
library("car")
library("lmtest")
library("forecast")

#download the data "CPI.dat.csv", "IP.dat.csv", "CokePepsi.csv", and "treasury_yields.txt" and save 
#in a folder. 
CPI.dat = read.csv("CPI.dat.csv")
IP.dat = read.csv("IP.dat.csv")
CPI = as.matrix(CPI.dat$CPI)[769:900,] # 1977-01-31 to 1987-12-31
IP = as.matrix(IP.dat$IP)[697:828,] # 1977-01-31 to 1987-12-31
cpi = log(CPI)
ip = log(IP)
cpi_diff1 = diff(cpi)
ip_diff1 = diff(ip)

par(mfrow=c(2,2),cex.axis=1.3,cex.lab=1.1,cex.main=1.35, mgp=c(3,1,0))
plot(ts(cpi,start = c(1977,1), frequency = 12),xlab="year",ylab="log(CPI)",main="(a)")
plot(ts(ip,start = c(1977,1), frequency = 12),xlab="year",ylab="log(IP)",main="(b)")
plot(ts(cpi_diff1,start = c(1977,2), frequency = 12),xlab="year",ylab=expression(paste(Delta," log(CPI)")),main="(c)")
plot(ts(ip_diff1,start = c(1977,2), frequency = 12),xlab="year",ylab=expression(paste(Delta," log(IP)")),main="(d)")

#cross correlation
par(cex.axis=1.35,cex.lab=1.35,cex.main=1.35)
ccf(cpi_diff1,ip_diff1,lwd=3,  ylab="CCF",
    main=expression(paste("corr{",Delta,"cpi(t),",Delta,"ip(t-lag)}" )))
abline(v=0,lty=5)

#acf and ccf
CPI_IP = cbind(cpi_diff1, ip_diff1)
acf(CPI_IP)

#multivariate Ljung-Box test
source("SDAFE2.R")
mLjungBox(CPI_IP,lag=10)

#fit VAR model
arFit = ar(CPI_IP,order.max=10)
options(digits=2)
arFit$aic
arFit1=ar(CPI_IP,order.max=1)
arFit1

arFit2 = VAR(CPI_IP,p=1)
arFit2$coef
arFit2$secoef
arFit2$aic
mLjungBox(arFit2$resid,lag=12)
acf(arFit2$resid)

CPI_IP2 = cbind(diff(cpi_diff1), ip_diff1[-1])
acf(CPI_IP2)
arFit1.d=ar(CPI_IP2)
arFit1.d

#cointegration

CokePepsi = read.table("CokePepsi.csv", header=T)
ts.plot(CokePepsi)

ts.plot(CokePepsi[,2] - CokePepsi[,1])

#Johansen-Procedure: test # of cointegration
library(urca)
library(vars)
summary(ca.jo(CokePepsi))

#example of five yields

yieldDat = read.table("treasury_yields.txt",header=T)
date = as.Date(yieldDat[,1], format = "%m/%d/%y")
dat = as.xts(yieldDat[,3:7], date)

res = residuals(lm(dat[,3]~dat[,1]+dat[,2]+dat[,4]+dat[,5]))

#
par(mfrow=c(2,4))
plot(dat[,1],type="l",ylab="",main="3-month", minor.tick=FALSE)
plot(dat[,2],type="l",ylab="",main="6-month", minor.tick=FALSE)
plot(dat[,3],type="l",ylab="",main="1-year", minor.tick=FALSE)
plot(dat[,4],type="l",ylab="",main="2-year", minor.tick=FALSE)
plot(dat[,5],type="l",ylab="",main="3-year", minor.tick=FALSE)
plot(res,type="l",ylab="",main="residuals")
acf(res,main="ACF of residuals",xlab="lag")

#Augmented Dicky Fuller test
options(digits=4)
results.adf = matrix(0,nrow=5,ncol=2)
dat.names<-colnames(dat)
for (i in 1:5){
adf<-adf.test(dat[,i])
results.adf[i,1]<-adf$statistic
results.adf[i,2]<-adf$p.value
}
rownames(results.adf)<-dat.names
colnames(results.adf)<-c("ADF.statistic","p.value")
results.adf


# Phillips-Ouliaris Cointegration Test
po.test(dat[,c(3,1,2,4,5)])  #smaller p-value indicates that the residuals are stationary

#Johansen-Procedure: test # of cointegration
summary(ca.jo(dat))
jo.out<-ca.jo(dat)
ecm.out<-cajorls(jo.out,r=3)
summary(ecm.out$rlm)

#Johansen-Procedure: test # of cointegration for daily yields of 3-mon & 6-mon 
dat.m<-dat[,1:2]
summary(ca.jo(dat.m))
jo.out.m<-ca.jo(dat.m)
ecm.out.m<-cajorls(jo.out.m,r=1)
summary(ecm.out.m$rlm)
res<-dat.m%*%ecm.out.m$beta
adf.test(res)






