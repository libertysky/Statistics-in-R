install.packages("forecast")
install.packages("tseries")
library(forecast)
library(tseries)

getwd()
setwd("C:/Users/lxm133730/Documents/fin6306")

mydata<-read.table("6306data.txt", header=TRUE, sep="\t")
plot(mydata[,1],type="l")  #type="l' refers to a "line" plot
plot(mydata[,1],type="l", col="blue", xlab="time", ylab="closing values", main="Stock A")  
plot(diff(log(mydata[,1])),type="l")

#histogram of stock A
hist(diff(log(mydata[,1])), prob=T, col="red", ylim=c(0,40))
lines(density(diff(log(mydata[,1]))), col="blue")
#Hypothetical Density Distribution
mu<-mean(diff(log(mydata[,1])))
sigma<-sd(diff(log(mydata[,1])))
x<-seq(-0.2,0.2,length=100)
y<-dnorm(x,mu,sigma)
lines(x,y,col="black",lwd=2)

myts<-ts(mydata[,1],start=1980,freq=12)
plot(myts)

#Augmented Dickey Fuller test
adf.test(myts)
myts.diff<-diff(myts)
plot(myts.diff)
adf.test(myts.diff)

#ACF and PACF
par(mfrow=c(2,1))   #combine multiple plots into one graph
acf(myts.diff,main="ACF of Value Difference")
pacf(myts.diff,main="PACF of Value Difference")

#fit a MA(1) model
myts.fit<-arima(myts,order=c(0,1,1))


#residual diagnostics
tsdiag(myts.fit)

#Predication
myts.pred<-predict(myts.fit, n.ahead=12)
plot(myts)
lines(myts.pred$pred,col="red")
lines(myts.pred$pred+2*myts.pred$se,col="red",lty=3)
lines(myts.pred$pred-2*myts.pred$se,col="red",lty=3)






