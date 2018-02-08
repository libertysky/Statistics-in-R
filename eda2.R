#Log transformation is used to stabilize the conditional variance.
#risk-free return example
data(Capm,package="Ecdat")
diffrf=diff(Capm$rf)
n = length(diffrf)
year_rf = 1960 + (1:n) * (2003 - 1960)/n

par(mfrow=c(2,2))
plot(Capm$rf[-1],diff(Capm$rf),xlab="lagged rate",ylab="change in rate",
     main="(a)")
plot(year_rf,diff(Capm$rf),xlab="year",ylab="change in rate",
     main="(b)")
plot(Capm$rf[-1],diff(log(Capm$rf)),xlab="lagged rate",ylab="change in log rate",
     main="(c)")
plot(diff(log(Capm$rf)),xlab="year",ylab="change in log rate",
     main="(d)")


#Transformation of Left skewed data
#3 flows of gas pipeline

setwd("###")
dat=read.csv("FlowData.csv")
dat = dat/10000

#hist of 3 flows
par(mfrow=c(2,2))
hist(dat$Flow1)
hist(dat$Flow2)
hist(dat$Flow3)

x1=dat$Flow1
par(mfrow=c(2,3))
plot(density(x1),main=expression(alpha == 1) )
plot(density((x1^2-1)/2),main=expression(alpha == 2) )
plot(density((x1^3-1)/3),main=expression(alpha == 3) )
plot(density((x1^4-1)/4),main=expression(alpha == 4) )
plot(density((x1^5-1)/5),main=expression(alpha == 5) )
plot(density((x1^6-1)/6),main=expression(alpha == 6) )

#Automatic choose of an optimal alpha to transform the data
library("MASS")
adj = 1.5
par(mfrow=c(3,3))
x = dat$Flow1
x1 = sort(x)
bcfit1 = boxcox(x1~1,lambda=seq(2.6, 4.5, 1/100),
                xlab=expression(alpha))
text(3,-1898.75,"Flow 1")
plot(density((x1^3.5-1)/3.5,adjust=adj),main="Flow 1")
qqnorm((x1^3.5-1)/3.5,datax=T,main="Flow 1")
x=dat$Flow2
x2 = sort(x)
bcfit2 = boxcox(x2~1,lambda=seq(8, 13.5, 1/100),xlab=expression(alpha))
text(8.5,-1776.5,"Flow 2")
plot(density( (x2^10.5-1)/10.5,adjust=adj),main="Flow 2")
qqnorm((x2^10.5-1)/10.5,datax=T,main="Flow 2")
x=dat$Flow3
x3 = sort(x)
bcfit3 = boxcox(x3~1,lambda=seq(.6, 2, 1/100),xlab=expression(alpha))
text(1.6,-1793,"Flow 3")
plot(density( (x3^1.235-1)/1.235,adjust=adj),main="Flow 3")
qqnorm((x3^1.235-1)/1.235,datax=T,main="Flow 3")





