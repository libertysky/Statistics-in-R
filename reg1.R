install.packages("Rcmdr")
library(MASS)
library(Rcmdr)

setwd("C:/Users/vli/Desktop/Spring 2017/FIN 6832 Num Stats Fri/datasets")
ret<-read.table("d_logret_12stocks.txt", header=T)
ret[,-1]<-ret[,-1]*100
attach(ret)
day<-ret[,1]

### 1. scattor plot
plot(ret[,-1])
format(cor(ret[,-1]), digits=4)
### 2. fit the full model
fit.full<-lm(msft~aapl+adbe+adp+amd+dell+gtw+hp+ibm+orcl+sunw+yhoo)
summary(fit.full)
### 3. variable selection
#backward selection
dropterm(fit.full, test="F")
fit1<-update(fit.full, .~.-sunw-hp)
dropterm(fit1, test="F")
fit2<-update(fit1, .~.-amd)
dropterm(fit2, test="F")
fit3<-update(fit2, .~.-adp-yhoo)
dropterm(fit3, test="F")
fit.reduce<-lm(msft ~ aapl + adbe + dell + gtw + ibm + orcl)
summary(fit.reduce)

anova(fit.full,fit1,fit2,fit3,fit.reduce, test="F")

#forward selection
fit.null<-lm(msft~1)

addterm(fit.null, scope = fit.full,test="F")
fit.add1<- update( fit.null, .~.+dell)
summary(fit.add1)

addterm(fit.add1, scope = fit.full,test="F")
fit.add2<- update( fit.add1, .~.+ibm)
summary(fit.add2)

addterm(fit.add2, scope = fit.full,test="F")
fit.add3<- update( fit.add2, .~.+orcl)
summary(fit.add3)

addterm(fit.add3, scope = fit.full,test="F")
fit.add4<- update( fit.add3, .~.+adbe)
summary(fit.add4)

addterm(fit.add4, scope = fit.full,test="F")
fit.add5<- update( fit.add4, .~.+gtw)
summary(fit.add5)

addterm(fit.add5, scope = fit.full,test="F")
fit.add6<- update( fit.add5, .~.+aapl)
summary(fit.add6)

anova(fit.null, fit.add1, fit.add2, fit.add3, fit.add4, fit.add5,fit.add6)

resstep<-stepwise(fit.full)


#Model Diagnostics 

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit.reduce,which=1:4)

