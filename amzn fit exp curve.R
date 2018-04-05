library(quantmod)
a<-new.env()
getSymbols("NVDA", env=a,src="google", from="2015-01-01")
Cl(a$NVDA)

x<-as.data.frame(a$NVDA)
x$id<- seq.int(nrow(x))
x<-x[-c(1:3,5)]

Cl<-x$NVDA.Close
id<-x$id

c.0<-min(Cl)*.5
model.0<-lm(log(Cl-c.0)~id,data=x)
start<-list(a=exp(coef(model.0)[1]),b=coef(model.0)[2],c=c.0)
model<-nls(Cl~a*exp(b*id)+c, data=x,start=start, trace=TRUE)

par(mfrow=c(1,1))

chartSeries(a$NVDA, layout=NULL, TA=NULL, theme="white")
addSMA(n=9, col="blue")
addSMA(n=20, col="blue")
addSMA(n=50, col="blue")
addSMA(n=60, col="blue")
addSMA(n=120, col="blue")
addSMA(n=200, col="blue")
addSMA(n=300, col="blue")

p <- coef(model)
plot(id, cl,type="l")
curve(p["a"] * exp(p["b"] * x) + p["c"], lwd=2, col="Green", add=TRUE)
co<-as.list(coef(model))
mtext("y = "+co$a )+ "e ^(" + co$b +"* t)" + "+ " + co$c)


