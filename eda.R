data(EuStockMarkets)
#mode(EuStockMarkets)
#class(EuStockMarkets)

#pdf("EuStocks.pdf",width=6,height=5)
plot(EuStockMarkets)
#graphics.off()

logR = diff(log(EuStockMarkets))
plot(logR)
plot(as.data.frame(logR))

par(mfrow=c(2,2))
for(i in colnames(logR))
{
  qqnorm(logR[,i],datax=T,main=i)
  qqline(logR[,i],datax=T)
  print(shapiro.test(logR[,i]))
}

n=dim(logR)[1]
q_grid = (1:n)/(n+1)
df_grid=c(1,4,6,10,20,30)
index.names = dimnames(logR)[[2]]     #column names of the data set logR
for(i in 1:4)
{
 
  dev.new()
  par(mfrow=c(3,2))
  for(df in df_grid)
  {
    qqplot(logR[,i], qt(q_grid,df),
           main=paste(index.names[i], ", df=", df) )
    abline(lm(qt(c(.25,.75),df=df)~quantile(logR[,i],c(.25,.75))))   #fit a line to the quantiles of t-distribution
  }
}



###  Exercise 1

ford = read.csv("ford.csv")
FORD = ford[,3]
options(digits=3)
mean(FORD)
median(FORD)
sd(FORD)

#pdf("fordNormal.pdf",width=6,height=5)
qqnorm(FORD,datax=T)
#graphics.off()

shapiro.test(FORD)
n=length(FORD)
q.grid = (1:n)/(n+1)
df=c(1,2,3,4,5,6,7,8,10)


#pdf("fordNormalPlots.pdf",width=6,height=6)
par(mfrow=c(3,3))
for(j in 1:length(df))
{
  qqplot(FORD, qt(q.grid,df=df[j]),
         main=paste("df=", df[j]) )
  abline(lm(qt(c(.25,.75),df=df[j])~quantile(FORD,c(.25,.75))))
}
graphics.off()


options(digits=4)
den=density(FORD)
n =  length(FORD)
sqrt(.25/n)/spline(den$x,den$y,xout=median(FORD))$y
sd(FORD)/sqrt(n)


