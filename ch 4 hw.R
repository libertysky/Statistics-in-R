data("EuStockMarkets")
mode(EuStockMarkets)
class(EuStockMarkets)
#plot(EuStockMarkets)

logR = diff(log(EuStockMarkets))
#plot(logR)
#plot(as.data.frame(logR))

par(mfrow=c(2,2))
for(i in colnames(logR))
{
  qqnorm(logR[,i], datax = T, main=i)
  qqline(logR[,i],datax=T)
  print(shapiro.test(logR[,i]))
}

  n=dim(logR)[1]
  q_grid = (1:n)/(n+1)
  df_grid = c(1,4,6,10,20,30)
  index.names = dimnames(logR)[[2]]
for(i in 1:4)
{
  #dev.new()
  par(mfrow=c(3,2))
  for(df in df_grid)
  {
    qqplot(logR[,i],qt(q_grid,df),
          main = paste(index.names[i],",df = ",df))
        abline(lm(qt(c(0.25,0.75),df=df)~
          quantile(logR[,i],c(0.25,0.75))))
  }
}
