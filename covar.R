install.packages("Ecdat")
install.packages("robust")
library("Ecdat")
library("robust")

setwd("###")

##  Uses monthly data from Jan-69 to Dec-98
## Use realistic factor model (time-series regression) to estimate the covariance matrix of three equities

#read the data of fama-french 3 factors 
FF_data = read.table("FamaFrench_mon_69_98.txt",header=T)
attach(FF_data)

#read the data of 3 equities and compute the excess returns
data(CRSPmon)
ge = 100*CRSPmon[,1] - RF
ibm = 100*CRSPmon[,2] - RF
mobil = 100*CRSPmon[,3] - RF
stocks=cbind(ge,ibm,mobil)

#fit a 3-factor model
fit = lm(cbind(ge,ibm,mobil)~Mkt.RF+SMB+HML)
options(digits=4)
fit

#scatter plot
pairs(cbind(ge,ibm,mobil,Mkt.RF,SMB,HML))

#correlation of residuals
cor(fit$residuals)
covRob(fit$residuals,cor=T)
cor.test(fit$residuals[,1], fit$residuals[,2])
cor.test(fit$residuals[,1], fit$residuals[,3])
cor.test(fit$residuals[,2], fit$residuals[,3])

pairs(fit$residuals)

#compute the covariance of 3 equities by factor model
sigF = as.matrix(var(cbind(Mkt.RF,SMB,HML)))
bbeta = as.matrix(fit$coef)
bbeta = t( bbeta[-1,])
n=dim(CRSPmon)[1]
sigeps = (n-1)/(n-4) * as.matrix((var(as.matrix(fit$resid))))
sigeps = diag(as.matrix(sigeps))
sigeps = diag(sigeps,nrow=3)
cov_equities = bbeta %*% sigF %*% t(bbeta) + sigeps

#compare the sample covariance with covariance estimated by factor model
options(digits=5)
sigF
bbeta
sigeps
bbeta %*% sigF %*% t(bbeta)
cov_equities
cov(stocks)

#cross sectional factor model
berndtInvest = read.csv("berndtInvest.csv")
returns = berndtInvest[,-c(1,11,18)]

#three factor loadings with value of ones and zeros
n.stocks = ncol(returns)
asset.names=colnames(returns)
tech.dum = oil.dum = other.dum =matrix(0,n.stocks,1) #generate three dummy vars
rownames(tech.dum) = rownames(oil.dum) =rownames(other.dum) = asset.names
tech.dum[c(4,5,9,13),] = 1
oil.dum[c(3,6,10,11,14),] = 1
other.dum = 1 - tech.dum - oil.dum
betas = cbind(tech.dum,oil.dum,other.dum)
colnames(betas) = c("TECH","OIL","OTHER")
betas
betas=as.data.frame(betas)

#fit cross-sectional factor models and estimate the value of three factors (market, tech, and oil) in each day
factors = matrix(0,nrow=120,ncol=3)
for (i in 1:120)
{
  return_data = cbind(t(returns[i,]),betas)
  colnames(return_data)[1] = "return"
  lmfit = lm(return~betas[,1] + betas[,2], data=return_data)
  factors[i,]= lmfit$coef
}

#plot the value of 3 factors over time
par(mfrow=c(1,3),cex.axis=1.08,cex.lab=1.08,cex.main=1.05)
plot(factors[,1],type="b",lty="dotted",
     lwd=2,xlab="month",ylab="factor",main="market")
plot(factors[,2],lty="dotted",lwd=2,type="b",
     xlab="month",ylab="factor",main="technology")
plot(factors[,3],lty="dotted",lwd=2,type="b",
     xlab="month",ylab="factor",main="oil")


colnames(factors) = c("market","tech","oil")
cor(factors)
options(digits=2)
sqrt(diag(cov(factors)))

acf(factors,ylab="",xlab="lag")


#Statistical Factor Model
equityFunds = read.csv("equityFunds.csv")
fa_none = factanal(equityFunds[,2:9],4,rotation="none")
#fa_vari = factanal(equityFunds[,2:9],4,rotation="varimax")
print(fa_none,cutoff=0.1)
#print(fa_none,cutoff=0.1,sort=T)
#print(fa_vari,cutoff=0.1,sort=T)
#print(fa_vari,cutoff=0.1)
#sum(fa_vari$loadings[,1]^2)
#B=fa_vari$loadings[,]
unique



