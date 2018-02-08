##R lab##
library("faraway")
library(MASS)
setwd("C:\\Users\\vli\\Desktop\\Spring 2017\\FIN 6832 Num Stats Fri\\datasets")

#Estimate default probabilities by nonlinear regression

DefaultData = read.table("DefaultData.txt",header=T)
attach(DefaultData)
freq2=freq/100
y = log(freq2[freq2>0])
fit_bow = lm(y ~ rating[freq>0])        #linear reg by log transformation of default freq
fit_nls = nls(freq2 ~ exp(b1+b2*rating), start=list(b1=-5,b2=.5))  #non-linear reg
#Box-cox transformation
boxcox(freq2+1e-6 ~ rating,ylab="log-likelihood")
fit_tbs = nls(sqrt(freq2) ~ exp(b1/2+b2*rating/2), start=list(b1=-6,b2=.5))  #Transformation-both-sides reg
sum_nls = summary(fit_nls)
coef_nls = as.numeric(sum_nls$coef[1:2])
sum_tbs = summary(fit_tbs)
coef_tbs = as.numeric(sum_tbs$coef[1:2])
rate_grid = seq(1,16,by=.01)
coef_bow = fit_bow$coefficients


par(mfrow=c(1,2))
plot(rating,freq2,ylim=c(-.0001,.13),pch="*",ylab="frequency",cex=1.5)
lines(rate_grid,exp( coef_nls[1]+coef_nls[2]*rate_grid))
legend("topleft",c("exponential","data"),lty=c(1,NA),
       pch=c("","*"),pt.cex=c(1, 1.5))
plot(rate_grid, (coef_bow[1]+rate_grid*coef_bow[2]),
     type="l",ylim=c(-14.15,1),xlab="rating",ylab="log(default probability)")
lines(rate_grid,( coef_nls[1]+coef_nls[2]*rate_grid) ,lty=2,col="red")
lines(rate_grid,( coef_tbs[1]+coef_tbs[2]*rate_grid) ,lty=6,col="blue")
points(rating,log(freq2+1e-6))
legend("topleft",c("BOW","nonlinear","tbs","data"),lty=c(1,2,6,NA),
       pch=c("","","","o"),col=c("black","red","blue"))

#Residual plots for nonlinear reg
par(mfrow=c(1,2))
fitted_nls = -sum_nls$resid+freq2  #fitted value of default frequencies
plot(fitted_nls,abs(sum_nls$resid),xlab="fitted values",
     ylab="absolute residual")
fit_loess  = loess(abs(sum_nls$resid)~ fitted_nls,span=1,deg=1) #local polynomial regression fitting 
ord_nls = order(fitted_nls)
lines(fitted_nls[ord_nls],fit_loess$fit[ord_nls])
qqnorm(sum_nls$resid,datax=T,main="",ylab="sample quantiles",
       xlab="theoretical quantiles")
qqline(sum_nls$resid,datax=T)


#Residual plots for nonlinear reg
par(mfrow=c(1,2))
fitted_tbs=sqrt(freq2)-sum_tbs$resid
plot(fitted_tbs,abs(sum_tbs$resid),xlab="fitted values",
     ylab="absolute residual")
fit_loess  = loess(abs(sum_tbs$resid)~ fitted_tbs,span=1,deg=1) #local polynomial regression fitting 
ord_tbs = order(fitted_tbs)
lines(fitted_tbs[ord_tbs],fit_loess$fit[ord_tbs])
qqnorm(sum_tbs$resid,datax=T,main="",ylab="sample quantiles",
       xlab="theoretical quantiles")
qqline(sum_tbs$resid,datax=T)


#Prob to get a credit card
library("AER")
data("CreditCard") 
CreditCard_clean = CreditCard[CreditCard$age>18,]
attach(CreditCard_clean)
names(CreditCard)
fit1= glm(card~log(reports+1)+income+log(share)+age+owner+dependents+months,
          family="binomial",data=CreditCard_clean)
summary(fit1)
stepAIC(fit1)

par(mfrow=c(3,3)) 
hist(reports,main="reports")
hist(income, main="income")
hist(share, main="share")
hist(age, main="age")
owner2 = c( sum((owner=="yes")),sum((owner=="no")))
hist(as.numeric(owner), main="owner",
     breaks=2,xlab=" no   yes",axes=F,ylab="")
h=hist(dependents,main="dependents",breaks=(0:7)-.5)
hist(months,main="months")
hist(log(share),main="log(share)")
hist(log(reports+1),main="log(reports+1)")

log_reports_c = log(reports+1)
log_reports_c = log_reports_c - mean(log_reports_c)
income_c = income - mean(income)
log_share_c = log(share) - mean(log(share))
dependents_c = dependents - mean(dependents)
glm_fit02 = glm(card~log_reports_c+income_c+log_share_c+dependents_c,
                family="binomial",data=CreditCard_clean)
summary(glm_fit02)
income_grid = seq(min(income),max(income),.01)
share_grid = exp(seq(min(log(share)),max(log(share)),.01))
share_grid2 = log(share_grid) - mean(log(share))
reports_grid = 0:14
reports_grid2 = log(reports_grid+1) - mean(log(reports+1))


par(mfrow=c(2,2))
plot(reports_grid, plogis(9.5238 -2.8953 * reports_grid2 ),type="b",
     lwd=2,xlab="reports",ylab="P(accept)",ylim=c(0,1))
plot(income_grid, plogis(9.5238 + 0.8717 *(income_grid-mean(income)) ),type="l",
     cex=2,lwd=2,xlab="income",ylab="P(accept)",ylim=c(0,1))
plot(log(share_grid), plogis(9.5238  + 3.3102  * share_grid2 ),type="l",
     cex=2,lwd=2,xlab="log(share)",ylab="P(accept)",ylim=c(0,1))
plot(0:6,plogis(9.5238 - .5506*((0:6)-mean(dependents)) ),type="b",
     lwd=2,xlab="dependents",ylab="P(accept)",ylim=c(0,1))


par(mfrow=c(2,2))
plot(log(share),as.numeric(card)-1,ylab="card",main="(a)" )
plot(log(share),reports,main="(b)" )
plot(log(share),income,main="(c)" )
plot(log(share),majorcards,main="(d)" )
detach(CreditCard_clean)



