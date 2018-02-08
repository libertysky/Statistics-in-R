install.packages("car")
install.packages("lmtest")

library(MASS)
library(car)
library(lmtest)

##import data

mydata<-read.table("C:/Users/lxm133730/Documents/FIN6306/2014fall/6306data.txt", header=TRUE, sep="\t")
par(mfrow=c(2,1))   #combine multiple plots into one graph
plot(mydata$x,type="l",  col="red", xlab="time", ylab="closing values", main="Stock A")  #type="l' refers to a "line" plot
plot(mydata$y,type="l", col="blue", xlab="time", ylab="closing values", main="Stock B")  

plot(mydata, main="Scatter plot")

##Calculate returns
return.x<-diff(log(mydata$x))
return.y<-diff(log(mydata$y))
summary(return.x)
summary(return.y)

##Regression analysis
fit<-lm(return.x~return.y, data=mydata)
summary(fit)

coefficients(fit) #model coefficients
confint(fit, level=0.95) # CIs for model parameters
anova(fit)

##Predict new return under 0.0005 of index return
newdata<-data.frame(return.y=0.0005)
predict(fit,newdata,interval="predict", level=0.95)

##Model Diagnostics 

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit)

# Influence Plot 
influencePlot(fit, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

# Test for heteroskadasticity
bptest(fit)

# Evaluate Collinearity
vif(fit) #variance inflation factors for multi regression

# Test for Autocorrelated Errors
durbinWatsonTest(fit)




