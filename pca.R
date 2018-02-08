setwd("C:/Users/vli/Desktop/Spring 2017/FIN 6832 Num Stats Fri/datasets")
#read a data set with 11 yields
datNoOmit = read.table("treasury_yields.txt",header=T)
diffdatNoOmit = diff(as.matrix(datNoOmit[,2:12]))
dat=na.omit(datNoOmit)
diffdat = na.omit(diffdatNoOmit)
n = dim(diffdat)[1]
options(digits=5)

#generate principle components
pca = prcomp(diffdat)
summary(pca)
pca$rotation

eigen.out<-eigen(cov(diffdat))
eigen.out$values
eigen.out$vectors

#plot the eigen values of each component and eigen vectors on all yield vars
par(mfrow=c(2,2))
time = c(1/12,.25,.5,1, 2, 3, 5, 7, 10, 20, 30)
plot(time,as.vector(dat[1,2:12]),ylim=c(0,6),type="b",lty=1,lwd=2,
     ylab="Yield",xlab="T",main="(a)")    #plot three obs.
lines(time,as.vector(dat[486,2:12]),type="b",lty=2,lwd=2,col="red")
lines(time,as.vector(dat[n+2,2:12]),type="b",lty=3,lwd=2,col="blue")
legend("bottomright",c("07/31/01","07/02/07","10/31/08"),lty=c(1,2,3),lwd=2,
       cex=1, col=c("black","red","blue"))
plot(pca,ylim=c(0,0.05),main="(b)") 		#plot the eigen values
plot(time,pca$rotation[,1],,ylim=c(-.8,.8),type="b",lwd=2,ylab="PC",xlab="T",
     main="(c)")        #plot the first three eigen vectors on each yield var
lines(time,pca$rotation[,2],lty=2,type="b",lwd=2,col="red")
lines(time,pca$rotation[,3],lty=3,type="b",lwd=2,col="blue")
lines(0:30,0*(0:30),lwd=1)
legend("bottomright",c("PC 1","PC 2","PC 3"),lty=c(1,2,3),lwd=2,col=c("black","red","blue"))
plot(time,pca$rotation[,1],ylim=c(-.8,.8),type="b",lwd=2,ylab="PC",xlab="T",
     xlim=c(0,3),main="(d)")   #plot the first three eigen vectors on each yield var with years less than 3 
lines(time,pca$rotation[,2],lty=2,type="b",lwd=2,col="red")
lines(time,pca$rotation[,3],lty=3,type="b",lwd=2,col="blue")
lines(0:30,0*(0:30),lwd=1)
legend("bottomright",c("PC 1","PC 2","PC 3"),lty=c(1,2,3),lwd=2,col=c("black","red","blue"))


#Interpret of 3 PCs "shift-twist-butterfly moves"
mu = apply(dat[,2:12],2,mean)  #compute means of all yields
par(mfrow=c(2,2))
plot(time,mu,ylim=c(2.75,4.65),type="b",lwd=4,xlab="T",ylab="Yield",
     main="(a)",xlim=c(0,7),col="black")
lines(time,mu+pca$rotation[,1],lty=5,type="b",lwd=2,col="red")
lines(time,mu-pca$rotation[,1],lty=5,type="b",lwd=2,col="blue")
legend("bottomright",c("mean","mean + PC1",
   "mean -  PC1"),lty=c(1,5,5),lwd=c(4,2,2),col=c("black","red","blue"))
plot(time,mu,ylim=c(2.75,4.65),type="b",lwd=4,xlab="T",ylab="Yield",
     main="(b)", xlim=c(0,7))
lines(time,mu+pca$rotation[,2],lty=5,type="b",lwd=2,col="red")
lines(time,mu-pca$rotation[,2],lty=5,type="b",lwd=2,col="blue")
legend("bottomright",c("mean","mean + PC2","mean -  PC2"),lty=c(1,5,5),
   lwd=c(4,2,2),col=c("black","red","blue"))
plot(time,mu,ylim=c(2.75,4.65),type="b",lwd=4,xlab="T",ylab="Yield",
     main="(c)",xlim=c(0,7))
lines(time,mu+pca$rotation[,3],lty=5,type="b",lwd=2,col="red")
lines(time,mu-pca$rotation[,3],lty=5,type="b",lwd=2,col="blue")
legend("bottomright",c("mean","mean + PC3",
   "mean -  PC3"),lty=c(1,5,5),lwd=c(4,2,2),col=c("black","red","blue"))
par(lwd=1)
plot(time,pca$rotation[,4],ylim=c(-.7,.7),type="b",lwd=2,ylab="PC",xlab="T",
     xlim=c(0,30),main="(d)")
lines(time,pca$rotation[,5],lty=5,type="b",lwd=2,col="red")
lines(0:30,0*(0:30),lwd=1)
legend("topright",c("PC 4","PC 5"),lty=c(1,5,5),lwd=2,col=c("black","red"))


#plot the obs. of 3 PCs over time
par(mfrow=c(1,3))
for (i in 1:3){
  plot(pca$x[,i],main=paste("PC",toString(i)),xlab="day",
       ylab="")
}

#Auto-correlation function of the first 3 PCs and cross-correlations 
acf(pca$x[,1:3],ylab="",xlab="lag")



equityFunds = read.csv("equityFunds.csv")
equityFunds[1:10,]
pairs(equityFunds[,2:9])
pcaEq = prcomp(equityFunds[,2:9])
summary(pcaEq)

par(mfrow=c(1,2))
plot(pcaEq,main="(a)")
Names = names(equityFunds)[2:9]
plot(t(Names), pcaEq$rotation[,1],type="b",ylab="PC",lwd=2,ylim=c(-1.4,2),main="(b)")
lines(pcaEq$rotation[,2],type="b",lty=2,lwd=2,col="red")
lines(pcaEq$rotation[,3],type="b",lty=3,lwd=2,col="blue")
lines(0:9,0*(0:9))
legend("top",c("PC1","PC2","PC3"),lty=c(1,2,3),lwd=2,cex=.65,col=c("black", "red", "blue"))
text(4.35,-1.25, "    EASTEU   LATAM   CHINA   INDIA   ENERGY   MINING   GOLD   WATER",cex=.38)





