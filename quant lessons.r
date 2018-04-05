#weiner process
m1 = setModel(drift=0, diffusion=1,hurst=0.5,jump.coeff=100,jump.variable ="z", time.variable="t",state.var="x", 
time.var="t", solve.var="x", xinit=50)
X=simulate(m1,sampling=grid)
plot(X)

#grid = setSampling(Terminal=1, n=1000)

#geometric brownian motion
m1=setModel(drift="mu*s",diffusion="sigma*s", state.var="s",time.var="t",solve.var="s",xinit=100)
X=simulate(m1,true.param=list(mu=0.1,sigma=0.2))
plot(X)

m1=setModel(drift="mu*s",diffusion="sigma*s", state.var="s",time.var="t",solve.var="s",xinit=100)

#probability associated with geometric brownian motion
#probability distribution for target prices at time simnum
simnum=100
dist=c(.31, .52,0.6,0.7, .95)
newsim=function(i){simulate(m1,true.param=list(mu=0.1,sigma=0.2))@data@original.data}
newsim(1)
sim=sapply(1:simnum,function(x)newsim(x))
m2=t(sim)
m2
apply(m2,2,mean)
tile=sapply(1:100,function(x)quantile(m2[,x], dist) )
tile

#Vasicek model
require(yuima)
m1=setModel(drift="theta*(mu-x)",diffusion="sigma", state.var="x",time.var="t",solve.var="x",xinit=0.5)
X=simulate(m1,true.param=list(mu=0.1,sigma=0.2,theta=2))
plot(X)


require(yuima)
m1=setModel(drift="theta*(mu-x)",diffusion="sigma", state.var="x",time.var="t",solve.var="x",xinit=0.5)
X=simulate(m1,true.param=list(mu=0.1,sigma=0.2,theta=2))
plot(X)
simnum=100
dist=c(.31, .52,0.6,0.7, .95)
newsim=function(i){simulate(m1,true.param=list(mu=0.1,sigma=0.2,theta=2))@data@original.data}
newsim(1)
sim=sapply(1:simnum,function(x)newsim(x))
m2=t(sim)
apply(m2,2,mean)
tile=sapply(1:100,function(x)quantile(m2[,x], dist) )
tile
