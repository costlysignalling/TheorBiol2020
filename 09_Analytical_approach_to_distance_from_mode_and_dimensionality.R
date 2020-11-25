library(rethinking)

mu<-0
sd<-1

x<-seq(0,5,0.01)
dens1<-dnorm(x,mu,sd)
area1<-ifelse(x==mu,1,2)

dmvnorm

dmvnorm(c(1,0),mean=rep(mu,2),sigma=diag(2))

#Points at the same distance have the identical probability density
dmvnorm(c(sqrt(2),0),mean=rep(mu,2),sigma=diag(2))
dmvnorm(c(1,1),mean=rep(mu,2),sigma=diag(2))

#Circle cicumflex
dens2<-sapply(x,function(dist){dmvnorm(c(dist,0),mean=rep(mu,2),sigma=diag(2))})
area2<-2*pi*x

#Sphere area
dens3<-sapply(x,function(dist){dmvnorm(c(dist,0,0),mean=rep(mu,3),sigma=diag(3))})
area3<-4*pi*(x^2)
              

par(mfrow=c(3,1),mar=c(3.5,3.5,2.5,0.5),mgp=c(2,0.7,0))
plot(x,dens1,type="l",col=4,xlab="")
plot(x,area1,type="l",col=4,xlab="")
plot(x,dens1*area1,type="l",col=4,xlab="Distance from mode")


par(mfrow=c(3,1),mar=c(3.5,3.5,2.5,0.5),mgp=c(2,0.7,0))
plot(x,dens2,type="l",col=2,xlab="")
plot(x,area2,type="l",col=2,xlab="")
plot(x,dens2*area2,type="l",col=2,xlab="Distance from mode")


par(mfrow=c(3,1),mar=c(3.5,3.5,2.5,0.5),mgp=c(2,0.7,0))
plot(x,dens3,type="l",col=3,xlab="")
plot(x,area3,type="l",col=3,xlab="")
plot(x,dens3*area3,type="l",col=3,xlab="Distance from mode")



par(mfrow=c(3,1),mar=c(3.5,3.5,1.5,0.5),mgp=c(2,0.7,0))
plot(x,dens1,type="l",col=4,xlab="")
title("1-dimensional normal distribution",col.main=4,adj=0)
plot(x,dens2,type="l",col=2,xlab="")
title("2-dimensional normal distribution",col.main=2,adj=0)
plot(x,dens3,type="l",col=3,xlab="Distance from mode")
title("3-dimensional normal distribution",col.main=3,adj=0)


par(mfrow=c(3,1),mar=c(3.5,3.5,1.5,0.5),mgp=c(2,0.7,0))
plot(x,area1,type="l",col=4,xlab="")
title("1-dimensional normal distribution",col.main=4,adj=0)
plot(x,area2,type="l",col=2,xlab="")
title("2-dimensional normal distribution",col.main=2,adj=0)
plot(x,area3,type="l",col=3,xlab="Distance from mode")
title("3-dimensional normal distribution",col.main=3,adj=0)


par(mfrow=c(3,1),mar=c(3.5,3.5,1.5,0.5),mgp=c(2,0.7,0))
plot(x,dens1*area1,type="l",col=4,xlab="")
title("1-dimensional normal distribution",col.main=4,adj=0)
plot(x,dens2*area2,type="l",col=2,xlab="")
title("2-dimensional normal distribution",col.main=2,adj=0)
plot(x,dens3*area3,type="l",col=3,xlab="Distance from mode")
title("3-dimensional normal distribution",col.main=3,adj=0)


