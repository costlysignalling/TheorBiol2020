#Logit link function

p<-seq(0,1,0.001)

plot(p,p/(1-p),type="l",col=2,lwd=1,ylim=c(0,20))
abline(h=1,lty=2,col=8)
abline(v=0.5,lty=1,col=8)

plot(p,log(p/(1-p)),type="l",col=2,lwd=1)
abline(h=0,lty=2,col=8)
abline(v=0.5,lty=1,col=8)


#Mapping linear model onto log(odds)
x<-seq(-5,5,0.01)
p<-exp(x)/(1+exp(x))

plot(x,p,col=2,type="l")
abline(v=seq(-5,5,1),col=8)
abline(h=exp(seq(-5,5,1))/(1+exp(seq(-5,5,1))),col=8,lty=2)


#Log link

p<-seq(0.02,50,0.01)

plot(p,log(p),type="l",col=2,lwd=1)
abline(h=0,lty=2,col=8)
abline(v=1,lty=1,col=8)


#Mapping linear model onto log
x<-seq(-5,5,0.01)
s<-exp(x)

plot(x,s,col=2,type="l")
abline(v=seq(-5,5,1),col=8)
abline(h=exp(seq(-5,5,1)),col=8,lty=2)


