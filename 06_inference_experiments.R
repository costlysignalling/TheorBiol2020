library(rethinking)
#First prepapre the dataset
data(milk)
d <- milk

## R code 5.29
d$K <- standardize( d$kcal.per.g )
d$N <- standardize( d$neocortex.perc )
d$M <- standardize( log(d$mass) )

#We have 6 Markov-equivalent DAGs 
dag <- dagitty( "dag{
    M -> K <- N
    M -> N }" )
coordinates(dag) <- list( x=c(M=0,K=1,N=2) , y=c(M=0.5,K=1,N=0.5) )
MElist <- equivalentDAGs(dag)
drawdag(MElist,xlim=c(0,10),ylim=c(0,10))


#We can easily simulate an equivalent dataset
n <- 1000

bMN<-0.4
bMK<--0.35
bNK<-0.5


#We have 6 possible data-generating algorithms
M <- rnorm( n )
N <- rnorm( n, bMN*M, sd=sqrt(1-bMN^2))
K <- rnorm( n, bNK*N + bMK*M, sd=sqrt(1-(bMK)^2-(bMN*bNK)^2))

d1 <- data.frame(K=K,N=N,M=M)


M <- rnorm( n )
K <- rnorm( n, bMK*M, sd=sqrt(1-bMK^2))
N <- rnorm( n, bMN*M + bNK*K, sd=sqrt(1-(bMN)^2-(bMK*bNK)^2))

d2 <- data.frame(K=K,N=N,M=M)


N <- rnorm( n )
M <- rnorm( n, bMN*N, sd=sqrt(1-bMN^2))
K <- rnorm( n, bNK*N + bMK*M, sd=sqrt(1-(bNK)^2-(bMN*bMK)^2))

d3 <- data.frame(K=K,N=N,M=M)


K <- rnorm( n )
M <- rnorm( n, bMK*K, sd=sqrt(1-bMK^2))
N <- rnorm( n, bNK*K + bMN*M, sd=sqrt(1-(bNK)^2-(bMK*bMN)^2))

d4 <- data.frame(K=K,N=N,M=M)


N <- rnorm( n )
K <- rnorm( n, bNK*N, sd=sqrt(1-bNK^2))
M <- rnorm( n, bMN*N + bMK*K, sd=sqrt(1-(bMN)^2-(bNK*bMK)^2))

d5 <- data.frame(K=K,N=N,M=M)


K <- rnorm( n )
N <- rnorm( n, bNK*K, sd=sqrt(1-bNK^2))
M <- rnorm( n, bMK*K + bMN*N, sd=sqrt(1-(bMK)^2-(bNK*bMN)^2))

d6 <- data.frame(K=K,N=N,M=M)

#All SDs should be close to 1
lapply(list(d1,d2,d3,d4,d5,d6),function(d){lapply(d,sd)})



#And 6 formulas
f1<-alist(
  K ~ dnorm( muK , sigmaK ) ,
  muK <- aK + bMK*M + bNK*N ,
  aK ~ dnorm( 0 , 1 ) ,
  bNK ~ dnorm( 0 , 1 ) ,
  bMK ~ dnorm( 0 , 1 ) ,
  sigmaK ~ dexp( 1 ),
  
  N ~ dnorm( muN , sigmaN ) ,
  muN <- aN + bMN*M ,
  aN ~ dnorm( 0 , 1 ),
  bMN ~ dnorm( 0 , 1 ),
  sigmaN ~ dexp( 1 ))


f2<-alist(
  N ~ dnorm( muN , sigmaN ) ,
  muN <- aN + bMN*M + bNK*K ,
  aN ~ dnorm( 0 , 1 ),
  bNK ~ dnorm( 0 , 1 ),
  bMN ~ dnorm( 0 , 1 ),
  sigmaN ~ dexp( 1 ),
  
  K ~ dnorm( muK , sigmaK ) ,
  muK <- aK + bMK*M ,
  aK ~ dnorm( 0 , 1 ) ,
  bMK ~ dnorm( 0 , 1 ) ,
  sigmaK ~ dexp( 1 ))


f3<-alist(
  K ~ dnorm( muK , sigmaK ) ,
  muK <- aK + bMK*M + bNK*N ,
  aK ~ dnorm( 0 , 1 ) ,
  bMK ~ dnorm( 0 , 1 ) ,
  bNK ~ dnorm( 0 , 1 ) ,
  sigmaK ~ dexp( 1 ),
  
  M ~ dnorm( muM , sigmaM ) ,
  muM <- aM + bMN*N ,
  aM ~ dnorm( 0 , 1 ),
  bMN ~ dnorm( 0 , 1 ),
  sigmaM ~ dexp( 1 ))


f4<-alist(
  N ~ dnorm( muN , sigmaN ) ,
  muN <- aN + bMN*M + bNK*K ,
  aN ~ dnorm( 0 , 1 ),
  bMN ~ dnorm( 0 , 1 ),
  bNK ~ dnorm( 0 , 1 ),
  sigmaN ~ dexp( 1 ),
  
  M ~ dnorm( muM , sigmaM ) ,
  muM <- aM + bMK*K ,
  aM ~ dnorm( 0 , 1 ) ,
  bMK ~ dnorm( 0 , 1 ) ,
  sigmaM ~ dexp( 1 ))


f5<-alist(
  M ~ dnorm( muM , sigmaM ) ,
  muM <- aM + bMN*N + bMK*K ,
  aM ~ dnorm( 0 , 1 ) ,
  bMK ~ dnorm( 0 , 1 ) ,
  bMN ~ dnorm( 0 , 1 ),
  sigmaM ~ dexp( 1 ),
  
  K ~ dnorm( muK , sigmaK ) ,
  muK <- aK + bNK*N ,
  aK ~ dnorm( 0 , 1 ),
  bNK ~ dnorm( 0 , 1 ),
  sigmaK ~ dexp( 1 ))


f6<-alist(
  M ~ dnorm( muM , sigmaM ) ,
  muM <- aM + bMK*K + bMN*N ,
  aM ~ dnorm( 0 , 1 ) ,
  bMN ~ dnorm( 0 , 1 ),
  bMK ~ dnorm( 0 , 1 ) ,
  sigmaM ~ dexp( 1 ),
  
  N ~ dnorm( muN , sigmaN ) ,
  muN <- aN + bNK*K ,
  aN ~ dnorm( 0 , 1 ),
  bNK ~ dnorm( 0 , 1 ),
  sigmaN ~ dexp( 1 ))

#Restart the plotting window
dev.off()

#What does it look like if D1 is the true generative model and all possible models with corresponding DAGs are evaluated 
m11 <- quap(f1, data=d1 )
m21 <- quap(f2, data=d1 )
m31 <- quap(f3, data=d1 )
m41 <- quap(f4, data=d1 )
m51 <- quap(f5, data=d1 )
m61 <- quap(f6, data=d1 )

#We can plot the means
plot( coeftab(m11,m21,m31,m41,m51,m61) , pars=c("bMN","bMK","bNK") )
#These lines mark true parameter values
lines(x=rep(bMN,2),y=c(16.5,22.5),col=2,lwd=1)
lines(x=rep(bMK,2),y=c(8.5,14.5),col=2,lwd=1)
lines(x=rep(bNK,2),y=c(6.5,0.5),col=2,lwd=1)
#This line indicates the true generative model
abline(h=c(0,8,16)+1,col="#0066FF55",lwd=2,lty=2)

#It is not possible to distinguish between model 1 and 3. But that information is useful. At least you know, that the true model might be close to one of these two. Each pattern of mediation fit (and associated error patterns) is like a finger-print of the set of plausible models.

#Another pattern is obtained if dataset 2 is used
m12 <- quap(f1, data=d2 )
m22 <- quap(f2, data=d2 )
m32 <- quap(f3, data=d2 )
m42 <- quap(f4, data=d2 )
m52 <- quap(f5, data=d2 )
m62 <- quap(f6, data=d2 )

plot( coeftab(m12,m22,m32,m42,m52,m62) , pars=c("bMN","bMK","bNK") )
lines(x=rep(bMN,2),y=c(16.5,22.5),col=2,lwd=1)
lines(x=rep(bMK,2),y=c(8.5,14.5),col=2,lwd=1)
lines(x=rep(bNK,2),y=c(6.5,0.5),col=2,lwd=1)
abline(h=c(0,8,16)+2,col="#0066FF55",lwd=2,lty=2)

#I can wrap this up into a fucntion
fingerprint<-function(dn=1){
  for(i in 1:6){
    ask<-paste("m",i,dn,"<- quap(f",i,", data=d",dn," )",sep="")
    eval(parse(text=ask))
  }
  ask<-paste("plot( coeftab(m1",dn,",m2",dn,",m3",dn,",m4",dn,",m5",dn,",m6",dn,") , pars=c(\"bMN\",\"bMK\",\"bNK\") )",sep="")
  eval(parse(text=ask))
  lines(x=rep(bMN,2),y=c(16.5,22.5),col=2,lwd=1)
  lines(x=rep(bMK,2),y=c(8.5,14.5),col=2,lwd=1)
  lines(x=rep(bNK,2),y=c(6.5,0.5),col=2,lwd=1)
  abline(h=c(0,8,16)+dn,col="#0066FF55",lwd=2,lty=2)
}

#This will daw the fingerprint plot from pre-prepared datasets and formulas
fingerprint(dn=1)
fingerprint(dn=2)
fingerprint(dn=3)
fingerprint(dn=4)
fingerprint(dn=5)
fingerprint(dn=6)



#This function will attemp to do the same with a dataset created with an unknow generative process (one of the six processes above)
#It extractes estimates for all model parameters, attempts to recreate the dataset and then analyses each dataset with all 6 formulas. Than overlays the fingerprint plots against the empirical fingerprint plot.

ds<-list(d1,d2,d3,d4,d5,d6)
answer<-sample(1:6,1)
du<-ds[[answer]]



formulas<-list(f1,f2,f3,f4,f5,f6)

res<-lapply(1:6,function(i){quap(formulas[[i]], data=du )})

#Measure the sample size
n<-nrow(du)

#Estimate model parameters and generate allegedly equivalent datasets
e<-precis(res[[1]])[,1]
M <- rnorm( n )
N <- rnorm( n, e[5]+e[6]*M, sd=e[7])
K <- rnorm( n, e[1]+e[2]*N + e[3]*M, sd=e[4])
d1 <- data.frame(K=K,N=N,M=M)

e<-precis(res[[2]])[,1]
M <- rnorm( n )
K <- rnorm( n, e[5]+e[6]*M, sd=e[7])
N <- rnorm( n, e[1]+e[2]*K + e[3]*M, sd=e[4])
d2 <- data.frame(K=K,N=N,M=M)

e<-precis(res[[3]])[,1]
N <- rnorm( n )
M <- rnorm( n, e[5]+e[6]*N, sd=e[7])
K <- rnorm( n, e[1]+e[2]*M + e[3]*N, sd=e[4])
d3 <- data.frame(K=K,N=N,M=M)

e<-precis(res[[4]])[,1]
K <- rnorm( n )
M <- rnorm( n, e[5]+e[6]*K, sd=e[7])
N <- rnorm( n, e[1]+e[2]*M + e[3]*K, sd=e[4])
d4 <- data.frame(K=K,N=N,M=M)

e<-precis(res[[5]])[,1]
N <- rnorm( n )
K <- rnorm( n, e[5]+e[6]*N, sd=e[7])
M <- rnorm( n, e[1]+e[2]*K + e[3]*N, sd=e[4])
d5 <- data.frame(K=K,N=N,M=M)

e<-precis(res[[6]])[,1]
K <- rnorm( n )
N <- rnorm( n, e[5]+e[6]*K, sd=e[7])
M <- rnorm( n, e[1]+e[2]*N + e[3]*K, sd=e[4])
d6 <- data.frame(K=K,N=N,M=M)

#Repeat the same evaluation precedure for all six simulated datasets
res1<-lapply(1:6,function(i){quap(formulas[[i]], data=d1 )})
res2<-lapply(1:6,function(i){quap(formulas[[i]], data=d2 )})
res3<-lapply(1:6,function(i){quap(formulas[[i]], data=d3 )})
res4<-lapply(1:6,function(i){quap(formulas[[i]], data=d4 )})
res5<-lapply(1:6,function(i){quap(formulas[[i]], data=d5 )})
res6<-lapply(1:6,function(i){quap(formulas[[i]], data=d6 )})

#Get empirical coef table
colt<-"#FF005580"
plot( coeftab(res[[1]],res[[2]],res[[3]],res[[4]],res[[5]],res[[6]]) , pars=c("bMN","bMK","bNK") ,prob=0.89)
upr<-lapply(res,function(m)(precis(m)[match(c("bNK","bMK","bMN"),rownames(precis(m))),]))
mest<-unlist(lapply(1:6,function(i){upr[[i]][,1]}))


#Plot and avaluate all simulated datasets and their evaluations
plot( coeftab(res1[[1]],res1[[2]],res1[[3]],res1[[4]],res1[[5]],res1[[6]]) , pars=c("bMN","bMK","bNK"))
for(i in 1:6){
points(upr[[i]][,1],c(0,8,16)+i,col=2)
segments(upr[[i]][,3],c(0,8,16)+i,upr[[i]][,4],c(0,8,16)+i,col=colt,lwd=2)
}
upr1<-lapply(res1,function(m)(precis(m)[match(c("bNK","bMK","bMN"),rownames(precis(m))),]))
mest1<-unlist(lapply(1:6,function(i){upr1[[i]][,1]}))
dist1<-sum(abs(mest1-mest))
title("model 1",adj=0)
title("empirical",adj=1,col.main=colt)
mtext(paste("dist =",round(dist1,3)))


plot( coeftab(res2[[1]],res2[[2]],res2[[3]],res2[[4]],res2[[5]],res2[[6]]) , pars=c("bMN","bMK","bNK"))
for(i in 1:6){
  points(upr[[i]][,1],c(0,8,16)+i,col=2)
  segments(upr[[i]][,3],c(0,8,16)+i,upr[[i]][,4],c(0,8,16)+i,col=colt,lwd=2)
}
upr2<-lapply(res2,function(m)(precis(m)[match(c("bNK","bMK","bMN"),rownames(precis(m))),]))
mest2<-unlist(lapply(1:6,function(i){upr2[[i]][,1]}))
dist2<-sum(abs(mest2-mest))
title("model 2",adj=0)
title("empirical",adj=1,col.main=colt)
mtext(paste("dist =",round(dist2,3)))


plot( coeftab(res3[[1]],res3[[2]],res3[[3]],res3[[4]],res3[[5]],res3[[6]]) , pars=c("bMN","bMK","bNK"))
for(i in 1:6){
  points(upr[[i]][,1],c(0,8,16)+i,col=2)
  segments(upr[[i]][,3],c(0,8,16)+i,upr[[i]][,4],c(0,8,16)+i,col=colt,lwd=2)
}
upr3<-lapply(res3,function(m)(precis(m)[match(c("bNK","bMK","bMN"),rownames(precis(m))),]))
mest3<-unlist(lapply(1:6,function(i){upr3[[i]][,1]}))
dist3<-sum(abs(mest3-mest))
title("model 3",adj=0)
title("empirical",adj=1,col.main=colt)
mtext(paste("dist =",round(dist3,3)))


plot( coeftab(res4[[1]],res4[[2]],res4[[3]],res4[[4]],res4[[5]],res4[[6]]) , pars=c("bMN","bMK","bNK"))
for(i in 1:6){
  points(upr[[i]][,1],c(0,8,16)+i,col=2)
  segments(upr[[i]][,3],c(0,8,16)+i,upr[[i]][,4],c(0,8,16)+i,col=colt,lwd=2)
}
upr4<-lapply(res4,function(m)(precis(m)[match(c("bNK","bMK","bMN"),rownames(precis(m))),]))
mest4<-unlist(lapply(1:6,function(i){upr4[[i]][,1]}))
dist4<-sum(abs(mest4-mest))
title("model 4",adj=0)
title("empirical",adj=1,col.main=colt)
mtext(paste("dist =",round(dist4,3)))


plot( coeftab(res5[[1]],res5[[2]],res5[[3]],res5[[4]],res5[[5]],res5[[6]]) , pars=c("bMN","bMK","bNK"))
for(i in 1:6){
  points(upr[[i]][,1],c(0,8,16)+i,col=2)
  segments(upr[[i]][,3],c(0,8,16)+i,upr[[i]][,4],c(0,8,16)+i,col=colt,lwd=2)
}
upr5<-lapply(res5,function(m)(precis(m)[match(c("bNK","bMK","bMN"),rownames(precis(m))),]))
mest5<-unlist(lapply(1:6,function(i){upr5[[i]][,1]}))
dist5<-sum(abs(mest5-mest))
title("model 5",adj=0)
title("empirical",adj=1,col.main=colt)
mtext(paste("dist =",round(dist5,3)))


plot( coeftab(res6[[1]],res6[[2]],res6[[3]],res6[[4]],res6[[5]],res6[[6]]) , pars=c("bMN","bMK","bNK"))
for(i in 1:6){
  points(upr[[i]][,1],c(0,8,16)+i,col=2)
  segments(upr[[i]][,3],c(0,8,16)+i,upr[[i]][,4],c(0,8,16)+i,col=colt,lwd=2)
}
upr6<-lapply(res6,function(m)(precis(m)[match(c("bNK","bMK","bMN"),rownames(precis(m))),]))
mest6<-unlist(lapply(1:6,function(i){upr6[[i]][,1]}))
dist6<-sum(abs(mest6-mest))
title("model 6",adj=0)
title("empirical",adj=1,col.main=colt)
mtext(paste("dist =",round(dist6,3)))


dists<-c(dist1,dist2,dist3,dist4,dist5,dist6)
names(dists)<-paste("model",1:6)
(sorted<-sort(dists))


#I have converted this to a function. It is now a separate script fingeprint_function.R
#It is not that awesome, but it is a start..

library(rethinking)
source("fingerprint_function.R")

#You can play with random dataset generation
#This will create random dataset with random parameter values and random mediation structure (one of the 6 DAGs)
RS<-randomSet(n=1000)

#Extract unknow dataset
du<-RS$du

fingerprintGener(du)
RS$answer
RS$par.values

#It works fine for me. Frequently, the corrects model (see RS$answer) is not the best bet, but most of the time it is at least the second best.
#Optimally we should pre-register the study now.
#So now we can try it with the emprical Milk dataset.

library(rethinking)
data(milk)
d <- milk

d$K <- standardize( d$kcal.per.g )
d$N <- standardize( d$neocortex.perc )
d$M <- standardize( log(d$mass) )

du <- d[ complete.cases(d$K,d$N,d$M) , ]

fingerprintGener(du)
#Model 1 seems to be most likely in my simulation 2 or 3 are closely second
#But it differs (a lot) from simualtion to simulation.
#You could do the same procedure many times and demonstrate the distribution of distances as a result. 
