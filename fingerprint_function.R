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


fingerprintGener<-function(du,sorted=T){
  formulas<-list(f1,f2,f3,f4,f5,f6)
  
  res<-lapply(1:6,function(i){quap(formulas[[i]], data=du)})
  print(0)
  
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
  res1<-lapply(1:6,function(i){quap(formulas[[i]], data=d1)});print(1)
  res2<-lapply(1:6,function(i){quap(formulas[[i]], data=d2)});print(2)
  res3<-lapply(1:6,function(i){quap(formulas[[i]], data=d3)});print(3)
  res4<-lapply(1:6,function(i){quap(formulas[[i]], data=d4)});print(4)
  res5<-lapply(1:6,function(i){quap(formulas[[i]], data=d5)});print(5)
  res6<-lapply(1:6,function(i){quap(formulas[[i]], data=d6)});print(6)
  
  
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
  
  if(sorted==T){
    dists<-sort(dists)
  }
  
  return(dists)
}


randomSet<-function(bMN=NULL,bNK=NULL,bMK=NULL,n=1000){
  
  if(is.null(bMN)){
    bMN<-runif(1,-1,1)
  }
  if(is.null(bNK)){
    bNK<-runif(1,-1,1)
  }
  if(is.null(bMK)){
    bMK<-runif(1,-1,1)
  }
  
  answer<-sample(1:6,1)
  
  if(answer==1){
    #We have 6 possible data-generating algorithms
    M <- rnorm( n )
    N <- rnorm( n, bMN*M, sd=1)
    K <- rnorm( n, bNK*N + bMK*M, sd=1)
    du <- data.frame(K=K,N=N,M=M)
  }
  if(answer==2){
    M <- rnorm( n )
    K <- rnorm( n, bMK*M, sd=1)
    N <- rnorm( n, bMN*M + bNK*K, sd=1)
    du <- data.frame(K=K,N=N,M=M)
  }
  if(answer==3){
    N <- rnorm( n )
    M <- rnorm( n, bMN*N, sd=1)
    K <- rnorm( n, bNK*N + bMK*M, sd=1)
    du <- data.frame(K=K,N=N,M=M)
  }
  if(answer==4){
    K <- rnorm( n )
    M <- rnorm( n, bMK*K, sd=1)
    N <- rnorm( n, bNK*K + bMN*M, sd=1)
    du <- data.frame(K=K,N=N,M=M)
  }
  if(answer==5){
    N <- rnorm( n )
    K <- rnorm( n, bNK*N, sd=1)
    M <- rnorm( n, bMN*N + bMK*K, sd=1)
    du <- data.frame(K=K,N=N,M=M)
  }
  if(answer==6){
    K <- rnorm( n )
    N <- rnorm( n, bNK*K, sd=1)
    M <- rnorm( n, bMK*K + bMN*N, sd=1)
    du <- data.frame(K=K,N=N,M=M)
  }
  
  return(list(du=du,par.values=c(bMN=bMN,bNK=bNK,bMK=bMK),answer=answer))
}

