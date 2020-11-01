## R code 5.45
library(rethinking)

data(Howell1)
d <- Howell1
str(d)

mIndicator <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a+bSex*male ,
    a    ~ dnorm( 178 , 20 ) ,
    bSex ~ dnorm( 0 , 10 ) ,
    sigma ~ dunif( 0 , 50 )
  ) , data=d )

precis(mIndicator)

#extract posterior and plot parameter correlations
post<-extract.samples(mIndicator)
plot(post$a,post$bSex)

#Posterior predictions for men and women
postpreds<-data.frame(W=post$a,M=post$a+post$bSex)
precis(postpreds)
plot(precis(postpreds))

#McElreath's demonstration of higher uncertainty in men if indiator variable model is used:
mu_female <- rnorm(1e4,178,20)
mu_male <- rnorm(1e4,178,20) + rnorm(1e4,0,10)
precis( data.frame( mu_female , mu_male ) )

#Index variable is sex
d$sex <- ifelse( d$male==1 , 2 , 1 )
str( d$sex )

## R code 5.48
mIndex <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a[sex] ,
    a[sex] ~ dnorm( 178 , 20 ) ,
    sigma ~ dunif( 0 , 50 )
  ) , data=d )

precis( mIndex , depth=2 )

#Extract samples and than compute the contrast
post <- extract.samples(mIndex)
post$diff_fm <- post$a[,1] - post$a[,2]

#Parameters are no longer intercorrelated
plot(post$a[,1],post$a[,2])
precis( post , depth=2 )

#You can also plot it like this
plot(precis( mIndex , depth=2 ),pars=c("a[1]","a[2]"))


#You can also easily inspect implied contrast prior:

prior <- extract.prior(mIndex)
prior$diff_fm <- prior$a[,1] - prior$a[,2]

precis( prior , depth=2 )

