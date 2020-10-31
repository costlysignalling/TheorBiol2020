#Slightly expanded rethinking code continuing all the way to full mediation model
library(rethinking)
data(WaffleDivorce)

d <- WaffleDivorce

# standardize variables
d$D <- standardize( d$Divorce )
d$M <- standardize( d$Marriage )
d$A <- standardize( d$MedianAgeMarriage )

mAonly <- quap(
  alist(
    D ~ dnorm( muD , sigmaD ) ,
    muD <- aD + bAD * A ,
    aD ~ dnorm( 0 , 0.2 ) ,
    bAD ~ dnorm( 0 , 0.5 ) ,
    sigmaD ~ dexp( 1 )
  ) , data = d )

mMonly <- quap(
  alist(
    D ~ dnorm( muD , sigmaD ) ,
    muD <- aD + bMD * M ,
    aD ~ dnorm( 0 , 0.2 ) ,
    bMD ~ dnorm( 0 , 0.5 ) ,
    sigmaD ~ dexp( 1 )
  ) , data = d )

mAM <- quap(
  alist(
    D ~ dnorm( muD , sigmaD ) ,
    muD <- aD + bMD*M + bAD*A ,
    aD ~ dnorm( 0 , 0.2 ) ,
    bMD ~ dnorm( 0 , 0.5 ) ,
    bAD ~ dnorm( 0 , 0.5 ) ,
    sigmaD ~ dexp( 1 )
  ) , data = d )

mPATH <- quap(
  alist(
    D ~ dnorm( muD , sigmaD ) ,
    muD <- aD + bMD*M + bAD*A ,
    aD ~ dnorm( 0 , 0.2 ) ,
    bMD ~ dnorm( 0 , 0.5 ) ,
    bAD ~ dnorm( 0 , 0.5 ) ,
    sigmaD ~ dexp( 1 ),
    
    M ~ dnorm( muM , sigmaM ) ,
    muM <- aM + bAM*A ,
    aM ~ dnorm( 0 , 0.2 ) ,
    bAM ~ dnorm( 0 , 0.5 ) ,
    sigmaM ~ dexp( 1 )
    
  ) , data = d )

#See? You do not damage any estimation of slopes in D regression by including the model for M as well
plot( coeftab(mAonly,mMonly,mAM,mPATH) )

#The reason why McElreath does not go "all the way" to compelete mediation model and rather treats the model of M as a separate regression altogether is probably the possible confusion stemming from model comparison. Including the model of M does not add anything to the predictive efficiency (measured as WAIC for example, see Chapetr  for more info), but it prevents us from calculation the WAIC of the M ~ A etc, because it works only with the first (uppermost) response variable.
compare(mAonly,mMonly,mAM,mPATH)

#See? Rearranging the predictor order would not change parameter estimates, but the compare (or WAIC, or PSIS..) function would calculate the information criterion for the model of M instead of the model of D, so the comparison would no longer make sense:
mPATH2 <- quap(
  alist(
    M ~ dnorm( muM , sigmaM ) ,
    muM <- aM + bAM*A ,
    aM ~ dnorm( 0 , 0.2 ) ,
    bAM ~ dnorm( 0 , 0.5 ) ,
    sigmaM ~ dexp( 1 ),
    
    D ~ dnorm( muD , sigmaD ) ,
    muD <- aD + bMD*M + bAD*A ,
    aD ~ dnorm( 0 , 0.2 ) ,
    bMD ~ dnorm( 0 , 0.5 ) ,
    bAD ~ dnorm( 0 , 0.5 ) ,
    sigmaD ~ dexp( 1 )

  ) , data = d )

#Check this out:
plot( coeftab(mAonly,mMonly,mAM,mPATH,mPATH2) )
compare(mAonly,mMonly,mAM,mPATH,mPATH2)

