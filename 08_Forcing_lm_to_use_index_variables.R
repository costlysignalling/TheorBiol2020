#How to force Linear models to do something similar to index interaction models and more (most of the code is verz similar to Statistical Rethinking by Richard McElreath), it only explicates some of the issues posed in the 8th chapter.

library(rethinking)
data(rugged)
d <- rugged

# make log version of outcome
d$log_gdp <- log( d$rgdppc_2000 )

# extract countries with GDP data
dd <- d[ complete.cases(d$rgdppc_2000) , ]

# rescale variables
dd$log_gdp_std <- dd$log_gdp / mean(dd$log_gdp)
dd$rugged_std <- dd$rugged / max(dd$rugged)

# make variable to index Africa (1) or not (2)
dd$cid <- ifelse( dd$cont_africa==1 , 1 , 2 )

#Define three models of increasing complexity
m8.1 <- quap(
  alist(
    log_gdp_std ~ dnorm( mu , sigma ) ,
    mu <- a + b*( rugged_std - 0.215 ) ,
    a ~ dnorm( 1 , 0.1 ) ,
    b ~ dnorm( 0 , 0.3 ) ,
    sigma ~ dexp(1)
  ) , data=dd )

## R code 8.8
m8.2 <- quap(
  alist(
    log_gdp_std ~ dnorm( mu , sigma ) ,
    mu <- a[cid] + b*( rugged_std - 0.215 ) ,
    a[cid] ~ dnorm( 1 , 0.1 ) ,
    b ~ dnorm( 0 , 0.3 ) ,
    sigma ~ dexp( 1 )
  ) , data=dd )

## R code 8.13
m8.3 <- quap(
  alist(
    log_gdp_std ~ dnorm( mu , sigma ) ,
    mu <- a[cid] + b[cid]*( rugged_std - 0.215 ) ,
    a[cid] ~ dnorm( 1 , 0.1 ) ,
    b[cid] ~ dnorm( 0 , 0.3 ) ,
    sigma ~ dexp( 1 )
  ) , data=dd )


#First model
plot( d.A1$rugged_std , d.A1$log_gdp_std , pch=16 , col=rangi2 ,
      xlab="ruggedness (standardized)" , ylab="log GDP (as proportion of mean)" ,
      xlim=c(0,1) ,ylim=c(0.7,1.3))
points( d.A0$rugged_std , d.A0$log_gdp_std , pch=1 , col="black")

mu <- link( m8.1, data=data.frame( cid=1 , rugged_std=rugged_seq ) )
mu_mean <- apply( mu , 2 , mean )
mu_ci <- apply( mu , 2 , PI , prob=0.97 )
lines( rugged_seq , mu_mean , lwd=2 , col=rangi2)
shade( mu_ci , rugged_seq , col=col.alpha(rangi2,0.3) )

mu <- link( m8.1 , data=data.frame( cid=2 , rugged_std=rugged_seq ) )
mu_mean <- apply( mu , 2 , mean )
mu_ci <- apply( mu , 2 , PI , prob=0.97 )
lines( rugged_seq , mu_mean , lwd=2 )
shade( mu_ci , rugged_seq , col=col.alpha("#000000",0.3) )

legend("top",inset=c(0,-0.2),col=c(rangi2,1),legend = c("Africa","Not Africa"),pch=c(16,1),ncol=2,xpd=T,bty = "n")


#Second model
plot( d.A1$rugged_std , d.A1$log_gdp_std , pch=16 , col=rangi2 ,
      xlab="ruggedness (standardized)" , ylab="log GDP (as proportion of mean)" ,
      xlim=c(0,1) ,ylim=c(0.7,1.3))
points( d.A0$rugged_std , d.A0$log_gdp_std , pch=1 , col="black")

mu <- link( m8.2 , data=data.frame( cid=1 , rugged_std=rugged_seq ) )
mu_mean <- apply( mu , 2 , mean )
mu_ci <- apply( mu , 2 , PI , prob=0.97 )
lines( rugged_seq , mu_mean , lwd=2 , col=rangi2)
shade( mu_ci , rugged_seq , col=col.alpha(rangi2,0.3) )

mu <- link( m8.2 , data=data.frame( cid=2 , rugged_std=rugged_seq ) )
mu_mean <- apply( mu , 2 , mean )
mu_ci <- apply( mu , 2 , PI , prob=0.97 )
lines( rugged_seq , mu_mean , lwd=2 )
shade( mu_ci , rugged_seq , col=col.alpha("#000000",0.3))

legend("top",inset=c(0,-0.2),col=c(rangi2,1),legend = c("Africa","Not Africa"),pch=c(16,1),ncol=2,xpd=T,bty = "n")


#Third model
plot( d.A1$rugged_std , d.A1$log_gdp_std , pch=16 , col=rangi2 ,
      xlab="ruggedness (standardized)" , ylab="log GDP (as proportion of mean)" ,
      xlim=c(0,1) ,ylim=c(0.7,1.3))
points( d.A0$rugged_std , d.A0$log_gdp_std , pch=1 , col="black")

mu <- link( m8.3 , data=data.frame( cid=1 , rugged_std=rugged_seq ) )
mu_mean <- apply( mu , 2 , mean )
mu_ci <- apply( mu , 2 , PI , prob=0.97 )
lines( rugged_seq , mu_mean , lwd=2 , col=rangi2)
shade( mu_ci , rugged_seq , col=col.alpha(rangi2,0.3) )

mu <- link( m8.3 , data=data.frame( cid=2 , rugged_std=rugged_seq ) )
mu_mean <- apply( mu , 2 , mean )
mu_ci <- apply( mu , 2 , PI , prob=0.97 )
lines( rugged_seq , mu_mean , lwd=2 )
shade( mu_ci , rugged_seq , col=col.alpha("#000000",0.3) )

legend("top",inset=c(0,-0.2),col=c(rangi2,1),legend = c("Africa","Not Africa"),pch=c(16,1),ncol=2,xpd=T,bty = "n")


#Identify possibly problematic points and plot their k values from PSIS
pk<-round(PSIS( m8.3 , pointwise=TRUE )$k,2)
identify(dd$rugged_std,dd$log_gdp_std,paste(dd$country,pk))

#Compare the models
compare( m8.1 , m8.2 , m8.3 , func=PSIS )


#Usually you work with factor variables in models with qualitative predictors
dd$continent<-as.factor(ifelse(dd$cont_africa,"Africa","Other"))

#The standard interaction model does not give you, what you want. It gives you an intercept (which is now Africa), the change from this intercept to the other factor level, than it gives you the slope in the African subsample and than the difference between the slope in the other nations and the african sample
m1<-lm(log_gdp_std ~ rugged_std*continent ,data=dd)
summary(m1)

#You have to create an opposite indicator variable to the original indicator to harvest the power of index models (basically combiantion of several indicator variables)
dd$cont_not<-ifelse(dd$cont_africa==1,0,1)

#And then you force the lm() unction to forgot about common intecept with the 0+ start, than you add the two indicator predictors and than their interaction with the ruggedness to get "index-like" slope estimations
#Equivalent of the second model (two intercepts)
m2<-lm(log_gdp_std ~ 0+cont_africa+cont_not+rugged_std ,data=dd)
summary(m2)

precis(m8.2,depth=2)

#Equivalent of the third model (two intercepts and two slopes)
m3<-lm(log_gdp_std ~ 0+cont_africa+cont_not+cont_africa:rugged_std+cont_not:rugged_std ,data=dd)
summary(m3)

precis(m8.3,depth=2)

#As you can see, the estimates from the cleverly defined linear models are equivalent to the Bayesian ones that, howver, allow you to xplicitely define model parameters including parameter vectors that use index predictors.
