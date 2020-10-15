#Analytical vs sampled posterior
library(rethinking)

#We have tossed 6 waters and three Lands
W<-6
L<-3

d<-list(W=W,L=L)

#If we use quap, we yield analytical posterior. Here it is a posterior characterized by 2 parameters (mean and SD) of a normal distribution probability density function.
globe.qa <- quap(
  alist(
    W ~ dbinom( W+L ,p) , 
    p ~ dunif(0,1)    
  ) ,
  data=d )

# display summary of quadratic approximation
precis( globe.qa )
curve( dnorm( x , 0.67 , 0.16 ) , lty=1, col=2 )

#If the posterior defined like this, the cornerstone fucntion is dnorm. The probability density function defined as:

mydens<-function(x,mean=0,sd=1){(1/(sd*sqrt(2*pi)))*exp(-(1/2)*((x-mean)/sd)^2)}

A<-mydens(seq(0,1,0.1),mean=0.67,sd=0.16)
B<-dnorm(seq(0,1,0.1),mean=0.67,sd=0.16)

#exacly the same
A
B

#But you need inetgral calculus to summerize this posterior, which can be difficult and annoying.
#So it is better to replace the analytical approach with sampling. To trade dnorm() for rnorm().

#You just use the function to generate a desired (10 000 should be enough) number of samples from the distribution (no matter if defined analyticaly or otherwise).
post<-rnorm(10000,mean=0.67,sd=0.16)

#Then, our posterior is just a vector of numbers. It is no longer a formula. It is "data". And you probably have a robust intuition on how to summerize data. :)
post

mean(post)
chainmode(post) #you need to use this function, because in the sampled posterior almost every value has only one copy.
median(post)
quantile(post,seq(0,1,0.1))
#etc...

#The density plot is very easy to draw (kde this method uses KDE)
dens(post,xlim=c(0,1))

plot(density(post),xlim=c(0,1))
curve( dnorm( x , 0.67 , 0.16 ) , lty=1, col=2 ,add=T)


#The function extract.samples does all this for you for any rethinking model regardless of the fitting function and model complexity.
#Compuationaly intensive sampling methods like MCMC are just these samples. They have no analytical form outside of the sampling algorithm that generated them.
globe.ul <- ulam(
  alist(
    W ~ dbinom( W+L ,p) , 
    p ~ dunif(0,1)    
  ) ,
  data=d ,chains=4, cores=4, iter=5000)

#Extract the sampled posterior like this
post2<-extract.samples(globe.ul)

#There is no diadvantage to general sampling methods except for the demands on computation time. But that just gets better and better. These problems (boring "computer problems") tend to solve themselves. Let it to the machine. It will sample for a week? So what?
plot(density(post2$p),xlim=c(0,1))

