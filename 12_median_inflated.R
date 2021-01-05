library(rethinking)

#Sample size
N<-200

#Generate x coordinates
x<-runif(N,-1,1)

#Linear terms determining probability of "going for median" (ar any other value)
ap<--2
bp<-1.5

logodds<-ap+bp*x
p<-inv_logit(logodds)

#See? this is x vs p
plot(x,p)
#Generate the median tendency 
med<-rbinom(N,1,prob=p)

#Intercept and slope of the linear model (and sigma of the normal distribution around mean)
a<-0
b<-0.65
sigma<-0.5

#Generate the y vector: Number from normal distribution and value 0.5 if med==1
y<-b*x+rnorm(N,0,sigma)
y[med==1]<--2

#plot the data and show the overdistribution of 0.5 value
plot(x,y,col=2)
abline(h=0.5,lty=2)


#Sample from the models
#Create the dataset
d<-list(x=x,y=y)



#Neat version using custom functon from ulam
m2 <- ulam(
  alist(
    y|y!=0.5 ~ custom( log1m(p) + normal_lpdf(y|mu,sigma) ),
    y|y==0.5 ~ custom( log_mix( p , 0 , normal_lpdf(0.5|mu,sigma) ) ),
    
    mu<-a+b*x,
    a~dnorm(0,0.5),
    b~dnorm(0,1),
    sigma~dexp(1),
    
    logit(p) <- ap+bp*x,
    ap ~ dnorm(-1.5,1),
    bp ~ dnorm(0,1)
    
  ) , data=d )


stancode(m2)

precis(m2)

#Stancode using higher level clever functions equivalent to the ones used in m2
m3code<-"
data{
    vector[200] y;
    vector[200] x;
}
parameters{
    real a;
    real b;
    real<lower=0> sigma;
    real ap;
    real bp;
}
model{
    vector[200] mu;
    vector[200] p;
    bp ~ normal( 0 , 1 );
    ap ~ normal( -1.5 , 1 );
    for ( i in 1:200 ) {
        p[i] = ap + bp * x[i];
        p[i] = inv_logit(p[i]);
    }
    sigma ~ exponential( 1 );
    b ~ normal( 0 , 1 );
    a ~ normal( 0 , 0.5 );
    for ( i in 1:200 ) {
        mu[i] = a + b * x[i];
    }
    for ( i in 1:200 ) 
        if ( y[i] == 0.5 ) target += log_mix(p[i], 0, normal_lpdf(0.5 | mu[i], sigma));
    for ( i in 1:200 ) 
        if ( y[i] != 0.5 ) target += log1m(p[i]) + normal_lpdf(y[i] | mu[i], sigma);
}"


#Stan code ith low level functions - the gaussian probability density is written by hand 

m4code<-"
data{
vector[200] y;
vector[200] x;
}
parameters{
  real a;
  real b;
  real<lower=0> sigma;
  real ap;
  real bp;
}
model{
  vector[200] mu;
  vector[200] p;
  bp ~ normal( 0 , 1 );
  ap ~ normal( -1.5 , 1 );
  for ( i in 1:200 ) {
    p[i] = ap + bp * x[i];
    p[i] = inv_logit(p[i]);
  }
  sigma ~ exponential( 1 );
  b ~ normal( 0 , 1 );
  a ~ normal( 0 , 0.5 );
  for ( i in 1:200 ) {
    mu[i] = a + b * x[i];
  }
  for ( i in 1:200 ) 
    if ( y[i] == 0.5 ) target += log( p[i] + (1-p[i])*(1/(sqrt(2*pi())*sigma))*exp(-0.5*((y[i]-mu[i])/sigma)^2));
  for ( i in 1:200 ) 
    if ( y[i] != 0.5 ) target += log((1-p[i])*(1/(sqrt(2*pi())*sigma))*exp(-0.5*((y[i]-mu[i])/sigma)^2));
}"

m3<-stan(model_code=m3code,data=d)
m4<-stan(model_code=m4code,data=d)

precis(m2)
precis(m3)
precis(m4)

#Values of these parameters are estimated rather well
a
b
sigma

#Unlike these, which are given a very low weight (they seem unimportant since the probability of 0.5 grows with x as the probability of y=0.5 given the normal distribution)
ap
bp

pairs(m2)

#We can compensate for this a bit claiming that value 0.5 cannot arise in any other way than the binomial way, i.e. we can send probability density of the normal distribution at 0.5 to 0


mB <- ulam(
  alist(
    y|y!=0.5 ~ custom( log1m(p) + normal_lpdf(y|mu,sigma) ),
    y|y==0.5 ~ custom( log( p ) ),
    
    mu<-a+b*x,
    a~dnorm(0,0.5),
    b~dnorm(0,1),
    sigma~dexp(1),
    
    logit(p) <- ap+bp*x,
    ap ~ dnorm(-1.5,1),
    bp ~ dnorm(0,1)
    
  ) , data=d )

precis(mB)

#Super! This works!
a
b
sigma

ap
bp


#Check the stancode
stancode(mB)

#The rough manual syntax would look like this:

mB4code<-"
data{
vector[200] y;
vector[200] x;
}
parameters{
  real a;
  real b;
  real<lower=0> sigma;
  real ap;
  real bp;
}
model{
  vector[200] mu;
  vector[200] p;
  bp ~ normal( 0 , 1 );
  ap ~ normal( -1.5 , 1 );
  for ( i in 1:200 ) {
    p[i] = ap + bp * x[i];
    p[i] = inv_logit(p[i]);
  }
  sigma ~ exponential( 1 );
  b ~ normal( 0 , 1 );
  a ~ normal( 0 , 0.5 );
  for ( i in 1:200 ) {
    mu[i] = a + b * x[i];
  }
  for ( i in 1:200 ) 
    if ( y[i] == 0.5 ) target += log( p[i] );
  for ( i in 1:200 ) 
    if ( y[i] != 0.5 ) target += log((1-p[i])*(1/(sqrt(2*pi())*sigma))*exp(-0.5*((y[i]-mu[i])/sigma)^2));
}"

mB4<-stan(model_code=mB4code,data=d)

#Same here, perfect
precis(mB4)


