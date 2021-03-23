#First, we generate some data
#An exemplar scenario is the relationship between sex, cortisol and donation in Dictator game

#Women give more than men (out of 400)
aF<-200
aM<-100

#With higher cortisol, women give less, but men give more (btw, this is just an example, the real data woul have a slighly different costitution - e.g. donations would be discrete and inflated around the exact half: 200)
bF<--10
bM<-8

#There is some variation that cannot be explained by cortisol
sigma<-10

#Sample size of each sex
N<-10

set.seed(800)

#Generate some random cortisol levels
cort<-rnorm(N*2,4,1.5)
sex<-rep(c(1,2),each=N)

#Calculate the donations from the model and add the extra error variance
dona<-rep(c(aF,aM),each=N)+rep(c(bF,bM),each=N)*cort+rnorm(2*N,0,sigma)

#These are my plotting colors
sexcol<-c("#FF6000","#0060FF")

#This is my dataset
d<-list(
  cort=cort,
  sex=sex,
  dona=dona)

list(
c(1,3,5,4),5,"prd",c("a","b"))

plot(d$cort,d$dona,col=sexcol[d$sex],pch=16,ylim=c(0,400))

for(i in 1:100){
abline(a=rnorm(1,200,50),b=rnorm(1,0,40))
}

library(rethinking)

#Takto si zmerime, jak dlouho mu to trva
start<-Sys.time()


alist(
  dona~dnorm(mu,sigma),
  mu<-a[sex]+b[sex]*cort,
  a[sex]~dnorm(200,50),
  b[sex]~dnorm(0,40),
  sigma~dexp(0.25)
)

library(rethinking)

?ulam


A<-rexp(1000,0.05)
plot(density(A))

mean(A)

#Chytry HMC sampler, nas vzor
m1<-ulam(alist(
  dona~dnorm(mu,sigma),
  mu<-a[sex]+b[sex]*cort,
  a[sex]~dnorm(200,50),
  b[sex]~dnorm(0,40),
  sigma~dexp(0.05)
),data=d,cores=4,chains=4)


c(200,100)[2]

Sys.time()-start

precis(m1,depth=2)

samples1<-extract.samples(m1)

plot(density(samples1$sigma))

prior1<-extract.prior(m1)

str(prior1)

str(samples1)

#I can plot the posterior samples in many ways, this is a root of most visualizations
plot(density(samples1$a[,1]),xlim=c(0,400),col=sexcol[1])
dens2<-density(samples1$a[,2])
lines(dens2$x,dens2$y,col=sexcol[2])
dens3<-density(prior1$a[,2])
lines(dens3$x,dens3$y,col=1)



plot(density(samples1$b[,1]),xlim=c(-30,30),col=sexcol[1])
dens2<-density(samples1$b[,2])
lines(dens2$x,dens2$y,col=sexcol[2])



#Nejhloupejsi sampler: grid - nespojitá množina hypotéz
dent<-20

start<-Sys.time()

aFopts<-seq(0,400,dent)
aMopts<-seq(0,400,dent)

bFopts<-seq(-200,200,dent)
bMopts<-seq(-200,200,dent)

sigmaopts<-seq(0,200,dent)

hyps<-expand.grid(aF=aFopts,
                  aM=aMopts,
                  bF=bFopts,
                  bM=bMopts,
                  sigma=sigmaopts)

nrow(hyps)

(ps<-hyps[1130,])
a<-c(ps$aF,ps$aM)
b<-c(ps$bF,ps$bM)

pred<-a[d$sex]+d$cort*b[d$sex]
probs<-dnorm(d$dona,mean=pred,sd=ps$sigma)

#This is the data likelihood if hypothesis 1130 is true
prod(probs)


#We can wrap it up in a function
likelihood<-function(i){
  ps<-hyps[i,]
  a<-c(ps$aF,ps$aM)
  b<-c(ps$bF,ps$bM)
  
  pred<-a[d$sex]+d$cort*b[d$sex]
  probs<-dnorm(d$dona,mean=pred,sd=ps$sigma)
  prod(probs)
}

#And calulate the probability of data if hypothesis p(d|H) for each hypothesis
hyps$data.likelihood<-sapply(1:nrow(hyps),likelihood)

#Ted musime vyresit priory:
#a[sex]~dnorm(200,50)
#b[sex]~dnorm(0,40)
#sigma~dexp(0.25)

#A ted resime likelihood kombinaci parametru (hypotez) za predpokladu distribuce pravdepodobnosti danou priory
names(hyps)

#Takto spocitame pro kazdou hypotezu jeji prior pravdepodobnost p(H) 
hyps$prior.prob<-
  dnorm(hyps$aF,mean=200,sd=50)*
  dnorm(hyps$aM,mean=200,sd=50)*
  dnorm(hyps$bF,mean=0,sd=40)*
  dnorm(hyps$bM,mean=0,sd=40)*
  dexp(hyps$sigma,rate=0.05)

head(hyps)

#The product of the two is the numerator in the bayes theorem: p(d|H)p(H)
hyps$posterior.numerator<-hyps$data.likelihood*hyps$prior.prob

sum(hyps$posterior.prob)

#Divide the numerator by the total probability of the data p(d): a sum of all probabilities of the hypotehses set
hyps$posterior.prob<-hyps$posterior.numerator/sum(hyps$posterior.numerator)

#I sample those discrete hypotheses
take.hyps<-sample(1:nrow(hyps),size=2000,prob=hyps$posterior.prob,replace=T)
summary(as.factor(take.hyps))

#And extract the samples based on the take.hyps vector
samples2<-hyps[take.hyps,1:5]

Sys.time()-start

#Compare the two set of samples
#The clever method generated this
plot(density(samples1$a[,1]),xlim=c(0,400),col=sexcol[1])
dens2<-density(samples1$a[,2])
lines(dens2$x,dens2$y,col=sexcol[2])

#The stupid method this
dens1<-density(samples2$aF)
lines(dens1$x,dens1$y,col=sexcol[1],lty=2)

dens2<-density(samples2$aM)
lines(dens2$x,dens2$y,col=sexcol[2],lty=2)

#But the mean estimates are quite close... And the granularity was super stupid!
mean(samples1$a[,1])
mean(samples2$aF)

mean(samples1$a[,2])
mean(samples2$aM)


#We can do the same for slopes
plot(density(samples1$b[,1]),xlim=c(-30,30),col=sexcol[1])
dens2<-density(samples1$b[,2])
lines(dens2$x,dens2$y,col=sexcol[2])

#The stupid method this
dens1<-density(samples2$bF)
lines(dens1$x,dens1$y,col=sexcol[1],lty=2)

dens2<-density(samples2$bM)
lines(dens2$x,dens2$y,col=sexcol[2],lty=2)

#This is no so good, we have to change the granularity
mean(samples1$b[,1])
mean(samples2$bF)

mean(samples1$b[,2])
mean(samples2$bM)

#Return above, change the parameter "dent" and try again (btw dent=10 reqires you do calculate data likelihood for 59 340 981 hypotheses which is disgustingly many..., consider higher dent, e.g. dent=20 or 15) 


#Lowering the granularity would be super costly... So, cen we do better?
#Yes! Behold:
#The best "stupid sampler" in the world

#The trick is in random sampling of the hypotheses
excess<-1000
N<-2000

#If you require 2000 samples, you draw 2000*excess samples first and work with them as above, than you draw random samples based exclusively on the data likelihood (you do not further use priors, because priors determined wheteher the hypothesis will be even censidered!)

start<-Sys.time()

aF<-rnorm(N*excess,200,50)
aM<-rnorm(N*excess,200,50)

bF<-rnorm(N*excess,0,40)
bM<-rnorm(N*excess,0,40)

sigma<-rexp(N*excess,0.25)

hyps<-data.frame(aF=aF,
                  aM=aM,
                  bF=bF,
                  bM=bM,
                  sigma=sigma)

nrow(hyps)

#We can use the same likelihood function (see above)
hyps$data.likelihood<-sapply(1:nrow(hyps),likelihood)

#We do not need to deal with priors, becuase we sampled the hypotheses based on priors
hyps$posterior.prob<-hyps$data.likelihood/sum(hyps$data.likelihood)
sum(hyps$posterior.prob)

#I sample those discrete hypotheses
take.hyps<-sample(1:nrow(hyps),size=N,prob=hyps$posterior.prob,replace=T)

#Suddenly there is much more variation in hypotheses that got sampled (still, due to the low excess, there are repetitions)
summary(as.factor(take.hyps))

samples3<-hyps[take.hyps,1:5]

Sys.time()-start

#The clever method generated this
plot(density(samples1$a[,1]),xlim=c(0,400),col=sexcol[1])
dens2<-density(samples1$a[,2])
lines(dens2$x,dens2$y,col=sexcol[2])

#The stupid method this
dens1<-density(samples3$aF)
lines(dens1$x,dens1$y,col=sexcol[1],lty=2)

dens2<-density(samples3$aM)
lines(dens2$x,dens2$y,col=sexcol[2],lty=2)

mean(samples1$a[,1])
mean(samples3$aF)

mean(samples1$a[,2])
mean(samples3$aM)


#We can do the same for slopes
plot(density(samples1$b[,1]),xlim=c(-30,30),col=sexcol[1])
dens2<-density(samples1$b[,2])
lines(dens2$x,dens2$y,col=sexcol[2])

#The stupid method this
dens1<-density(samples3$bF)
lines(dens1$x,dens1$y,col=sexcol[1],lty=2)

dens2<-density(samples3$bM)
lines(dens2$x,dens2$y,col=sexcol[2],lty=2)

#With excess about 1000 (2000000 hypothesis considered in total), the sampled distribution is very similar to the result of HMC

mean(samples1$b[,1])
mean(samples3$bF)

mean(samples1$b[,2])
mean(samples3$bM)

#Compatibility intervals are similar
PI(samples1$a[,1],prob=0.90)

#This is how you would calculate the equivalient of rethinking's PI function using quantile
CI<-0.90
pr<-c((1-CI)/2,1-(1-CI)/2)
pr

quantile(samples3$aF,probs=pr)
PI(samples3$aF,prob=0.90)


PI(samples1$a[,2],prob=0.90)
PI(samples3$aM,prob=0.90)

PI(samples1$b[,1],prob=0.90)
PI(samples3$bF,prob=0.90)

PI(samples1$b[,2],prob=0.90)
PI(samples3$bM,prob=0.90)

#Perfect concordance... With much faster computers we could afford to be super stupid, but with many parameters comes the necesity to sample more cleverly 

plot(d$cort,d$dona,col=sexcol[d$sex],pch=16,ylim=c(50,250))
str(samples3)

for (i in 1:2000){
ps<-samples3[i,]
abline(a=ps$aF,b=ps$bF,col=sexcol[1])
abline(a=ps$aM,b=ps$bM,col=sexcol[2])
}




