library(rethinking)

#Set the seed to get the same set of "random" data 
set.seed(71)
# number of plants
N <- 100

# simulate initial heights
h0 <- rnorm(N,10,2)

# assign treatments and simulate fungus and growth
treatment <- rep( 0:1 , each=N/2 )
fungus <- rbinom( N , size=1 , prob=0.5 - treatment*0.4 )
h1 <- h0 + rnorm(N, 5 - 3*fungus)

# compose a clean data frame
d <- data.frame( h0=h0 , h1=h1 , treatment=treatment , fungus=fungus )
precis(d)


mPATH <- quap(
  alist(
    h1 ~ dnorm( mu , sigma ),
    mu <- h0 * p,
    p <- a + bt*treatment + bf*fungus,
    a ~ dlnorm( 0 , 0.2 ) ,
    bt ~ dnorm( 0 , 0.5 ),
    bf ~ dnorm( 0 , 0.5 ),
    sigma ~ dexp( 1 ),
    
    fungus~dbinom(1,pf),
    logit(pf)<-af+btf*treatment,
    af~dnorm(0,10),
    btf~dnorm(0,10)),
  data=d )

precis(mPATH)

#reconstruct the probability of developing fungus under treatment or without it with inverse logit link function
#No treatment
inv_logit(-0.49)
#Treatement
inv_logit(-0.49-1.95)

#See? These inferred probabilites are very close to simple descriptive statistics. But the advanatage is, taht you can quantify the uncertainty around these estimates and link it to other terms of the linear model. 
table(fungus[treatment==0])/length(fungus[treatment==0]) #Fungus if no treatment
table(fungus[treatment==1])/length(fungus[treatment==1]) #Fungus if treatment
