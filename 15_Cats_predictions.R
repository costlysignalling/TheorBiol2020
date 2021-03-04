# Simualte the data
# Note, that the original text uses alpha + beta*cat to generate note counts, but we use exp(alpha + beta*cat), because that is what the statistical model assumes (this allows us to check whether the extracted coefficients are correct)
set.seed(9)
N_houses <- 100L
alpha <- 5
beta <- (-3)
k <- 0.5
r <- 0.2
cat <- rbern( N_houses , k )
notes <- rpois( N_houses , exp(alpha + beta*cat) )
R_C <- rbern( N_houses , r )
cat_obs <- cat
cat_obs[R_C==1] <- (-9L)
dat <- list(
  notes = notes,
  cat = cat_obs,
  RC = R_C,
  N = as.integer(N_houses) )

#check teh data
dat

#Evaluate the model
m15.9 <- ulam(
  alist(
    # singing bird model
    notes|RC==0 ~ poisson( lambda ),
    notes|RC==1 ~ custom( log_sum_exp(
      log(k) + poisson_lpmf( notes | exp(a + b) ),
      log(1-k) + poisson_lpmf( notes | exp(a) )
    ) ),
    log(lambda) <- a + b*cat,
    a ~ normal(0,1),
    b ~ normal(0,0.5),
    
    # sneaking cat model
    cat|RC==0 ~ bernoulli(k),
    k ~ beta(2,2),
    
    # imputed values
    gq> vector[N]:PrC1 <- exp(lpC1)/(exp(lpC1)+exp(lpC0)),
    gq> vector[N]:lpC1 <- log(k) + poisson_lpmf( notes[i] | exp(a+b) ),
    gq> vector[N]:lpC0 <- log(1-k) + poisson_lpmf( notes[i] | exp(a) )
  ), data=dat , chains=4 , cores=4 )

#Get the precis output
tab<-precis(m15.9,depth=2)

#Take only the lines with cat predictions parameters
resdat<-as.data.frame(tab@.Data)
names(resdat)<-tab@names
take<-substr(tab@row.names,1,4)=="PrC1"
pr<-resdat[take,]
rownames(pr)<-tab@row.names[take]

#Combine the estiates with the original "cat" discrete variable
results<-cbind(round(pr,4),dat$RC,dat$notes,cat)

#Show only the lines where imputation was necessary
results[dat$RC==1,]

#The model is able to predict, where there is a cat

#Try it with the original generatiove process as well

set.seed(9)
N_houses <- 100L
alpha <- 5
beta <- (-3)
k <- 0.5
r <- 0.2
cat <- rbern( N_houses , k )
notes <- rpois( N_houses , alpha + beta*cat )
R_C <- rbern( N_houses , r )
cat_obs <- cat
cat_obs[R_C==1] <- (-9L)
dat <- list(
  notes = notes,
  cat = cat_obs,
  RC = R_C,
  N = as.integer(N_houses) )

#check teh data
dat

#Evaluate the model
m15.9 <- ulam(
  alist(
    # singing bird model
    notes|RC==0 ~ poisson( lambda ),
    notes|RC==1 ~ custom( log_sum_exp(
      log(k) + poisson_lpmf( notes | exp(a + b) ),
      log(1-k) + poisson_lpmf( notes | exp(a) )
    ) ),
    log(lambda) <- a + b*cat,
    a ~ normal(0,1),
    b ~ normal(0,0.5),
    
    # sneaking cat model
    cat|RC==0 ~ bernoulli(k),
    k ~ beta(2,2),
    
    # imputed values
    gq> vector[N]:PrC1 <- exp(lpC1)/(exp(lpC1)+exp(lpC0)),
    gq> vector[N]:lpC1 <- log(k) + poisson_lpmf( notes[i] | exp(a+b) ),
    gq> vector[N]:lpC0 <- log(1-k) + poisson_lpmf( notes[i] | exp(a) )
  ), data=dat , chains=4 , cores=4 )

#Get the precis output
tab<-precis(m15.9,depth=2)

#Take only the lines with cat predictions parameters
resdat<-as.data.frame(tab@.Data)
names(resdat)<-tab@names
take<-substr(tab@row.names,1,4)=="PrC1"
pr<-resdat[take,]
rownames(pr)<-tab@row.names[take]

#Combine the estiates with the original "cat" discrete variable
results<-cbind(round(pr,4),dat$RC,dat$notes,cat)

#Show only the lines where imputation was necessary
results[dat$RC==1,]

#This works as well

exp(precis(m15.9)[1,1])
exp(precis(m15.9)[1,1]+precis(m15.9)[2,1])
