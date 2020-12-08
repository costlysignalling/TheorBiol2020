library(rethinking)

#Corridor of stability example

#Create the dataset, where you know the correct answer
h<-rnorm(10000,0,1)
w<-h*0.2+rnorm(10000,0,0.6)

#Thei function gives you the sequence of beta estimations with random gradually growing sample

#Set the ns, you are willing to consider
ns<-5:200

#Define the function that will start at 5, estimate the beta (we know the correct answer, because we created the original dataset), add another participant, estimate the beta and iterate until n=max 
givebet<-function(){
  
  sam<-sample(1:10000)
  ds<-list(h=h[sam],w=w[sam])
  
  betas<-NA

  counter<-1
  
  for(n in ns){
    
    dsub<-list(h=ds$h[1:n],w=ds$w[1:n])
    
    m<-quap(alist(
      w~dnorm(mu,sigma),
      mu<-a+b*h,
      
      a~dnorm(0,0.2),
      b~dnorm(0,0.5),
      
      sigma~dexp(1)
      
    ),data=dsub,start=list(a=0,b=0,sigma=1))
    
    betas[counter]<-precis(m)[[1]][2]
    
    print(n)
    
    counter<-counter+1
  }
  
  return(betas)
}

#You need to create multiple curves like this. The more, the better (but it takes more computation time)
runs<-20

curves<-replicate(runs,givebet())
str(curves)

#Set the COS width, where you wish to land 
wd<-0.10

plot(ns,curves[,1],type="n",ylim=c(-0.2,0.5))
for(i in 1:runs){
  lines(ns,curves[,i],col="#00000080")
}

#You can higlight single simulation run like this
lines(ns,curves[,1],col="#0000FF")

#Plot the correct estimate and the corridor of stability
abline(h=0.2,col=2)
abline(h=0.2+c(-1,1)*wd,col=2,lty=2)

#The stability is usually defined as staynw within the corridor
#First you need to define, if iven point along each curve is within the corridor
between<-curves>0.2-wd&curves<0.2+wd
str(between)

#Then you need to check whether the given point is not just within the corridor, but also that given curve does not fluctuate outside the COS until the end of the simulation. This is achieved by comparison of the cummulative sum of the reversed T/F vector of being within the corridor with the vector corresponding to the order of the used semple size

#This is the procedure for the fourth curve
i<-4
rev(cumsum(rev(between[,i]))==c(1:length(ns)))

#This is the procedure for all of them (matrix is returned)
ins<-sapply(1:20,function(i){rev(cumsum(rev(between[,i]))==c(1:poc))})

#Another arbitrary parameter you need to define (besides COS width, wd in this script) is the Proportion of curves you want to securely keep within the COS until the end of the simulation (until maximum n is reached). 80% was selected here
security<-0.80

#Cacluate the proportion of your simulation runs, where the estimate stays within the corridor until the end of the simulation
proport<-rowSums(ins)/runs

#Result is the threshold. The first point, where the requested proportion of estimates stay within the requested corridor.
tt<-ns[min(which((proport>=security)==T))]

tt

#You can put it as a vertical line to the plot.
abline(v=tt,col=3)






