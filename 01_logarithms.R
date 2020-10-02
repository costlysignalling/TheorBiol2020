#Useful bits from the last summer semester

#See 
#Combinatorial
?choose

choose(3,2)

choose(5,2)
choose(5,3)

choose(100,45)


##Power of logarithm

#Logarithm is defined and a function inverse to exponencial function
log(exp(5))
exp(log(3))
#Which is used in probability calulations on stupid discrete-states computers :)
#But also in data visualizations and handling. It is important to understand logarithms if zou want to practice anything science-like.
#Logartigms transform clumsy multiplicative processes to easily handeled additive process.
#Logarithms make big numbers (>1) smaller and small numbers (<1) negative.
#When our ancestrors needed to multiply 3 large numbers
N<-c(225698,556877988,3546899)
#They took their logarithms
(logN<-log(N))
#Added the logarithms together
(sumN<-sum(logN))
#And then possibly raised e (natural logarithm base) to the resulting sum
exp(sumN)

#Now we have computers
prod(N)

#But logaritms are still verz useful, let me demonstrate:

#First I will define a neat plotting function that outputs AIC, Rsquared of a linear model visualized alongside estimated scatterplot smoothing. I find this plot useful in data exploration.
myplot<-function(x,y,y2=NULL,rounding=2,mycol="#2288FF",mycol2="#FF2255",span=NA){
  namx<-deparse(substitute(x))
  namy<-deparse(substitute(y))
  
  par(mar=c(4,4,4,1),mgp=c(2.2,1,0))
  plot(x,y,col=mycol,pch=16,xlab=namx,ylab=namy)
  
  if(is.na(span)){
    span<-(max(x,na.rm=T)-min(x,na.rm=T))/10
  }
  
  l <- loess(y ~ x, span=span)
  newx<-seq(min(x,na.rm=T),max(x,na.rm=T),l=1000)
  newy<-predict(l,newdata=data.frame(x=newx))
  lines(newx,newy,col=mycol,lwd=2)
  
  model<-lm(y~x)
  abline(model,lwd=2,lty=2)
  
  A<-model$coefficients[[1]]
  B<-model$coefficients[[2]]
  
  inform<-AIC(model)
  rsq<-summary(model)$r.squared
  
  title (paste(namy," = ",round(A,rounding)," + ",round(B,rounding),namx,"\nAIC = ",round(inform,rounding),", R2= ",round(rsq,rounding),sep=""),adj=0,font.main=1)
  
  if(!is.null(y2)){
    lines(x,y2,col=mycol2,lwd=2)
    points(x,y2,col=mycol2,lwd=2)
    legend("topleft",pch=1,lwd=2,col=mycol2,legend="change")
  }
}


#Here I start by uploading some data
d<-read.table("gdppopdata.txt" ,sep="\t",header=T)

#See what we got here
#On natural scales - GDP and popluation size, the data are messy and horribly nonlinear
myplot(d$population,d$totalGDP)
text(d$population,d$totalGDP,d$abr,pos=3,xpd=T)

#But that is because the process generating population size and GDP is not additive (random contributions are not +10 or -10 inhabitants, or +100 or -100 $), but rather multiplicative. Random changes in both are ×1.1 or ×0.91 (10% growth or 9% decline).
myplot(log(d$population),log(d$totalGDP),span=0.5)
text(log(d$population),log(d$totalGDP),d$abr,pos=3,xpd=T)

#If you investigate the description of the log function, you discover that you can modify the logarithm base. That can be really useful, but usually people do not think too much about it and just use natural logarithm or base 10.
?log

myplot(log(d$population,10),log(d$totalGDP,10),span=0.5)

#GDP is in millions, population v thousands
#lets do
myplot(log(d$population*1000,10),log(d$totalGDP*1000000,10),span=0.5)
points(log(d$population*1000,10)[d$abr=="CZE"],log(d$totalGDP*1000000,10)[d$abr=="CZE"],col=2,pch=16)
abline(v=log(d$population*1000,10)[d$abr=="CZE"],col=2,lty=2)
abline(h=log(d$totalGDP*1000000,10)[d$abr=="CZE"],col=2,lty=2)
#Czech republic is the red point

#We can have a better solution. All logarithmic scales get rid of the original units equally well. They do not scale the data differently. But they differ in the new units, that correspond to actions of multiplication by given base. Nice unit (1 on the x and y axes) can be, for example, multiplication by 1.1 (respective division by 1.1), i.e. 10% increase or 1-(1/1.1)% decrease.
better<-d
better$population<-log(d$population,1.1)
better$totalGDP<-log(d$totalGDP,1.1)

#If we plot it and find Czech republic on the graph
myplot(better$population,better$totalGDP,span=0.5)
points(better$population[better$abr=="CZE"],better$totalGDP[better$abr=="CZE"],col=2,pch=16)
abline(v=better$population[better$abr=="CZE"],col=2,lty=2)
abline(h=better$totalGDP[better$abr=="CZE"],col=2,lty=2)

#We can easily talk about differences between countries on thi scale
(cze<-better$totalGDP[better$abr=="CZE"])
(usa<-better$totalGDP[better$abr=="USA"])
#The numbers seem rather similar

(dif<-usa-cze)
#The difference between Czechia and USA is 41. That means to get USA GDP, we would have to multiply our current GDP by 1.1 41 times (that means increase our GDP by 10% than increase the result by 10% again and so on and so on...)

#Small "power" excersise
1.1*1.1
1.1*1.1*1.1
1.1^3
1.1^dif
#1.1 to the difference between Czech and USA GDP is 51.29 which means that USA has 51times higher GDP than Czech republic. All it takes is 41 increases by 10%

#We can deonstrate that the numbers fit
#Set the multiplicator
multip<-1.1^dif

d$totalGDP[d$abr=="CZE"]

#See, the numbers are the same
d$totalGDP[d$abr=="CZE"]*multip
d$totalGDP[d$abr=="USA"]

#What is the best visualization of data that illustrate a multiplicative processes and relationships between them?
#In my opinion it the best to set some threshold as 0 and reference the units of muliplication towards this threshold.
#All it takes is to substract the threhold value from all logartigm scores
better$population<-better$population-better$population[better$abr=="CZE"]
better$totalGDP<-better$totalGDP-better$totalGDP[better$abr=="CZE"]

#Now Czech republic is at 0 and all other scores represent ×1.1 (postitive) or /1.1 (negative) changes in GDP or population size 
myplot(better$population,better$totalGDP,span=0.5)
points(better$population[better$abr=="CZE"],better$totalGDP[better$abr=="CZE"],col=2,pch=16)
abline(v=better$population[better$abr=="CZE"],col=2,lty=2)
abline(h=better$totalGDP[better$abr=="CZE"],col=2,lty=2)
text(better$population,better$totalGDP,better$abr,pos=3,xpd=T,cex=0.8)

#Pay attention to AIC. AIC depends on the units of the predicted variable (here on log(GDP)) take this into account in model comparison! You can always compare only models with the IDENTICAL output data! 
myplot(better$population,better$totalGDP,span=0.5)
myplot(log(d$population,10),log(d$totalGDP,10),span=0.5)
myplot(log(d$population,1.1),log(d$totalGDP,1.1),span=0.5)
myplot(log(d$population,1.1),log(d$totalGDP,10),span=0.5)


#This comes useful in probability calculations, because computers are stupid and they do not represent small numbers very well - look:
A<-rep(c(0.001,0.0005,0.0003),100)
B<-rep(c(0.001,0.0005,0.0003),50)

#We have two vectors of small probabilities
A
B

#Taking their producs returns simply 0
prod(A)
prod(B)


#But if we log them
(logA<-log(A,10))
(logB<-log(B,10))
#And sum the logs
(sumA<-sum(log(A,10)))
(sumB<-sum(log(B,10)))

#We can clearly see, that the resulting probability of B product is much higer than resulting probability of A.
#Logarithm scales the positive part of the real number axis such that all numbers between 0 and 1 (most numbers in probability calculations) have the all negative part of the real axis for themselves, which is really useful if we represent continuous numbers on a discrete machine like our computer.

