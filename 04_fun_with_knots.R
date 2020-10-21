library(splines)

time<-1:2000

#First define a number of splines
num_knots <- 5
#Then ask R for appropriate positions of these splines (you can also define splines manually)
knot_list <- quantile( time , probs=seq(0,1,length.out=num_knots) )

#The bs function is a bit tricky. It will always supplement 100% knot and also (if intercept=F) omits the first knot. That is because is was originally used to model growth (from 0 to some maximum).

#If you want to approximate a general relationship, omit these two knots: see the "[-c(1,num_knots)]" part and set intercept to T. This is what you want. 

#The degree of the piecewise polynomial tell you, how many curves meet at a knot. On any other point degree+1 curves are usually relevant (with the exception of the "first" and "last" knot, which are not knots (strictly speaking). They just compensate for the "lack of curves" and only one curve determines them)
B <- bs(time,
        knots=knot_list[-c(1,num_knots)] ,
        degree=4, intercept=T )

#Plot splines
plot(time,B[,1],type="n",ylim=c(0,1))
for(i in 1:ncol(B)){
  lines( time , B[,i] ,col=i+1)
}

#Cut the plot at points of your original "knot list"
abline(v=knot_list)

