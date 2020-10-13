#Problems with too many (or too few) hypotheses
#It is never guaranteed that you include the "right hypothesis" into the set of considered hypotheses in a Bayesian inference, not it is gurateed that the algorithm that generated the data (i.e. the right hypothesis) will come out as the best fitting model, especially if you do not have enough data.

#The best you can do is to always conduct prior predictive simulation, see, what you can get out of the inference, when you are certain which process generated the data.

#And also it is useful to think about statistical models (especially the Bayesian ones) as predictive rather than truth revealing machines.

#If zou share zour data and justifz zour methods, zou have nothing to worry about.

library(rethinking)


#Assume that fondness of cats depends on the position along introversion-extraversion continuum
ext0<-seq(-2,2,0.01)

#Lets also assume, that the dependence is a complex one. All terms up to the fifth degree plaz a certain role.
betas<-c(-0.8,0.4,-0.5,-0.2,0.12)

cat.love01<-ext0*(-0.8)
cat.love02<-ext0*(-0.8)+(ext0^2)*0.4
cat.love03<-ext0*(-0.8)+(ext0^2)*0.4+(ext0^3)*(-0.5)
cat.love04<-ext0*(-0.8)+(ext0^2)*0.4+(ext0^3)*(-0.5)+(ext0^4)*(-0.2)
cat.love05<-ext0*(-0.8)+(ext0^2)*0.4+(ext0^3)*(-0.5)+(ext0^4)*(-0.2)+(ext0^5)*0.12

#Lets see those curves that describe the "actual relationship" between love for cats and extraversion
plot(ext0,cat.love01)
plot(ext0,cat.love02)
plot(ext0,cat.love03)
plot(ext0,cat.love04)
plot(ext0,cat.love05)


#But obviously many other factors contributo to our feelings for cats, so let's assume the measured cat love is actually a random distribution around the mean value posed by the model.
sd<-3

#Set a fixed random seed so everyone generates the same set of random numbers
#Also we do not have 400 regularly spaces points along the extravesrion. We assume we sample 100 participants at random. This is a realistic dataset.
set.seed(999)
ext<-rnorm(100,0,1) 
l<-length(ext)
cat.love1<-ext*(-0.8)+rnorm(l,0,sd)
cat.love2<-ext*(-0.8)+(ext^2)*0.4+rnorm(l,0,sd)
cat.love3<-ext*(-0.8)+(ext^2)*0.4+(ext^3)*(-0.5)+rnorm(l,0,sd)
cat.love4<-ext*(-0.8)+(ext^2)*0.4+(ext^3)*(-0.5)+(ext^4)*(-0.2)+rnorm(l,0,sd)
cat.love5<-ext*(-0.8)+(ext^2)*0.4+(ext^3)*(-0.5)+(ext^4)*(-0.2)+(ext^5)*0.12+rnorm(l,0,sd)

#See how the plots have changed
plot(ext,cat.love1)
plot(ext,cat.love2)
plot(ext,cat.love3)
plot(ext,cat.love4)
plot(ext,cat.love5)

#Order the data along the extraversion axis in the final dataset, this is useful when we use splines later in the script.
ord<-order(ext)

d <- list(
  ext = ext[ord],
  cat_love = cat.love5[ord]
)

#And now evaluate 5 Bayesian models with decreasing polynomial. Remember that prior for model 5 includes in a prior set of considered hzpotheses also all hypothesis described as polynomials of lower order (when b5=0 etc.)!
m5 <- quap(
  alist(
    cat_love ~ dnorm( mu , sigma ),
    mu <- a+b1*ext+b2*ext^2+b3*ext^3+b4*ext^4+b5*ext^5,
    a ~ dnorm(0,1),
    
    b1 ~ dnorm(0,1),
    b2 ~ dnorm(0,1),
    b3 ~ dnorm(0,1),
    b4 ~ dnorm(0,1),
    b5 ~ dnorm(0,1),
    
    sigma ~ dexp(1)
  ) , data=d)


m4 <- quap(
  alist(
    cat_love ~ dnorm( mu , sigma ),
    mu <- a+b1*ext+b2*ext^2+b3*ext^3+b4*ext^4,
    a ~ dnorm(0,1),
    
    b1 ~ dnorm(0,1),
    b2 ~ dnorm(0,1),
    b3 ~ dnorm(0,1),
    b4 ~ dnorm(0,1),

    sigma ~ dexp(1)
  ) , data=d)


m3 <- quap(
  alist(
    cat_love ~ dnorm( mu , sigma ),
    mu <- a+b1*ext+b2*ext^2+b3*ext^3,
    a ~ dnorm(0,1),
    
    b1 ~ dnorm(0,1),
    b2 ~ dnorm(0,1),
    b3 ~ dnorm(0,1),

    sigma ~ dexp(1)
  ) , data=d)

m2 <- quap(
  alist(
    cat_love ~ dnorm( mu , sigma ),
    mu <- a+b1*ext+b2*ext^2,
    a ~ dnorm(0,1),
    
    b1 ~ dnorm(0,1),
    b2 ~ dnorm(0,1),

    sigma ~ dexp(1)
  ) , data=d)

m1 <- quap(
  alist(
    cat_love ~ dnorm( mu , sigma ),
    mu <- a+b1*ext,
    a ~ dnorm(0,1),
    
    b1 ~ dnorm(0,1),

    sigma ~ dexp(1)
  ) , data=d)

m0 <- quap(
  alist(
    cat_love ~ dnorm( mu , sigma ),
    mu <- a,
    a ~ dnorm(0,1),
    sigma ~ dexp(1)
  ) , data=d)


#The most complex model is not entirely off! Considering we have only 100 data points, this is good news.
precis(m5)
betas

#But if we compare the models on the basis of information criteria (WAIC - how much predictive accuracy we get for each bit of information invested - i.e. for considering complex higher order polynomial models etc), we see that all models actually outperform the "true model" m5 (but i have selected such seed on poupose, try to fiddle with that line of code, change the seed. You will find out, that there are many seeds for which the m5 fits the data very well, if not best). That is becuase there is a lot of noise in the data (sd=3) that is mixed with the information. Using polynomila of the fifth order, we risk overfitting, even if fifth order polynomial was actually used to generate the data. There is just too much hypotheses in the set of the fifth order polynomilal models. It is better to "play it safe" and use only a linear regression (i.e. only the set of hypotheses that include hypothese along the continuum "love for cats grows with extraversion"-"love for cats decreases with extraversion")
compare(m5,m4,m3,m2,m1,m0)


#Lets visualize the data together with the prediction (the parimonious line + 89% compatibility corridor of predicted lines that summarizes the posterior.)
mu <- link( m5 , data=data.frame(ext=ext0) )
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI , prob=0.89 )

plot(ext,cat.love5,pch=16,col=rangi2)
lines( ext0 , mu.mean )
shade( mu.PI , ext0 )
lines( ext0 , cat.love05,col=2 ,lwd=2)
#Pretty close, right? The "correct model" is included in the 89% CI.


mu <- link( m1 , data=data.frame(ext=ext0) )
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI , prob=0.89 )

plot(ext,cat.love5,pch=16,col=rangi2)
lines( ext0 , mu.mean )
shade( mu.PI , ext0 )
lines( ext0 , cat.love05,col=2 ,lwd=2)

#But the linear model predicts the data just as well wth less parameters.
#By the way it is not OK to use "step" and "drop" model simplifications. There is a huge dabate about this, but in Bayesian inference, you do not care about "significances" but aboul "plausble hypotheses distributions". Inspect the model summary again.
precis(m5)
precis(m1)
#you can see thah overall love for cats decreases with extraversion, but the actual shape of the curve is obscure. In the big model, all parameters can be both positive and negative and still fit within the 89% CI. But that is just fine. You do not reveal the "truth", you predict future data based on the current data. You can change the N of the data above - generate 1000 participants instead of 100 and see what happens. This is useful to do before you go into the field and collect the data. Consider, how complex hypotheses you are willing to consider based on the N you are willing to collect.
#(Also notice, that all models include the intercept a, this was implicit in our model and equal to 0)


#There are alterative approaches to polynomial regression.
#One of them eploys splines (functions that are close to 0 along portions of the x axis, but have a positive value elsewhere, this value that peaks around a "knot" for each spline). Functions (here we use cubic splines, but you can use whatever), That are multiplied and than added together. See chapter 4.5 in Statistical rethinking for detail and a different example.
#To have a model with 5 parameters, we use 
num_knots <- 4
knot_list <- quantile( d$ext , probs=seq(0,1,length.out=num_knots) )

library(splines)
B <- bs(d$ext,
        knots=knot_list[-c(1,num_knots)] ,
        degree=2 , intercept=T )
str(B)

## R code 4.75
plot(ext,cat.love5,pch=16,col=rangi2)

#These are your functions that can be multiplied and added to get a mean prediction for each data point.
lines( d$ext , B[,1] ,col=2)
lines( d$ext , B[,2] ,col=3)
lines( d$ext , B[,3] ,col=4)
lines( d$ext , B[,4] ,col=5)
lines( d$ext , B[,5] ,col=6)


#You fit the model like this. The predicted value includes the intercept and sum of products of splince functions and their coefficiens w. w is the set of regression parameters, that you need to assign with a prior. 
mS <- quap(
  alist(
    cat_love ~ dnorm( mu , sigma ) ,
    mu <- a + B %*% w ,
    a ~ dnorm(0,1),
    w ~ dnorm(0,2),
    sigma ~ dexp(1)
  ), data=list( cat_love=d$cat_love , B=B ) ,
  start=list( w=rep( 0 , ncol(B) ) ) )

# Visualize the spline contributions - the products of spline functions and the mean of w coefficiens
post <- extract.samples( mS )
w <- apply( post$w , 2 , mean )
plot(ext,cat.love5,pch=16,col=rangi2)

lines( d$ext , w[1]*B[,1], col=2)
lines( d$ext , w[2]*B[,2], col=3)
lines( d$ext , w[3]*B[,3], col=4)
lines( d$ext , w[4]*B[,4], col=5)
lines( d$ext , w[5]*B[,5], col=6)


#You can see a similar message here. Parameters w2 and w5 seems to be most important
precis(mS,depth=2)


#And now see the visualization together with the most likey prediction curve and its 89% CI 
mu <- link( mS )
mu_mean <- apply(mu,2,mean)
mu_PI <- apply(mu,2,PI,0.97)
plot( d$ext , d$cat_love , col=rangi2 , pch=16 )
lines(d$ext,mu_mean)
shade( mu_PI , d$ext )

#The spline model (that includes a differently defined set of hypotheses - all hypotheses that can be describes as a sum of 5 cubic splines times their assigned parameters - agins, see chanper 4.5) outperforms the generative model m5 with the same amount of parameters for 100 participants! (but the simple linear regression with 2 parameters is still better when it comes to AIC)
compare(m1,m5,mS)

#There are non-bayesian, more descriptive approches that play on a similar not.
#Sometimes these can be also very useful in exploration studies, correlation studies etc, so zou should get familiar with them.

#Local regression (polynomial, here degree 2 polynomilas)
#considers few points (or a small span window) around each point and fits small rergression segment there (polynomial, here degree 2 polynomilas), as this window shifts, so does the parameter value and a nice smooth curve is fitted. (I am sure ggplot and similar toolboxes have a nice built-in functions to do this)

l <- loess(d$cat_love ~ ext, span=0.75)

#Predict the line and the standard error of the estimate
newx<-seq(min(d$ext),max(d$ext),l=1000)
newy<-predict(l,newdata=data.frame(ext=newx),se=T)

#Plot the results like this
plot( d$ext , d$cat_love , col=rangi2 , pch=16 )
lines(newx,newy$fit,col=1)
polygon(c(newx,rev(newx)),c(newy$fit+newy$se.fit,rev(newy$fit-newy$se.fit)),border=NA,col="#00000030")


#Another interesting method is KDE (Kernel Density estimation, check the wiki page: https://en.wikipedia.org/wiki/Kernel_density_estimation)
#It is a really interesting approach that does not assume too much about the causality. It just envelopes each data point with a density kernel (e.g. a multivariate gaussian distribution). The sum of all density kernels is than the density estimate of data probability (i.e. The probability of combination of data values). This comes useful when you compare data distrbutions between two samples etc.

library(ks)
library(viridis)

#We have to input the multivariate data as a matrix
dmat<-cbind(ext,cat.love5)

#We have to define the density kernel (here we use the same variation along both dimensions and with no covariance)
H <- diag(c(2, 2))

#This is how the Kernel density is evaluated
KDE<-kde(x=dmat, H=H)

#This is now a complicated object with a lots of contours, coordinates etc. They all summarize data density on a 2D plane
str(KDE)

#You can plot the results for example like this, the yellow colour correspond to teh highest density:
plot( d$ext , d$cat_love , col=rangi2 , pch=16 )
image(KDE$eval.points[[1]], KDE$eval.points[[2]], KDE$estimate,col=viridis(200),add=T)
points(KDE$x,col=1)


#You can play around with this tool to grasp its potential
ext3<-seq(-2,2,0.01)
cat.love3<-ext*(-0.8)+(ext^2)*0.4+(ext^3)*(-0.5)+rnorm(length(ext),0,0.2)
plot( ext3 , cat.love3 , col=rangi2 , pch=16 )

dmat<-cbind(ext3,cat.love3)
H <- diag(c(0.2, 0.2))

KDE<-kde(x=dmat, H=H)

plot( ext3 , cat.love3 , col=rangi2 , pch=16 )
image(KDE$eval.points[[1]], KDE$eval.points[[2]], KDE$estimate,col=viridis(200),add=T)
points(KDE$x,col=1)





#Inspect the kernels in a single data point scenario
dmat<-cbind(0,0)

H <- diag(c(0.2, 0.2))
KDE<-kde(x=dmat, H=H)

plot( NULL, xlim=c(-2,2),ylim=c(-2,2) )
image(KDE$eval.points[[1]], KDE$eval.points[[2]], KDE$estimate,col=viridis(200),add=T)
points(KDE$x,col=1)


#Kernel with correlated density along x and y axes
H <- matrix(c(1,0.6,0.6,1),ncol=2)
KDE<-kde(x=dmat, H=H)

plot( NULL, xlim=c(-2,2),ylim=c(-2,2) )
image(KDE$eval.points[[1]], KDE$eval.points[[2]], KDE$estimate,col=viridis(200),add=T)
points(KDE$x,col=1)


#Narrow kernel
H <- matrix(c(1,0,0,0.25),ncol=2)
KDE<-kde(x=dmat, H=H)

plot( NULL, xlim=c(-2,2),ylim=c(-2,2) )
image(KDE$eval.points[[1]], KDE$eval.points[[2]], KDE$estimate,col=viridis(200),add=T)
points(KDE$x,col=1)


#And look:
dmat<-cbind(x=c(-2,1,0.5),y=c(0,-0.8,0.1))
  
H <- matrix(c(1,0,0,0.25),ncol=2)
KDE<-kde(x=dmat, H=H)

plot( NULL, xlim=c(-3,3),ylim=c(-3,3) )
image(KDE$eval.points[[1]], KDE$eval.points[[2]], KDE$estimate,col=viridis(200),add=T)
points(KDE$x,col=1)


#This is all KDE does. It just adds density distributions around data points. It is a agnostic non-modelistic (nonparametric!) descriptive statistical analysis. (But it can yield inetersting results and can be summarized in and ineteresting way)
#You can reac more on KDE and other non-parametric methods here:
#https://bookdown.org/egarpor/NP-UC3M/kde-ii-mult.html



