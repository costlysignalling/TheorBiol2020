#Making sense of the data using correlations and network analysis
#Because sometimes you do not really need causal models. Sometimes you just look for patterns of co-occurence or, co-change, i.e. for correlation

#First, you need to obtain some data. Questions of Big 5 items from the package psych will do.
library(psych)
data(bfi)

#We take only the Big5 items
bfiSub <- bfi[,1:25]

#Download the questions and create a vector of "groups"
Names <- scan("http://sachaepskamp.com/files/BFIitems.txt",what = "character", sep = "\n")
Groups <- rep(c('A','C','E','N','O'),each=5)

#Look
Names
Groups

#In total we have 2800 participants
nrow(bfiSub)

#Most functions in this script would work also with missing values (all of them only with a little extra effort and a bit of code tinkering), but we remove incomplete responses for a perfect clarity and coherence with these few functions, that require complete observations by default.
bfiSub<-bfiSub[complete.cases(bfiSub),]
nrow(bfiSub)

#You can easily create a correlation matrix. Correlation is a dimesionless measure of association. Covariance of two variables divided by a product of their standard deviations. Correlation does not by default assume linear relationship. Rather it assumes a vague "ellipsoid" shape of density distribution in a 2D plane with correlated variables as axes.
#See here: https://en.wikipedia.org/wiki/Pearson_correlation_coefficient
#Pearson correlation is the most frequent approach
bfiPearson <- cor(bfiSub)

#There are many more. For discrete values (such as 1-7 values on a discontinuous likert scale) polychoric correlations are recommended: https://en.wikipedia.org/wiki/Polychoric_correlation
#Polychoric correlations assume a continuous latent variables underlying discrete values. This comes handy aspecially if only 0-1 values are availible. In 1-7 likert, it is really not a big deal.

library(qgraph)
#cor_auto from the qgraph package attempts to identify vectors of discrete values use appropriate methods where necessarry. 
bfiPolychoric <- cor_auto(bfiSub)

#But look. It really makes almost no difference:
cor(c(bfiPearson),c(bfiPolychoric))

plot(c(bfiPearson),c(bfiPolychoric),xlim = c(-1,1),ylim=c(-1,1))
lines(c(-1,1),c(-1,1),col=2)
abline(v=0,lty=2)
abline(h=0,lty=2)
#Only Person's r tends to underestimate correlations far from 0.

#Use polychoric correlations if you feel like it. But really... no big deal. It does not change much.

bfiCors<-bfiPearson #You can change the right side of this line to bfiPolychoric to run all the analyses again with Polychoric correlations.

#It is usefull to visualize huge correlation matrices such as this one.
#Classical corrplot. Not extremely elegant, but comes useful if you have only a few variables to explore.
library(corrplot)

corrplot(bfiCors, method="ellipse")
corrplot.mixed(bfiCors, lower = "number", upper = "circle")

#It is better to export large correlogram like this into an image
pdf("corrplot.pdf",width=16,height=16)
corrplot.mixed(bfiCors, lower = "number", upper = "circle")
dev.off()

#png is a nice and compact raster format. I suggest you to set only one parameter as a measure (such as width) and represent the other (height) in relation to it (here the ratio between weight and height is 1).
w<-30
png("corrplot.png",width=w,height=w,units="cm",res=600)
corrplot.mixed(bfiCors, lower = "number", upper = "circle")
dev.off()

#Check the whole list of options here: http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram
#Or here: https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html


#It is also useful to know correlation visualizations using heatmap
palette<-colorRampPalette(c("red","white","green"))(200)
heatmap(bfiCors, col=palette, symm=T)
#Always set symmetry to TRUE, this way, the color pallet will be anchored with 0 at the white center.
#Heatmap visualization will conduct hierarchical clustering automatically, and reorder variables to fit this clustering. This is a useful approach in genetic studies etc.
#If you are interested in clustering algorithms and hierarchical clusetring in general, you shuld definitely check this out: https://hdbscan.readthedocs.io/en/latest/how_hdbscan_works.html (just to give you a hint, that simple hclust here is not the end, and these method are going to develop further during the next 5 years. )

#Something similar with emphasis on positive correlations can be achieved with corrplot as well. Just order variables using hclust method. Also try to play with addrect (add rectangle).
corrplot(bfiCors, order = "hclust", addrect = 5)
paste(names(bfiSub),Names)
#Iterestig, huh? Using this method does not extract 5 underlying factors, because the grouping prefers to outline squares around positively associated variables. But check also the "red" zones of the corrplot.
#But also this investigation is not without ineterest. See those iinetrcorrelated measures from different dimensions of Big5.
corrplot(bfiCors, order = "hclust", addrect = 5)
paste(names(bfiSub),Names)[match(c("A1","E1","E2"),names(bfiSub))]
#Strange cluster of 2 almost independent dimensions!
#"autism dimension"
corrplot(bfiCors, order = "hclust", addrect = 6)
#"fun and boring autisms separate first"
#"Did they ask wrong questions?"

#Also look at the huge cluster of C,A, and E questions in the bootomright corner! 
#"Goodness dimension"
paste(names(bfiSub),Names)[match(c("C3","C1","C2","A4","A2","A3","A5","E5","E3","E4"),names(bfiSub))]
#"Uslesness :D dimension"
paste(names(bfiSub),Names)[match(c("C4","C5","N3","N1","N2","N4","N5"),names(bfiSub))]
#It seems the some of coscientiouness questions tend to clusted more with "depression" and "emotional instability" or perhaps "low selfesteem" than with "slack" in general.

#You can check the other variables as well
#Shallowness
paste(names(bfiSub),Names)[match(c("O2","O5"),names(bfiSub))]
#Weird questions lead to weird predictions
paste(names(bfiSub),Names)[match(c("O4","O1","O3"),names(bfiSub))]
#Because this kind of openess to a new experience completely lacks the notion of courage.

#If you want to conduct a proper factor analysis and not just positive-cor skewed "overinterpretation witchcraft" just do this:
#Parallel analysis is able to estimate (with nice permutation approach) a parsimonoius number of underlying factors:
fa.parallel(bfiSub)
#This is super interesting. There are 5 relevant orthogoal dimensions (PC) of the data, but it should be possible to extract 6 meaningful underlying (possibly intercorrelated) factors.
#Oblimin roatation is now a default. See
?fa
#for more deatils on rotations etc.
#(Btw all rotations in fa lead to the same amount of "total explained variance" so discoveries like these go beyond "simple predictions")
res.fa<-fa(bfiSub,nfactors=5,rotate = "varimax")
diagram(res.fa)
#This is the usual way how Big5 is extracted using EFA exploratory factor analysis. We do not have time to do CFA (confirmatory factor analysis), but if it does interest you see here: https://lavaan.ugent.be/tutorial/cfa.html
res.fa<-fa(bfiSub,nfactors=5,rotate = "oblimin")
diagram(res.fa)
#Oblimin rotation does not change much. But the number of factors to extract was 6, right?
res.fa<-fa(bfiSub,nfactors=6,rotate = "oblimin")
diagram(res.fa)
#The MR6 is super inetersting. It is a bridging factor capturing a very specific aspect of personality.

res.fa
#It is not the most relevant factor for any question but it seems to be second for many.
paste(names(bfiSub),Names)[match(c("A1","C4","E3","E4","O2","O5"),names(bfiSub))]
#I think people scoring high on this factors are dickheads.
#These are people who think that Do things in a half-way manner and avoiding difficult reading material is a good thing.
#It matters how people read questions as well! The larger sample you have, the more meaningful factors you can extract! Just do it. 


#The last thing is an alterantive strategy with network analysis (i use the package qgraph)
#It can start simple
autoCorGraph <- qgraph(bfiCors, layout = "spring", details = T)

#But it is a highly personalizable function
corGraph <- qgraph(bfiCors, layout = "spring", graph = "cor",
                   nodeNames = Names, groups = Groups, legend.cex = 0.3,
                   cut = 0.3, maximum = 1, minimum = 0, esize = 20,
                   vsize = 5, repulsion = 0.8, details=T)

#You can show correlations on the edges:
corGraph <- qgraph(bfiCors, layout = "spring", graph = "cor",
                   nodeNames = Names, groups = Groups, legend.cex = 0.3,
                   cut = 0.3, maximum = 1, minimum = 0, esize = 20,
                   vsize = 5, repulsion = 0.8, details=T,edge.labels = T)


#But than better make them smaller and export the network as a .pdf for exploration.
corGraph <- qgraph(bfiCors, layout = "spring", graph = "cor",
                   nodeNames = Names, groups = Groups, legend.cex = 0.3,
                   cut = 0.3, maximum = 1, minimum = 0, esize = 20,
                   vsize = 5, repulsion = 0.8, details=T,edge.labels = T,
                   filetype = "pdf",filename = "network",edge.label.cex = 0.5)


#Look it also gives you all the desciptives if you save it as an object!
str(corGraph)

#I will finaly save the names as a single object
n<-names(bfiSub)

#And crate this tab - from-to node labels and the asssociated correlation
edge.tab<-cbind(n[corGraph$Edgelist$from],n[corGraph$Edgelist$to],round(corGraph$Edgelist$weight,2))

head(edge.tab)

edge.tab

#Beautiful. Lets see the relationship between correlation and linear regression.
#Correlation has an easy interpretation in terms of linear regression, so it really is not true, that we should not think about correltions in terms of liear relationships :D

#We can do it with rethinking obviously. (But that will make us skeptical about very large and small correlation coefficients)
#Correlation is a dimensionless (in terms of SDs) chnege of one variable with another
library(rethinking)

#So we neet to standardize
d<-list(
  A1=standardize(bfiSub$A1),
  A2=standardize(bfiSub$A2),
  A3=standardize(bfiSub$A3),
  A4=standardize(bfiSub$A4),
  A5=standardize(bfiSub$A5),
  
  C1=standardize(bfiSub$C1),
  C2=standardize(bfiSub$C2),
  C3=standardize(bfiSub$C3),
  C4=standardize(bfiSub$C4),
  C5=standardize(bfiSub$C5),
  
  E1=standardize(bfiSub$E1),
  E2=standardize(bfiSub$E2),
  E3=standardize(bfiSub$E3),
  E4=standardize(bfiSub$E4),
  E5=standardize(bfiSub$E5),
  
  N1=standardize(bfiSub$N1),
  N2=standardize(bfiSub$N2),
  N3=standardize(bfiSub$N3),
  N4=standardize(bfiSub$N4),
  N5=standardize(bfiSub$N5),
  
  O1=standardize(bfiSub$O1),
  O2=standardize(bfiSub$O2),
  O3=standardize(bfiSub$O3),
  O4=standardize(bfiSub$O4),
  O5=standardize(bfiSub$O5))

#Check out summaries for the whole sample
lapply(d,mean)
lapply(d,sd)


m1 <- quap(alist(
  A2 ~ dnorm(mu,sigma),
  mu<-a+b*A3,
  a~dnorm(0,0.1),
  b~dnorm(0,4),
  sigma~dexp(1)),
  data=d)

m2 <- quap(alist(
  A3 ~ dnorm(mu,sigma),
  mu<-a+b*A2,
  a~dnorm(0,0.1),
  b~dnorm(0,4),
  sigma~dexp(1)),
  data=d)

#Look, it is the same!
edge.tab[3,]
precis(m1)
precis(m2)

#Partial correlation network is extremely useful. It is the ulimately careful structural approach. This is what most people in evolutionary psychology actually need, but only rarely use. (But than again it is a very novel method)
pcorGraph <- qgraph(bfiCors, layout = "spring", graph = "pcor",
                   nodeNames = Names, groups = Groups, legend.cex = 0.3,
                   cut = 0.1, maximum = 1, minimum = 0, esize = 20,
                   vsize = 5, repulsion = 0.8, details=T)

edge.tab<-cbind(edge.tab,round(pcorGraph$Edgelist$weight,2))
head(edge.tab)

#"The partial correlation between two nodes after conditioning on all other nodes is directly equivalent to the predictive quality between two nodes that would be obtained in multiple regression!"
#So again: Correlation - single regression, partial correlation - multiple regression. Easy right?

mm1 <- quap(alist(
  A3 ~ dnorm(mu,sigma),
  mu<-a+b*A2+c[1]*A1+c[2]*A4+c[3]*A5
  +c[4]*C1+c[5]*C2+c[6]*C3+c[7]*C4+c[8]*C5
  +c[9]*E1+c[10]*E2+c[11]*E3+c[12]*E4+c[13]*E5
  +c[14]*N1+c[15]*N2+c[16]*N3+c[17]*N4+c[18]*N5
  +c[19]*O1+c[20]*O2+c[21]*O3+c[22]*O4+c[23]*O5,
  a~dnorm(0,0.1),
  b~dnorm(0,4),
  c~dnorm(0,4),
  sigma~dexp(1)),
  data=d, start=list(c=rep(0,23)))

mm2 <- quap(alist(
  A2 ~ dnorm(mu,sigma),
  mu<-a+b*A3+c[1]*A1+c[2]*A4+c[3]*A5
  +c[4]*C1+c[5]*C2+c[6]*C3+c[7]*C4+c[8]*C5
  +c[9]*E1+c[10]*E2+c[11]*E3+c[12]*E4+c[13]*E5
  +c[14]*N1+c[15]*N2+c[16]*N3+c[17]*N4+c[18]*N5
  +c[19]*O1+c[20]*O2+c[21]*O3+c[22]*O4+c[23]*O5,
  a~dnorm(0,0.1),
  b~dnorm(0,4),
  c~dnorm(0,4),
  sigma~dexp(1)),
  data=d, start=list(c=rep(0,23)))

precis(mm1)
precis(mm2)

head(edge.tab)

#Not bad, right?

#You can employ model selection and "prune" the graph
#Simple threshold approach
holmGraph <- qgraph(bfiCors, layout = "spring", graph = "pcor",
                    threshold = "holm", sampleSize = nrow(bfi),
                    nodeNames = Names, groups = Groups, legend.cex = 0.3, 
                    cut = 0.1, maximum = 1, minimum = 0, esize = 20,
                    vsize = 5)

#Employ the least absolute shrinkage and selection operator
glassoGraph <- qgraph(bfiCors, layout = "spring", 
                      graph = "glasso", sampleSize = nrow(bfi),tuning=0.5,
                      nodeNames = Names, groups = Groups, legend.cex = 0.3, 
                      cut = 0.1, maximum = 1, minimum = 0, esize = 20,
                      vsize = 5,threshold = TRUE)

#You can explore the function 
?qgraph

#And calulate centrality and clustering coefficient for your nodes.
centralityPlot(
  list(
    saturated = pcorGraph,
    glasso = glassoGraph)
)

clusteringPlot(
  list(
    saturated = pcorGraph, 
    glasso = glassoGraph)
)

#See these detailed tutorials:
#http://psychosystems.org/tag/network-analysis/
#http://sachaepskamp.com/files/Cookbook.html

#Also see this paper:
#https://eiko-fried.com/wp-content/uploads/Epskamp-Fried-2018-Tutorial-partial-corr.pdf

#If you want to recreate the partial correlation approach perfectly, it is better to use simple analytical linear model - a close cousin to prior-less correlations and do not standardize before the model evaluation (more on this can be found in the enclosed paper above)
mlm1 <- lm(A3 ~ A2+A1+A4+A5+C1+C2+C3+C4+C5+E1+E2+E3+E4+E5+N1+N2+N3+N4+N5+O1+O2+O3+O4+O5,data=bfiSub)
mlm2 <- lm(A2 ~ A3+A1+A4+A5+C1+C2+C3+C4+C5+E1+E2+E3+E4+E5+N1+N2+N3+N4+N5+O1+O2+O3+O4+O5,data=bfiSub)

#Extract betas
beta1<-summary(mlm1)$coefficients[,1][2]
beta2<-summary(mlm2)$coefficients[,1][2]

#And a vector of residuals
resid1<-summary(mlm1)$residuals
resid2<-summary(mlm2)$residuals

beta1
beta2

#Although betas are slightly different
#After scaling according to residual variance, they agree perfectly 
(beta1*sd(resid2))/sd(resid1)
(beta2*sd(resid1))/sd(resid2)

head(edge.tab)

#So there you have it
#But what should you use then? Cor or pcor apporach?
#It really depends (and also: both). The single regression or correlation does not change with other variables. The multiple regression is much more powrful and much more dangerous tool. If you omit a key variable or include a spurious one instead, you skew your results more. This cannot happen if you investgete a relationship between two variables only. More on this in 5th chapter, 6th chapetr and the rest of your life...

A<-rnorm(100,0,5)
B<-A+rnorm(100,0,2)
C<--A+rnorm(100,0,2)
D<-B+C+rnorm(100,0,1)

d<-data.frame(A,B,C,D)
d2<-data.frame(A,B,D)

graph_partial <- qgraph(cor(d), graph = "cor", layout = "spring",edge.labels = T)
graph_partial <- qgraph(cor(d2), graph = "cor", layout = "spring",edge.labels = T)

graph_partial <- qgraph(cor(d), graph = "pcor", layout = "spring",edge.labels = T)
graph_partial <- qgraph(cor(d2), graph = "pcor", layout = "spring",edge.labels = T)

#The relationships in partial correlation network change (especially the relationship between A and D) if you omit C. But even the correct dataset with all variables assumes a partial correlation between C and B (even though there was no causal relationship between them). 

#If you have a good theory on the generative process (such that A can influence B and C, but neither C and B can influence A), you can use it! The ULAM will estimate the parameter values correctly. You can still reveal residual correlation between B and C and contribution of A to D. If it is there, it will show. If not, the parameter estimate will be close to 0. It is much more precise that just tossing everything into a correlation plot or network analysis.

#The full generative model might for example look like this:

mABCD <- ulam(alist(
  D ~ normal(muD,sigmaD),
  
  muD<-aD+bAD*A+bBD*B+bCD*C,
  
  aD~dnorm(0,1),
  
  bAD~dnorm(0,4),
  bBD~dnorm(0,4),
  bCD~dnorm(0,4),
 
  sigmaD~dexp(2),
  
  c(B,C) ~ multi_normal(c(muB,muC),RhoBC,sigmaBC),
  muB<-aB+bAB*A,
  muC<-aC+bAC*A,
  
  aB~dnorm(0,1),
  aC~dnorm(0,1),
  
  bAB~dnorm(0,4),
  bAC~dnorm(0,4),
  
  RhoBC~lkj_corr(2),
  sigmaBC~dexp(2)),
  data=d)

#With a generative model with a simple directional causality (even if we allow it to include additional parameters corresponding to the non-causal correlation between B and C and direct causality between A->D, distribution of these parameter estimates simply peaks around 0), we recover employed parameter values exactly! Just check the results
#The only downsideis the computational intensity.
precis(mABCD,depth=3)
plot(precis(mABCD,depth=3))

#You can plot the correct values over the estimates.
points(c(2,2,1,0,0,1,-1,1,0,0,1,1,1,0,0),1:15,col=2)
#And this is what the statistical inference should be about :) (mostly)



