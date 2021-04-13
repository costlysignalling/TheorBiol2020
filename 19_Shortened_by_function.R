
# Load packages (R studio should suggest installation of those, that are not present on your computer)
library(Morpho)
library(geomorph)
library(abind)
library(RRPP)
library(ggpubr)
library(standardize)
library(lmerTest)
library(vioplot)
library(randomcoloR)
library(ggplot2)
library(ppcor)
library(ggfortify)
library(tidyverse)
library(cluster)
library(factoextra)
library(dendextend)
library(corrplot)
library(randomcoloR)
library(mapplots)

# Set some defaults to avoid extensive report messages
formals(procD.lm)$print.progress<-FALSE
formals(morphol.disparity)$print.progress<-FALSE

# Load data
load("Symmetrized_coordinates_height.RData")  # Loads facial coordinates into object gpone.
#Here, only coordinates of faces for which the information about body height is available (N=1114). If you want to run parts of the analysis, where this information is not necessary on the unrestricted sample (N=1317) load  data file "Symmetrized_coordinates_all.RData" The rest of the script shall be identical, some parts of the analysis (like the decomposition to the allometric/nonallometric components) will, however, not work.

#load indications of links for the visualizations of facial morphology
links<-read.table("linksR.txt")

dimdat<-read.delim("dimorphism_data.txt") # Loads other variables
dimdat<-dimdat[!is.na(dimdat$height),] # Use only the lines, where body height is reliably assessed 

# Divide the data to two sets according to the sex
fdat <- subset(dimdat, sex=="F")
mdat <- subset(dimdat, sex=="M")

# Connect these data frames to the final data to ensure there are no iindividuals with unsure sex
dat<- rbind(fdat, mdat)

#Inspect the coordinates
plot(gpagen(gpone, PrinAxes = F))

# Create geomorph data frame with varibales corresponding to several variables from dimdat
onegdf <- geomorph.data.frame(coords = gpone, sex = dimdat$sex, set = dimdat$set, nat = dimdat$nat, age = dimdat$age, weight = dimdat$weight, height = dimdat$height, attractiveness = dimdat$Attractiveness)

# Subset landmark coordinates according to sex 
shapesex<- coords.subset(onegdf$coords, onegdf$sex)

# Create array ordered by sex from the shapesex data frame
shape <- abind (shapesex$F, shapesex$M)

# Generalized procrustes analysis of the resulting array
gpsh<-gpagen(shape,  ProcD = F, curves = NULL, PrinAxes = F, Proj = TRUE)
plot(gpsh)

# Create geomorph data frame with varibales corresponding to several vectors from dat which has the same order of lines as the set of shapes in gpsh - females then males
gtf<-geomorph.data.frame(gpsh, sex = dat$sex, set =  dat$set, nat = dat$nat, age = dat$age, weight = dat$weight, height = dat$height, attractiveness = dat$Attractiveness)

str(gtf)


# Investigate the outliers
invisible(capture.output(
  plotOutliers(gtf$coords, groups = gtf$sex, inspect.outliers = FALSE)
))
# Some points are indicated as potential outliers, but all the data points create a smooth curve, so we decide to retain the whole data

plotTangentSpace(gtf$coords, axis1=1, axis2=2, warpgrids=F, groups = gtf$sex, legend = T)
plotTangentSpace(gtf$coords, axis1=2, axis2=3, warpgrids=F, groups = gtf$sex, legend = T)
plotTangentSpace(gtf$coords, axis1=1, axis2=2, warpgrids=F, groups = gtf$nat, legend = T)
plotTangentSpace(gtf$coords, axis1=2, axis2=3, warpgrids=F, groups = gtf$nat, legend = T)
# Scatter plots do not indicate any problems of extreme outliers

# Analysis of variance in the face shape explained by sex
reg1<-procD.lm(coords ~ sex, iter = 999, data = gtf, weights = NULL)
summary(reg1)

# Extract the centers of the sex-specific clouds of points in the multidemensional space. Intercept corresponds to the female mean, sexM slope to the difference between male and female mean.
coefficients<-coef(reg1, test = F)
sex.vecs<-coefficients[2,]

#Projection of facial shapes on the vector connecting female and male mean, the resulting score is saved as sexscores variable
scores <- two.d.array (gpsh$coords) %*% t(coefficients)
sexscores <- scores [,2]
hist(sexscores)

#Save these sexscores for a future reference to an object with "original" sexscores
orig_sexscores<-sexscores

# Restart graphics window
dev.off()

# Preparing the maleness-femaleness scores for each face within each country and maleness-femaless vectors for each country
onecoords<-coords.subset(onegdf$coords, onegdf$nat)

str(onecoords)

# We repeat the same procedure for each nationality separately
# Brazil

giveSexVec<-function(coordnat,sexvec){
  
  natgdf <- geomorph.data.frame(coords = coordnat, sex = sexvec)
  reg1<-procD.lm(coords ~ sex, iter = 999, data = natgdf)
  
  print(summary(reg1))
  
  coefficients<-coef(reg1, test = F)
  
  scores <- two.d.array (coordnat) %*% t(coefficients)
  natsc <- scores [,2]
  
  fnatsc <- natsc [which(sexvec=="F")]
  mnatsc <- natsc [which(sexvec=="M")]
  
  natcoef<-t(coefficients)[,2]
  
  return(list(fnatsc=fnatsc,mnatsc=mnatsc,natcoef=natcoef))
}

braz.sc<-giveSexVec(coordnat=onecoords$BRAZ,sexvec=dat$sex[dat$nat=="BRAZ"])
cmr.sc<-giveSexVec(coordnat=onecoords$CMR,sexvec=dat$sex[dat$nat=="CMR"])
col.sc<-giveSexVec(coordnat=onecoords$COL,sexvec=dat$sex[dat$nat=="COL"])


giveSexVec(coordnat=onecoords$COL,sexvec=dat$sex[dat$nat=="COL"])

str(onecoords)
levels(dat$nat)

scores.by.nat<-lapply(1:8,function(i){giveSexVec(coordnat=onecoords[[i]],sexvec=dat$sex[dat$nat==levels(dat$nat)[i]])})


new.order<-c(2,4,7,5,6,8,3,1)

levels(dat$nat)[new.order]

str(scores.by.nat)

scores.by.nat2<-scores.by.nat[new.order]

str(scores.by.nat2)

#We connect the nation specific scores into vectors of all sex scores - equivalents of previous approach
fsexscores1 <-unlist(lapply(scores.by.nat2,function(l){l$fnatsc}))
msexscores1 <-unlist(lapply(scores.by.nat2,function(l){l$mnatsc}))
sexscores1<-c(fsexscores1,msexscores1)


vioplot(sexscores1~onegdf$sex, range = 1.5, horizontal = F, col = "gold", plotCentre = "point")
vioplot(sexscores1~onegdf$nat, range = 1.5, horizontal = F, col = "gold", plotCentre = "point")

#Visualization of both sexes malenes-femaleness distributions
vioplot(fsexscores1~gtf$nat[gtf$sex=="F"], range = 1.5, horizontal = F, col = "#FF770080", plotCentre = "point", ylim=c(-0.004, 0.004), xlab="Country",ylab="SShD")
vioplot(msexscores1~gtf$nat[gtf$sex=="M"], range = 1.5, horizontal = F, col = "#3366FF80", plotCentre = "point",add=T,xlab="",ylab="")
vioplot(sexscores1~gtf$nat, range = 1.5, horizontal = F, col = "#FFFFFF00", lineCol = NA,add=T,xlab="",ylab="",border="#FF0066",lwd=1.8,rectCol=NA,pchMed=NA)
title (main = "Sexual Shape Dimorphism")


str(scores.by.nat)

# We investigate the correlation between maleness-femaleness and attractivenes in each subset. We reverse the maleness-femaleness scale in females to expect positive correlation with the "sex-typicality" in both, males and females. Partial correaltion controlling for age of participants was conducted as well. We create a new variable called Sextypicality for each nationality

# BRAZ

label="BRAZ"
dat=dat
sublist=scores.by.nat[[1]]

GivePlot<-function(label,dat,sublist){
  
  natdat <- subset(dat, nat == label)
  fnatdat <- subset (natdat, sex == "F")
  mnatdat <- subset (natdat, sex == "M")
  
  Sextypicality <- c(-1*sublist$fnatsc, sublist$mnatsc)
  natdat1 <- cbind (natdat, Sextypicality)
  
  #We construct a linear model of the interaction between sex and sextypicality. We plot the regression lines together with 95% confidence intervals.
  
  fontsize<-14
  
  model<-lm(Attractiveness~sex*Sextypicality, data=natdat1)
  
  print(summary(model))
  
  DF.pred<-expand.grid(Sextypicality=seq(min(Sextypicality),max(Sextypicality),0.0001),sex=c("F","M"))
  PRED<-predict.lm(model,newdata = DF.pred, type="response",interval="confidence")
  gg.predict<-ggplot(data.frame(DF.pred,PRED),aes(y = fit,x=Sextypicality,group=sex))+
    geom_line(data=data.frame(DF.pred,PRED),aes(y = fit,x=Sextypicality,group=sex,color=sex))+
    geom_ribbon(data=data.frame(DF.pred,PRED),aes(ymin=lwr, ymax=upr, x=Sextypicality, fill = sex,group=sex), alpha = 0.2)+ geom_point(data=natdat1,aes(x=Sextypicality,y=Attractiveness,colour=sex, group=sex), alpha=0.5)+
    theme_bw(base_size = fontsize)
  
  gg.predict <- gg.predict + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
  
  nat.plot<- gg.predict + scale_color_discrete(labels=c("Women", "Men"))+
    scale_fill_discrete(labels=c("Women", "Men"))+
    scale_y_continuous(name="Attractiveness")
  
  return(nat.plot)
  
}


GivePlot(label="BRAZ",dat=dat,sublist=scores.by.nat[[1]])

natlabs<-levels(dat$nat)

plots<-lapply(1:8,function(i){GivePlot(label=natlabs[i],dat=dat,sublist=scores.by.nat[[i]])})

str(plots[[1]])

plots[[1]]
plots[[2]]
plots[[3]]
plots[[4]]
plots[[5]]
plots[[6]]
plots[[7]]
plots[[8]]



