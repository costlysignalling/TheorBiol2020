
library(rethinking)

data<-read.table("14_caltha_win.txt", header = TRUE, sep = ",",stringsAsFactors = F)
data<-data[-45,]
data$location<-as.factor(data$location)
length(levels(data$location))

#Vyrobim si numerickou lokaci rovnou v datasetu
data$locID<-as.integer(data$location)

#vektor unikatnich latitud lokalit
locs<-sapply(unique(data$location),function(x){unique(data$latitude[data$location==x])})

#Vektor loc ID per lokalita (ne per pozorovani)
locID<-unique(data$locID)
locID[order(locID)]
locs[order(locID)]

#Dataset ted obsahuje jen 42 lokalit postupne, prvni ve vektoru je latituda lokality 1, druha je latituda lokality 2 atd..
d <- list(
  uvp = data$uvp,
  loc = as.integer(data$location),
  lat = locs[order(locID)]-mean(locs)
)
str(d)


p<-0.6
theta<-22

v<-rbeta(1000,0.5,0.5)

p<-0.5
theta<-0.2

v<-rbeta(1000,p*theta,(1-p)*theta)

dens(v)

#Co se ted v tom modelu deje?
#Abychom ziskali p pro kazde pozorovani, udelame si vlastne model, jak ovlivnuje latiduda lokality prumerne p na dane lokalite.
#Ocekavany podil UV na lokalite je a+b*lat, kde lat je latituda dane lokality
#Lokality se ale lisi i jinak, to rozdeleni je ve vysledku overdispersed, proto pridavam do rovnice tu sigmu (ja to tady mam decnetered, aby to lip samplovalo)
#Proste pro kazdou z tech 42 lokalit si najdu jeji relativni odchylku od ocekavaneho prumeru na zaklade latitudy (to je to z) a vynasobim to standardni odchylkou, ktera je tady ale jasne definovana jako odchylka od ocekavane hodnoty vypocetne na zaklade vztahu mezi latitudou a ocekavaou hodnotou p.
#Kolem teto ocekavane hodnoty teprve "vyrabim" data per kvet (to je ta likelihood funkce beta2)

m1 <- ulam(
  alist(
    uvp ~ dbeta2(p, theta),
    vector[330]:logit(p)<-a+b*lat[loc]+z[loc]*sigma,
    z[loc]~dnorm(0,1),
    a~dnorm(0,0.3),
    b~dnorm(0,0.2),
    sigma~dexp(1),
    theta~dexp(1)
  ), data=d,cores=4,chains=4,log_lik=T,sample=T)

save(m1,file="m1.Rdata")

load("m1.Rdata")

precis(m1)

#Parametry tedy ted maji jasnou interpretaci.
#a je intercept predikujici podil uv na lokalite s prumernou latitudou (vsimni si, ze tam vyse neprumeruju data$latidute, ale jen vektor latitud per lokalita, rozdil je malej, takze je to skoro fuk)
#b je zmena teto ocekavane hodnoty per lokalita se zmenou latitudy od prumeru
#sigma je variabilita (merena smerodatnou odchylkou) lokalit nad ramec jejich variabilty v latitude
#theta udava rozpliz podilu UV okolo ocekavane hodnoty v ramci jedne lokality

#Tam nahore je dulezitej ten trik s tim, ze ulam informuju, ze p ma dylku 330, kdyz to neudelam, zkusi se zaridit podle delky latitudy (42) a narazi na chybu. Je lepsi to zkontrolovat vzdycky ve stanu:
stancode(m1)
#Tohle vypada dobre, dela to to, co ma

post<-extract.samples(m1)
str(post)


dens(post$b)
sum(post$b<0)/length(post$b)
PI(post$b, prob= 0.97)

#vizualizace
lat_seq<-seq(-11,11,0.2)

p.link <- function(lat) inv_logit(post$a + post$b*lat)
p<-sapply(lat_seq, p.link)

p.mean<-apply(p,2,mean)
p.PI<-apply(p,2,PI,prob=0.89)

#Najdu 42 jasných barev, jednu pro každou lokalitu, aby bylo vidìt na tom grafu, odkud který puntík pochází
library(randomcoloR)
cols<-randomColor(42,luminosity = "bright")

plot(data$latitude-mean(locs),data$uvp, col=sapply(cols[data$locID],alpha,0.5),pch=16,xlab="latitude", xaxt="n",xlim=c(-10,10))
at <- c(-10,-5,0,5,10)
labels <- at+mean(locs)
axis( side=1 , at=at , labels=round(labels) )

lines(lat_seq,p.mean)
shade(p.PI, lat_seq)

#Cara v grafu mi ted ukazuje prumer uvp v dane latitude + sebrane body ze 42 lokalit. Na zaklade "a" a "b", ktere popisuji vztah uvp s latitudou a parametru sigma (disperze lokalit okolo teto ocekavane hodnoty) si muzu plotnout koridor, kde ocekavam prumery ruznych lokalit s danou latitudou.
sim.locs<-sapply(lat_seq, function(lat){
  inv_logit(rnorm(n=length(post$a),
                  mean=post$a+post$b*lat,
                  sd=post$sigma))})

dim(sim.locs)
str(lat_seq)

meansPI<-apply(sim.locs,2,PI,prob=0.89)
str(meansPI)

shade(meansPI,lat_seq)

#Kdyz si do toho budu chtit plotnout ty sebrany prumery per lokalita, muzu pouzit jeste ten kompletni posterior sady parametru z (nemusim to tahat empiricky a muzu pridat i nejaky chybovy usecky, zejo, zase 89% compatibility intervaly toho prumeru), udelam to smyckou, pro kazdou lokaci (to je ta smycka) si pro kazdej sample spocitam odhad prumeru a pak z toho vektoru vezmu 89% PI a prumer, transformuju to na p inverse logitem a hodim to do dane latitudy (pouziju ty nahodny barvy)
for(i in 1:42){
  uvpi<-post$a+post$b*d$lat[i]+post$z[,i]*post$sigma
  lines(rep(d$lat[i],2),inv_logit(PI(uvpi,prob=0.89)),col=cols[i],lwd=2)
  points(d$lat[i],inv_logit(mean(uvpi)),col=cols[i],lwd=2,cex=1.2,pch=21,bg=0)
}

#Toho bordelu uz je tam hrozne moc... Asi to bude chtit pozdeji nejak procistit :D, ale doufam, ze ta multilevel struktura je takhle jasnejsi.

#Posledni krok by byl nasamplovat z posetrioru prumeru konkretni pozorovani kytek (observations). To by byl jen koridor navic (vlastne jsi ho tam uz mel0), kde kolem kazdyho prumeru fiknu kytku na zaklade theta.
sim.obs<-sapply(lat_seq, function(lat){
  rbeta2(n=length(post$a),
         p=inv_logit(rnorm(n=length(post$a),
                           mean=post$a+post$b*lat,
                           sd=post$sigma)),
         theta=post$theta)})

obsPI<-apply(sim.obs,2,PI,prob=0.89)
#Aby tam nebyli porad ty cely bloky sedy barvy, da se to treba nakreslit jako prerusovana cara
lines(lat_seq,obsPI[1,],lty=2)

#Mimochodem, kdyz chces tu samplovaci blbost vyhladit, da se pouzit jednoduchej linearni model a pak abline (ukazu na ty horni care)
abline(lm(obsPI[2,]~lat_seq),lty=2)

#Jeste uplne nakonec si muzes s tou thetou nasamplovat hromadu predikovanejch pozorovani pro kazdou lokalitu a vykreslit to treba jako vycentrovanej violin plot.

for(i in 1:42){
  uvpo<-rbeta2(n=length(post$a),
               p=inv_logit(post$a+post$b*d$lat[i]+post$z[,i]*post$sigma),
               theta=post$theta)
  #This controls relative with of the polygon
  dens<-density(uvpo)
  polygon(d$lat[i]+c(dens$y,-1*rev(dens$y))*0.1,c(dens$x,rev(dens$x)),col=alpha(cols[i],0.2),border=NA)
}

#Pro prehlednost doporucuju flaknout ty sumarizace prumeru pres to:
for(i in 1:42){
  uvpi<-post$a+post$b*d$lat[i]+post$z[,i]*post$sigma
  lines(rep(d$lat[i],2),inv_logit(PI(uvpi,prob=0.89)),col=cols[i],lwd=2)
  points(d$lat[i],inv_logit(mean(uvpi)),col=cols[i],lwd=2,cex=1.2,pch=21,bg=0)
}

#Aby teda bylo uplne jasny, co se deje, tyhle estimaty (kdyz se nepustime do odhadu z), jsou nezavisly na konkretnich lokalitach:

plot(data$latitude-mean(locs),data$uvp, col=sapply(cols[data$locID],alpha,0.5),pch=16,xlab="latitude",type="n", xaxt="n",xlim=c(-10,10),ylim=c(0,1))
at <- c(-10,-5,0,5,10)
labels <- at+mean(data$latitude)
axis( side=1 , at=at , labels=round(labels) )

#Model zavislosti prumeru lokalit na latitude
lines(lat_seq,p.mean)
shade(p.PI, lat_seq)
#Ocekavany hodnoty lokalit okolo prumeru (tzn latituda + dalsi neznamy vlivy)
shade(meansPI,lat_seq)
#Jendotlivy kytky suma sumarum ze vsech lokalit
abline(lm(obsPI[1,]~lat_seq),lty=2)
abline(lm(obsPI[2,]~lat_seq),lty=2)

#kdyz budu chtit, muzu si na zaklade tech parametru proste nasamplovat nahodny lokace (tam nemam nejistotu kolem odhadu z, bacha) a nalosovat si treba tisic kytek na kazdy a tz sesumarizovat pro maximalni "multilevel feeling", ale do clanku bych to nedaval.

#Pro kazdy cislo z lat_seq vyrobim nahodnej dataset
lat_seq
rcol<-randomColor(length(lat_seq),luminosity = "bright")

for(i in 1:length(lat_seq)){
  #Select a random relative deviation from the mean estimate for a new location
  newz<-rnorm(1,0,1)
  uvpo<-rbeta2(n=length(post$a),
               p=inv_logit(post$a+post$b*lat_seq[i]+newz*post$sigma),
               theta=post$theta)
  #This controls relative with of the polygon
  dens<-density(uvpo)
  polygon(lat_seq[i]+c(dens$y,-1*rev(dens$y))*0.1,c(dens$x,rev(dens$x)),col=alpha(rcol[i],0.2),border=NA)
  
  #Means based on everything except for theta
  uvpi<-post$a+post$b*lat_seq[i]+newz*post$sigma
  lines(rep(lat_seq[i],2),inv_logit(PI(uvpi,prob=0.89)),col=rcol[i],lwd=2)
  points(lat_seq[i],inv_logit(mean(uvpi)),col=rcol[i],lwd=2,cex=1.2,pch=21,bg=0)
}

#Tohle je konkretni predikce ohledne toho, jak vypada soubor populaci mimo ty nasbirany.

#Napade me dobry rozsireni: Mozna by nebylo marny zkusit jako funkci latitudy modelovat i tu thetu, na to by to teda pak chtelo zavest tam korelaci mezi odchylkou od prumeru (z) a odchylkou od prumerny theta (treba w), to se dela hned v ty dalsi kapitole "adventures in covariance".

#Pozor, theta musi bejt kladna, takze ji tam taky hazu pres link funkci: pres log link (neni shora omezena jednickou), taky koukni na ten prior pro model thety

#predtim byla theta velika, takze ten intercept by mel dostat vic prostoru
exp(0.6)
exp(2)

dmvnorm(mean=c(a+b*lat[loc],at+bt*lat[loc]),sigma=c(sigmaA^2,sigmaA*sigmaT*Rho,sigmaA*sigmaT*Rho,sigmaT^2))

Rho~dbeta2(0.5,1)

#Tohle se bude vztekat, protoze stan numi vektor*vektor v likelihood funkci
m2 <- ulam(
  alist(
    uvp ~ dbeta2(p, theta),

    vector[330]:log(theta)<-at+bt*lat[loc]+vAvT[loc,2],
    vector[330]:logit(p)<-a+b*lat[loc]+vAvT[loc,1],
    
    transpars> matrix[loc,2]:vAvT<-compose_noncentered(sigma_AT,L_Rho,z_AT),
    
    matrix[2,loc]:z_AT ~ normal(0,1),
    vector[2]:sigma_AT ~ dexp(1),
    cholesky_factor_corr[2]:L_Rho ~ lkj_corr_cholesky( 2 ),
    
    a~dnorm(0,0.3),
    b~dnorm(0,0.2),
    
    at~dnorm(0,1),
    bt~dnorm(0,0.2),
    
    gq> matrix[2,2]:Rho <<- Chol_to_Corr(L_Rho)
    
  ), data=d,cores=4,chains=4,log_lik=T,sample=T)

stancode(m2)

#Co s tim? Stan nerozumi soucinu dvou vektoru! je potreba mu to "predzvejkat"

m2 <- ulam(
  alist(
    uvp ~ beta(alpha, beta),
    
    vector[330]:alpha<-p*theta,
    vector[330]:beta<-(1-p)*theta,
    
    vector[330]:log(theta)<-at+bt*lat[loc]+vAvT[loc,2],
    vector[330]:logit(p)<-a+b*lat[loc]+vAvT[loc,1],
    
    transpars> matrix[loc,2]:vAvT<-compose_noncentered(sigma_AT,L_Rho,z_AT),
    
    matrix[2,loc]:z_AT ~ normal(0,1),
    vector[2]:sigma_AT ~ dexp(1),
    cholesky_factor_corr[2]:L_Rho ~ lkj_corr_cholesky( 2 ),
    
    a~dnorm(0,0.3),
    b~dnorm(0,0.2),
    
    at~dnorm(0,1),
    bt~dnorm(0,0.2),
    
    gq> matrix[2,2]:Rho <<- Chol_to_Corr(L_Rho)
    
  ), data=d,cores=4,chains=4,log_lik=T,sample=T)

#save(m2,file="m2.Rdata")

#Ja jsem si to nasamploval velde na lepsim pocitaci, takze loaduju ten nasamplovanej model ze souboru
load("m2.Rdata")
stancode(m2)

#Zakladni srovnani koeficientu
precis(m1)
precis(m2)

#Taky si vytahnu posterior
post<-extract.samples(m2)
str(post)

#A sceknu ten slope prumeru
dens(post$b)
sum(post$b<0)/length(post$b)
PI(post$b, prob= 0.97)

#Slope thety je taky zajimavej - cim vetsi latituda (cim cin na sever, tim mensi theta (cili tim vetsi rozplizlost))
dens(post$bt)
sum(post$bt<0)/length(post$bt)
PI(post$bt, prob= 0.97)


#vizualizace
lat_seq<-seq(-11,11,0.2)

stancode(m2)


p.link <- function(lat) inv_logit(post$a + post$b*lat)
p<-sapply(lat_seq, p.link)

p.mean<-apply(p,2,mean)
p.PI<-apply(p,2,PI,prob=0.89)

plot(data$latitude-mean(locs),data$uvp, col=sapply(cols[data$locID],alpha,0.5),pch=16,xlab="latitude", xaxt="n",xlim=c(-10,10))
at <- c(-10,-5,0,5,10)
labels <- at+mean(data$latitude)
axis( side=1 , at=at , labels=round(labels) )

lines(lat_seq,p.mean)
shade(p.PI, lat_seq)

#Cara v grafu mi ted ukazuje prumer uvp v dane latitude + sebrane body ze 42 lokalit. Na zaklade "a" a "b", ktere popisuji vztah uvp s latitudou a parametru sigma (disperze lokalit okolo teto ocekavane hodnoty) si muzu plotnout koridor, kde ocekavam prumery ruznych lokalit s danou latitudou.
sim.locs<-sapply(lat_seq, function(lat){
  inv_logit(rnorm(n=length(post$a),
                  mean=post$a+post$b*lat,
                  sd=post$sigma_AT[1,]))})

meansPI<-apply(sim.locs,2,PI,prob=0.89)
shade(meansPI,lat_seq)

str(post$vAvT)
str(post$z_AT)

for(i in 1:42){
  uvpi<-post$a+post$b*d$lat[i]+post$vAvT[,i,1]
  lines(rep(d$lat[i],2),inv_logit(PI(uvpi,prob=0.89)),col=cols[i],lwd=2)
  points(d$lat[i],inv_logit(mean(uvpi)),col=cols[i],lwd=2,cex=1.2,pch=21,bg=0)
}

#Toho bordelu uz je tam hrozne moc... Asi to bude chtit pozdeji nejak procistit :D, ale doufam, ze ta multilevel struktura je takhle jasnejsi.

str(post)
#Posledni krok by byl nasamplovat z posetrioru prumeru konkretni pozorovani kytek (observations). Kolem kazdyho prumeru fiknu kytku na zaklade prislusnyho theta.
sim.obs<-sapply(lat_seq, function(lat){
  
  AT<-matrix(NA,ncol=2,nrow=length(post$a))
  
  for(i in 1:length(post$a)){
    AT[i,]<-rmvnorm(n=1,
                    mean=c(post$a[i]+post$b[i]*lat,post$at[i]+post$bt[i]*lat),
                    sigma=matrix(c(post$sigma_AT[i,1]^2,rep(post$sigma_AT[i,1]*post$sigma_AT[i,2]*post$Rho[i,1,2],2),post$sigma_AT[i,2]^2),nrow=2))
  }
  
  res<-rbeta2(n=length(post$a),
         p=inv_logit(AT[,1]),
         theta=exp(AT[,2]))
  return(res)
  })

obsPI<-apply(sim.obs,2,PI,prob=0.89)
#Aby tam nebyli porad ty cely bloky sedy barvy, da se to treba nakreslit jako prerusovana cara
lines(lat_seq,obsPI[1,],lty=2)
lines(lat_seq,obsPI[2,],lty=2)

# Toto bych pouzivat nemel kvuli log-link funkci, ktera predpoklada, ze ta cara nebude uplne rovna
# abline(lm(obsPI[2,]~lat_seq),lty=2)

#Jeste uplne nakonec si muzes s tou thetou nasamplovat hromadu predikovanejch pozorovani pro kazdou lokalitu a vykreslit to treba jako vycentrovanej violin plot.

str(post)


for(i in 1:42){
  uvpo<-rbeta2(n=length(post$a),
               p=inv_logit(post$a+post$b*d$lat[i]+post$vAvT[,i,1]),
               theta=exp(post$at+post$bt*d$lat[i]+post$vAvT[,i,2]))
  #This controls relative with of the polygon
  dens<-density(uvpo)
  polygon(d$lat[i]+c(dens$y,-1*rev(dens$y))*0.1,c(dens$x,rev(dens$x)),col=alpha(cols[i],0.2),border=NA)
}

#Pro prehlednost doporucuju flaknout ty sumarizace prumeru pres to:
for(i in 1:42){
  uvpi<-post$a+post$b*d$lat[i]+post$vAvT[,i,1]
  lines(rep(d$lat[i],2),inv_logit(PI(uvpi,prob=0.89)),col=cols[i],lwd=2)
  points(d$lat[i],inv_logit(mean(uvpi)),col=cols[i],lwd=2,cex=1.2,pch=21,bg=0)
}

#Aby teda bylo uplne jasny, co se deje, tyhle estimaty (kdyz se nepustime do odhadu z), jsou nezavisly na konkretnich lokalitach:

plot(data$latitude-mean(locs),data$uvp, col=sapply(cols[data$locID],alpha,0.5),pch=16,xlab="latitude",type="n", xaxt="n",xlim=c(-10,10))
at <- c(-10,-5,0,5,10)
labels <- at+mean(data$latitude)
axis( side=1 , at=at , labels=round(labels) )

#Model zavislosti prumeru lokalit na latitude
lines(lat_seq,p.mean)
shade(p.PI, lat_seq)
#Ocekavany hodnoty lokalit okolo prumeru (tzn latituda + dalsi neznamy vlivy)
shade(meansPI,lat_seq)
#Jendotlivy kytky suma sumarum ze vsech lokalit, loess (Local regression: http://r-statistics.co/Loess-Regression-With-R.html) pekne resi prokladani kostrbatosti hladkyma krivkama
lines(lat_seq,predict(loess(obsPI[1,]~lat_seq)),lty=2)
lines(lat_seq,predict(loess(obsPI[2,]~lat_seq)),lty=2)

#kdyz budu chtit, muzu si na zaklade tech parametru proste nasamplovat nahodny lokace (tam nemam nejistotu kolem odhadu z, bacha) a nalosovat si treba tisic kytek na kazdy a tz sesumarizovat pro maximalni "multilevel feeling", ale do clanku bych to nedaval.

#Pro kazdy cislo z lat_seq vyrobim nahodnej dataset
lat_seq
rcol<-randomColor(length(lat_seq),luminosity = "bright")

#Jak dostat ze standardizovanych promennych prokorelovane s prislusnymi variancemi? Staci se podivat, co dela Stan!
stancode(m2)
#(Predtim jsme taky losovali jen jedno z, takze musime postupovat takto - vygenerovat lokalite relativni odchylku od ocekaveneho prumeru a thety a pak ji "decentrovat" podle hodnot sigmaA, sigmaT a Rho prislusneho posteriorniho samplu)

#Tohle je jen na ukazku, ze to, co dela Stan funguje, jak ma
#2000 standardizovanych pozorovani
z_AT<-matrix(rnorm(2000,0,1),nrow=2)
vAvT<-(diag(post$sigma_AT[i,])%*%post$L_Rho[i,,])%*%z_AT
str(vAvT)

cor.test(vAvT[1,],vAvT[2,])
post$Rho[i,,]


for(l in 1:length(lat_seq)){
  
  lat<-lat_seq[l]
  
  #Select a random relative deviation from the mean estimate for a new location
  
  #Create a random location at given lattitude  
  z_AT<-matrix(rnorm(2,0,1),nrow=2)
  
  vAvT<-matrix(NA,ncol=2,nrow=length(post$a))  
  
  for(i in 1:length(post$a)){
    vAvT[i,]<-(diag(post$sigma_AT[i,])%*%post$L_Rho[i,,])%*%z_AT
  }
  
  p<-inv_logit(post$a+post$b*lat+vAvT[,1])
  theta<-exp(post$at+post$bt*lat+vAvT[,2])
  
  uvpo<-rbeta2(n=length(post$a),p=p,theta=theta)
  
  dens<-density(uvpo)
  polygon(lat+c(dens$y,-1*rev(dens$y))*0.1,c(dens$x,rev(dens$x)),col=alpha(rcol[l],0.2),border=NA)
  
  #Means based on everything except for theta
  uvpi<-post$a+post$b*lat+vAvT[,1]
  lines(rep(lat,2),inv_logit(PI(uvpi,prob=0.89)),col=rcol[l],lwd=2)
  points(lat,inv_logit(mean(uvpi)),col=rcol[l],lwd=2,cex=1.2,pch=21,bg=0)
}


compare(m1,m2)

str(post)

precis(m2,depth=3,pars = c("a","b","at","bt","sigma_AT","Rho[1,2]"))
pairs(m2,depth=3,pars = c("a","b","at","bt","sigma_AT","Rho[1,2]"))


stancode(m2)


