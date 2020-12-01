#Simple permutation test, comparison between two groups
hm<-rnorm(30,180,15)
hw<-rnorm(30,170,15)

heights<-c(hm,hw)
sex<-rep(c("M","W"),each=30)

ms<-tapply(heights,sex,mean)
observed<-ms[1]-ms[2]

#Statistics of interest is the difference between group means
observed

#Here starts a permutation test with a loop. Distribution of the estimator (statistics of interest) "if H0 was true" is generated.
#At each simulation run, heights are scrambled, so the situation corresponds to H0 - there is no link between height and sex. 
start<-Sys.time()
expected<-NA

for(i in 1:10000){
  msEh0<-tapply(sample(heights),sex,mean)
  expected[i]<-msEh0[1]-msEh0[2]
}
Sys.time()-start


#We can achive the same result with the replicate() function (a convenien wrapper for sapply), which is slightly faster 
start<-Sys.time()
oneperm<-function(i){
  msEh0<-tapply(sample(heights),sex,mean)
  return(msEh0[1]-msEh0[2])
}

expected<-replicate(10000,oneperm())
Sys.time()-start


#Anyway
#In permutation test the distribution of expected values is compared to the observed value of the estimator
#This compares them graphically
plot(density(expected))
abline(v=observed,col=2)

#This calculates p values of two one-sided statistical tests
ph<-sum(expected>observed)/10000
pl<-sum(expected<observed)/10000

#We are probably interested in the one-sided test on how much of teh expected "samples" exceeds the observed value
ph

#To get the p value of the two-sided test (caculating how many more extreme samples can be expected if H0 is true, all you have to do is multiply the lower p value by 2)
p<-min(ph,pl)*2

#This is your test results
observed
p

#If you are interested in standard errors, the best thing to do is to take standard deviation if the distribution of expected values. This is basically the standard error of the mean if H0 is true.
sd(expected)


#Another way to conduct a two-sided test is to work directlz with the distance between the means (i.e. the difference in the absolute value)

start<-Sys.time()
oneperm<-function(i){
  msEh0<-tapply(sample(heights),sex,mean)
  return(abs(msEh0[1]-msEh0[2]))
}

expected<-replicate(10000,oneperm())
Sys.time()-start

#You can see, that the distribution of the differences is now strictly positive. All permutations, where female average was above the male average got "mirrored" on the right side on the number axis
plot(density(expected))
abline(v=observed,col=2)

#Now the p value of the two-sided test is just the proportion expected values that exceed the observed estimate. In this context "higher" is the equivalent of "more extreme", since all numbers are >0
p<-sum(expected>observed)/10000
p


##Permuation test with higher number  of compared groups
#If zou build a puermutation test to compare more groups (e.g. compare body heights between men, women and non-binary), you have to cleraly formulate you objective.
#For example, you can consider all pairwise differences and construct 3 tests for three pairwise comparisons:


set.seed(50)
hm<-rnorm(30,180,15)
hw<-rnorm(30,170,15)
hn<-rnorm(30,175,18)

heights<-c(hm,hw,hn)
sex<-rep(c("M","W","N"),each=30)

ms<-tapply(heights,sex,mean)

#Matrix of distances from the function dist() is handy here
dist(ms)

observed<-c(dist(ms))
observed

runs<-10000
expected<-matrix(NA,ncol=3,nrow=runs)

set.seed(42)
for(i in 1:runs){
  msEh0<-tapply(sample(heights),sex,mean)
  expected[i,]<-c(dist(msEh0))
}

observed
str(expected)

p<-sapply(1:3,function(i){sum(expected[,i]>observed[i])/runs})

observed
p
#Only the pairwise difference between men and women reached statistical significance


#If you are intersted only in this single extreme difference, you can compare the maximum distance between group means with the distribution of maximum distances (no additional correction for multiple comparisons is necessary! The more groups you campare with each other, the more extreme maximum you are naturally bound to get.)

observed<-max(c(dist(ms)))
observed

runs<-10000
expected<-NA

for(i in 1:runs){
  msEh0<-tapply(sample(heights),sex,mean)
  expected[i]<-max(c(dist(msEh0)))
}

plot(density(expected))
abline(v=observed,col=2)

observed

(p<-sum(expected>observed)/runs)


#The last permutation test might compute the mean distance between two groups. This test would compare the diversity in group means in a single number. Another approach might include calculation of standard deviation of group means. These methotd are similar but not identical. (but in a permutation test, they woul likely converge on the same conclusion)

observed<-mean(c(dist(ms)))
observed

runs<-10000
expected<-NA

for(i in 1:runs){
  msEh0<-tapply(sample(heights),sex,mean)
  expected[i]<-mean(c(dist(msEh0)))
}

plot(density(expected))
abline(v=observed,col=2)

observed

(p<-sum(expected>observed)/runs)





