# START: Input, preliminaries, settings, scenarios ####
corr.mat <- cbind(c(1,c(.3,.4,.5)),rbind(c(.3,.4,.5),diag(rep(1,3))))
corr.mat
dimnames(corr.mat)[[1]]<-NULL
cov.mat <- diag(diag(corr.mat)) %*% corr.mat %*% diag(diag(corr.mat)) #convert correlation matrix to covariance matrix
cov.mat

Z.ho <- mvrnorm(n=50*sum(c(1,1)), mu=c(rep(0,4)), Sigma=cov.mat)
p.X <- rand.rat[1]/sum(rand.rat)
#Null
Z.ho1 <- Z.ho[,1]




sim.check<-function(){
  sim.dat<-datgen(n.delta12=c(50,1.864,3.292), rand.rat=c(1,1), corr.3=c(.3,.4,.5), cont.prop=c(.1,.2,.3))
  
  tx.null<-mean(sim.dat$X.Z.ho) #expected to be equal to rand.rat[1]/sum(rand.rat), 0.5 when c(1,1), 33% when c(1,2) etc
  tx.alt<-mean(sim.dat$X.Z.ha) #expected value is same as above
  resp.rate.trt <- mean(sim.dat$Y) #Response rate in the treated arm, expected is 
  xz.beta<-as.numeric(with(sim.dat, glm(X.Z.ha~Z.ho1, family=binomial()))$coefficients[2])
  yx.beta<-as.numeric(with(sim.dat, glm(Y~X.Z.ha, family=binomial()))$coefficients[2])
  
  return(c(tx.null, #1, 0.5 when c(1,1)
           tx.alt,  #2, 0.5 when c(1,1)
           resp.rate.trt,
           xz.beta,
           yx.beta)) #5, 100%-40% = 60%
}





#revamp
inv.logit <- function(x) exp(x)/(1+exp(x))
logit <- function(x) log(x/(1-x))

c1<-rbinom(n=100, size=1, prob=0.5)
c2<-rbinom(n=100, size=1, prob=0.5)
c3<-rnorm(n=100, mean=0, sd=1)

x.eta.ho <- logit(1/sum(c(1,1))) + (log(1)*c1) + (0.001*c2) + (0.001*c3)
mu.x.eta.ho <- inv.logit(x.eta.ho)
x.ho <- rbinom(n=100, size=1, prob=mu.x.eta.ho)
mean(x.ho)

summary(glm(x.ho ~ c1+c2+c3, family=binomial()))

#Check appropriateness
#test power





c2<-rbinom(n=100, size=1, prob=0.5)
c3<-rnorm(n=100, mean=0, sd=1)
c(mean(c2), mean(c3))


w2<-rbinom(n=100, size=1, prob=0.5)
w3<-rnorm(n=100, mean=0, sd=1)
c(mean(w2), mean(w3))









