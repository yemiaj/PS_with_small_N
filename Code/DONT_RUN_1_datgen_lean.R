#Credits: Simulation structure, especially the parallelization aspects, follows Mark Warden's code for his AHE 2024 paper 
#(https://doi.org/10.1093/aje/kwae148)
#https://github.com/marknwarden/Statistical-approaches-for-the-integration-of-external-controls-in-CF-code.git

#Notes for next time:
#1) Justify choice of estimators
#2) Conditions:
#  a)Multicollinearity: find the approximate correlation that ensures a VIF of 2 and 4 and 6 [3 conditions]
#  b)Misspecification: missing variable of different magnitudes (of the betas) and incorrect functional form [3 conditions]
#  c)Contamination: linear predictor contamination of 3 proportions [3 conditions]

#Todo
#Check the possibility: Implement probability integral transform to generate correlated variables of different distributions in the exposure model
#should the contamination be applied to the linear predictor from the exposure model since the model is a multivariable one
#Check (theory, simulation, application): Constant adjustment to power for simpler extension of two-sample test to regression setting where R^2 increases, independent samples case



library(MASS)

datgen <- function(n.per.arm, rand.rat, corr.3, cont.prop, bsl.prev=0.4, var.cont=4){

# START: Input, preliminaries, settings, scenarios ####
  
  corr.mat <- cbind(c(1,corr.3),rbind(corr.3,diag(rep(1,3)))) #Creates a 4 by 4 correlation matrix, which has diagonals of 1
  cov.mat <- diag(diag(corr.mat)) %*% corr.mat %*% diag(diag(corr.mat)) #convert correlation matrix to covariance matrix
  
  #Dataset of effect sizes for XZ and YX
  if (!(n.per.arm %in% seq(30,110,20))) {stop("Only 'n.per.arm' (sample sizes) in the sequence: seq(30,110,20) is permitted, for now.")}
  
  n.delta12<-data.frame(N=seq(30,110,20), dxz=c(2.352,1.864,1.670,1.563,1.492), dxy=c(5.032,3.292,2.696,2.383,2.187))
  #https://cran.r-project.org/web/packages/pwrss/vignettes/examples.html#5_Poisson_Regression_(Wald%E2%80%99s_z_Test)
  
  delta.XZ<-log(n.delta12[n.delta12$N==n.per.arm,2]) #effect size for relationship between X and Z.
  delta.XY<-log(n.delta12[n.delta12$N==n.per.arm,3]) #effect size for relationship between X and Y.
  
  mu.null <- c(rep(0,4)) #Vector of 0 for MVN
  
  p.X <- rand.rat[1]/sum(rand.rat) #desired prevalence of treated to controls
  
  c2<-rbinom(n=n.per.arm*sum(rand.rat), size=1, prob=0.5) #Confounders of exposure or treatment assignment
  c3<-rnorm(n=n.per.arm*sum(rand.rat), mean=0, sd=1)
  
  w2<-rbinom(n=n.per.arm*sum(rand.rat), size=1, prob=0.5) #Confounders of outcome model
  w3<-rnorm(n=n.per.arm*sum(rand.rat), mean=0, sd=1)
# STOP: Input, preliminaries, settings, scenarios ####  
  
#START: Intermediate functions used in steps below ####
  inv.logit <- function(x) exp(x)/(1+exp(x)) #Inverse logit function to generate probabilities given any number on the real line
  logit <- function(x) log(x/(1-x)) #Logit function
#STOP: Intermediate functions used in steps below ####
  
  
# START: Generate data for ordinary, contamination, and collinearity scenarios####
  Z.ho <- mvrnorm(n=n.per.arm*sum(rand.rat), mu=mu.null, Sigma=cov.mat) #Obtain Z from a multivariate normal distribution; dim(Z) = Nrows and 4columns

  #Generate data for the propensity score (PS) model for each of the 3 scenarios
  #Here is the structure of the PS model: X~Z+c2+c3; where X is trt, Z is KEY predictor, and c2 and c3 are weakly associated confounders (the idea is to extend the effect of the confounders in future)
  
  
  #Ordinary scenario
    #Ordinary scenario: Under null
    Z.ho1 <- Z.ho[,1]
    eta.Z.ho <- logit(p.X)  + (log(1)*Z.ho1) + (0.001*c2) + (0.001*c3)  #linear predictor; note log(1) coefficient of Z.ho1 was used to represent null coefficient
    mu.Z.ho <-  inv.logit(eta.Z.ho)
    X.Z.ho <- rbinom(length(Z.ho1), 1, mu.Z.ho) #treatment assignment, 1=treated, 0=control with prevalence (i.e., % treated) equivalent to p.X
    #model=X.Z.ho~Z.ho1+c2+c3
    
    #Ordinary scenario: Under alternative
    eta.Z.ha <- logit(p.X) + (delta.XZ*Z.ho1) + (0.001*c2) + (0.001*c3) #linear predictor; note delta.XZ coefficient of Z.ho1 was used to represent alternative coefficient
    mu.Z.ha <-   inv.logit(eta.Z.ha)
    X.Z.ha <- rbinom(length(Z.ho1), 1, mu.Z.ha)
    #model=X.Z.ha~Z.ho1+c2+c3
  
  
  #Contamination scenario
    #Contamination scenario: Under null
    z0.1 <- Z.ho1
    z0.1[sample(length(Z.ho1), ceiling(length(Z.ho1)*cont.prop[1]))] <- rnorm(ceiling(length(Z.ho1)*cont.prop[1]), 0, 1) 
    z0.2 <- Z.ho1
    z0.2[sample(length(Z.ho1), ceiling(length(Z.ho1)*cont.prop[2]))] <- rnorm(ceiling(length(Z.ho1)*cont.prop[2]), 0, 1)
    z0.3 <- Z.ho1
    z0.3[sample(length(Z.ho1), ceiling(length(Z.ho1)*cont.prop[3]))] <- rnorm(ceiling(length(Z.ho1)*cont.prop[3]), 0, 1)
    #model=X.Z.ho~z0.1 (z0.2, z0.3)+c2+c3
    
    #Contamination scenario: Under alternative
    z0.1a <- Z.ho1
    z0.1a[sample(length(Z.ho1), ceiling(length(Z.ho1)*cont.prop[1]))] <- rnorm(ceiling(length(Z.ho1)*cont.prop[1]), 0, sqrt(var.cont))
    z0.2a <- Z.ho1
    z0.2a[sample(length(Z.ho1), ceiling(length(Z.ho1)*cont.prop[2]))] <- rnorm(ceiling(length(Z.ho1)*cont.prop[2]), 0, sqrt(var.cont))
    z0.3a <- Z.ho1
    z0.3a[sample(length(Z.ho1), ceiling(length(Z.ho1)*cont.prop[3]))] <- rnorm(ceiling(length(Z.ho1)*cont.prop[3]), 0, sqrt(var.cont))
    #model=X.Z.ha~z0.1a (z0.2a, z0.3a)+c2+c3
  
  
  #Collinearity scenario 
    #Collinearity scenario: Under null
    Zc.1 <- Z.ho[,2]
    Zc.2 <- Z.ho[,3]
    Zc.3 <- Z.ho[,4]
    #model=X.Z.ho~Zc.1 (Zc.2, Zc.3)+c2+c3
    
    #Collinearity scenario: Under alternative
    Zc.1a <- Zc.1
    Zc.2a <- Zc.2 
    Zc.3a <- Zc.3
    #model=X.Z.ha~Zc.1a (Zc.2a, Zc.3a)+c2+c3
  
    
  #Generate Y|X (under the alternative)
  #Relationship between X and Y, where Y is outcome, and w2 & w3 are weakly associated covariates (same logic as above, these w2 & w3 will be used later)
  eta.x <- logit(bsl.prev) + delta.XY*X.Z.ha  + (0.001*w2) + (0.001*w3)
  mu.x <-  inv.logit(eta.x)
  Y <- rbinom(length(Z.ho1), 1, mu.x)
  #model=Y~X.Z.ha+w2+w3
  
# STOP: Generate data ####
  
#Spit out final data
  outdat<-data.frame(id=1:length(Z.ho1), 
                     Z.ho1, 
                     X.Z.ho,
                     X.Z.ha,
                     c2, c3,
                     z0.1, z0.2, z0.3,
                     z0.1a, z0.2a, z0.3a,
                     Zc.1, Zc.2, Zc.3, 
                     Zc.1a, Zc.2a, Zc.3a,
                     w2, w3,
                     Y,
                     mat.rat=rep(rand.rat[2],length(Z.ho1))
                     )
  return(outdat)
}






#####################         #Rough work and misc codes below            ################################





####        Check the accuracy of parameters and input of the simulation program ####
#dat.tst<- datgen(n.per.arm=100,rand.rat=c(1,1), corr.3=c(.3,.4,.5), cont.prop=c(.1,.2,.3))

sim.check<-function(){
  sim.dat<-datgen(n.per.arm=70, rand.rat=c(1,4), corr.3=c(.3,.4,.5), cont.prop=c(.1,.2,.3))
  
  tx.null<-mean(sim.dat$X.Z.ho) #expected to be equal to rand.rat[1]/sum(rand.rat), 0.5 when c(1,1), 33% when c(1,2) etc
  tx.alt<-mean(sim.dat$X.Z.ha) #expected value is same as above
  resp.rate.trt <- mean(sim.dat$Y) #Response rate in the treated arm, expected is bsl.prev
  
  xz.beta<-as.numeric(with(sim.dat, glm(X.Z.ha~Z.ho1+c2+c3, family=binomial()))$coefficients[2]) #works as expected
  yx.beta<-as.numeric(with(sim.dat, glm(Y~X.Z.ha+w2+w3, family=binomial()))$coefficients[2])
  
  return(c(tx.null, #1, 0.5 when c(1,1)
           tx.alt,  #2, 0.5 when c(1,1)
           resp.rate.trt,
           xz.beta,
           yx.beta)) #5, 100%-40% = 60%
}

sim.check()
n.sim.chk<-50000
sim.check.save<-matrix(NA,nrow=n.sim.chk, ncol=length(sim.check()))
m<-1
#set.seed(2)
for (m in 1:n.sim.chk) sim.check.save[m,]<-sim.check()
summary(sim.check.save)



#model=X.Z.ha~Z.ho1




#Stratified analysis
#https://rstudio-pubs-static.s3.amazonaws.com/3473_aec63a6fb6f7437faf2f583233716b92.html
#Learn more
mod4<-glm(X.Z.ha~Z.ho1+Zc.1a+Zc.2a+Zc.3a, data=dat.tst, family=binomial())
dat.tst$pscore<-fitted(mod4)
quantile(dat.tst$pscore, prob=seq(0,1,.2), na.rm=TRUE)
dat.tst$pscore.cat<-cut(dat.tst$pscore, breaks=quantile(dat.tst$pscore, prob=seq(0,1,.2), na.rm=TRUE), labels=1:5, include.lowest=TRUE )

library(plyr)
logistic.stratified <- dlply(.data = dat.tst, .variables = "pscore.cat",
                             .fun = function(DF) {
                               glm(Y ~ X.Z.ha, data = DF, family = binomial)
                             })
## Get OR in each stratum
res.strata.logit <- lapply(logistic.stratified[1:5], function(X){
  #summary(X)$coefficients[2,]
  confint(X)[2,]
})
res.strata.logit


print(do.call(rbind, res.strata.logit), quote = F)
