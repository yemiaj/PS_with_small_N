library(MatchIt)
library(survival)
library(survey)
library(AIPW)
library(SuperLearner)
library(magrittr)

#Binomial SuperLearner for AIPW
#Adapted from SuperLearner::SL.glm
SL.glm.binom <- function (Y, X, newX, family=binomial(), obsWeights, model = TRUE, ...) {
  if (is.matrix(X)) {
    X = as.data.frame(X)
  }
  fit.glm <- glm(Y ~ ., data = X, family = family, weights = obsWeights, model = model)
  if (is.matrix(newX)) {
    newX = as.data.frame(newX)
  }
  pred <- predict(fit.glm, newdata = newX, type = "response")
  fit <- list(object = fit.glm)
  class(fit) <- "SL.glm"
  out <- list(pred = pred, fit = fit)
  return(out)
}

getests<-function(data){

#Start Null models, X~Z
    #Ordinary
    ord<-glm(X.Z.ho~Z.ho1,family=binomial(),data=data) 
    ord.s<-summary(ord)
    outmat <- setNames((ord.s %>% coefficients)[2,],c('xz.null.ord.beta','xz.null.ord.se','xz.null.ord.Z','xz.null.ord.pv'))
    outmat %<>% c(setNames((ord %>% confint)[2,],c('xz.null.ord.LL','xz.null.ord.UL')));rm(ord); rm(ord.s) 
  
    #Contamination
    con<-glm(X.Z.ho~z0.1,family=binomial(),data=data) 
    con.s<-summary(con)
    outmat %<>% c(setNames((con.s %>% coefficients)[2,],c('xz.null.con1.beta','xz.null.con1.se','xz.null.con1.Z','xz.null.con1.pv')))
    outmat %<>% c(setNames((con %>% confint)[2,],c('xz.null.con1.LL','xz.null.con1.UL')));rm(con); rm(con.s) 
    
    con<-glm(X.Z.ho~z0.2,family=binomial(),data=data) 
    con.s<-summary(con)
    outmat %<>% c(setNames((con.s %>% coefficients)[2,],c('xz.null.con2.beta','xz.null.con2.se','xz.null.con2.Z','xz.null.con2.pv')))
    outmat %<>% c(setNames((con %>% confint)[2,],c('xz.null.con2.LL','xz.null.con2.UL')));rm(con); rm(con.s) 
    
    con<-glm(X.Z.ho~z0.3,family=binomial(),data=data) 
    con.s<-summary(con)
    outmat %<>% c(setNames((con.s %>% coefficients)[2,],c('xz.null.con3.beta','xz.null.con3.se','xz.null.con3.Z','xz.null.con3.pv')))
    outmat %<>% c(setNames((con %>% confint)[2,],c('xz.null.con3.LL','xz.null.con3.UL')));rm(con); rm(con.s) 
    
    #Collinearity 
    col<-glm(X.Z.ho~Z.ho1+Zc.1,family=binomial(),data=data) 
    col.s<-summary(col)
    outmat %<>% c(setNames((col.s %>% coefficients)[2,],c('xz.null.col1.beta','xz.null.col1.se','xz.null.col1.Z','xz.null.col1.pv')))
    outmat %<>% c(setNames((col %>% confint)[2,],c('xz.null.col1.LL','xz.null.col1.UL')));rm(col); rm(col.s)
    
    col<-glm(X.Z.ho~Z.ho1+Zc.1+Zc.2,family=binomial(),data=data) 
    col.s<-summary(col)
    outmat %<>% c(setNames((col.s %>% coefficients)[2,],c('xz.null.col2.beta','xz.null.col2.se','xz.null.col2.Z','xz.null.col2.pv')))
    outmat %<>% c(setNames((col %>% confint)[2,],c('xz.null.col2.LL','xz.null.col2.UL')));rm(col); rm(col.s)   
    
    col<-glm(X.Z.ho~Z.ho1+Zc.1+Zc.2+Zc.3,family=binomial(),data=data) 
    col.s<-summary(col)
    outmat %<>% c(setNames((col.s %>% coefficients)[2,],c('xz.null.col3.beta','xz.null.col3.se','xz.null.col3.Z','xz.null.col3.pv')))
    outmat %<>% c(setNames((col %>% confint)[2,],c('xz.null.col3.LL','xz.null.col3.UL')));rm(col); rm(col.s)   
#End Null models, X~Z
    
#Alternative models (performs test under alternative, estimates propensity scores, performs conditional logistic, & IPTW weighted analysis)  
  #Applies to XZ model and then YX model for each of the 3 estimators 
    
  #Ordinary
    ord<-glm(X.Z.ha~Z.ho1,family=binomial(),data=data) 
    ord.s<-summary(ord)
    outmat %<>% c(setNames((ord.s %>% coefficients)[2,],c('xz.alt.ord.beta','xz.alt.ord.se','xz.alt.ord.Z','xz.alt.ord.pv')))
    outmat %<>% c(setNames((ord %>% confint)[2,],c('xz.alt.ord.LL','xz.alt.ord.UL')));rm(ord); rm(ord.s)     
 
    data1<-data
      #obtain propensity score and match pairs
      data1<-match.data( matchit(X.Z.ha~Z.ho1, data=data, method='optimal',distance='glm', ratio=data1[1,'mat.rat']), 
                         data=data, distance="prop.score", weights = "wts", subclass = "subcls", drop.unmatched=FALSE )
      data1$iptw<-with(data1, ifelse(X.Z.ha==1,1/prop.score,1/(1-prop.score)))
    
    #Adjusted analysis
    ord<-glm(Y~X.Z.ha+prop.score,family=binomial(),data=data1) 
    ord.s<-summary(ord)
    outmat %<>% c(setNames((ord.s %>% coefficients)[2,],c('yx.alt.ord.adj.beta','yx.alt.ord.adj.se','yx.alt.ord.adj.t','yx.alt.ord.adj.pv')))
    outmat %<>% c(setNames((ord %>% confint)[2,],c('yx.alt.ord.adj.LL','yx.alt.ord.adj.UL')));rm(ord); rm(ord.s)       
      
    #Conditional logistic
    ord<-clogit(Y~X.Z.ha+strata(subcls),data=data1) 
    ord.s<-summary(ord)
    outmat %<>% c(setNames((ord.s %>% coefficients)[1,c(1,3:5)],c('yx.alt.ord.cond.beta','yx.alt.ord.cond.se','yx.alt.ord.cond.Z','yx.alt.ord.cond.pv')))
    outmat %<>% c(setNames((ord %>% confint)[1,],c('yx.alt.ord.cond.LL','yx.alt.ord.cond.UL')));rm(ord); rm(ord.s)      
    
    #IPTW logistic regression
#    ord<-glm(Y~X.Z.ha,family=binomial(),weights=iptw,data=data1) 
#    ord.s<-summary(ord)
    ord<-svyglm(Y~X.Z.ha,design=svydesign(ids=~1,weights=~iptw,data=data1),family=quasibinomial()) 
    ord.s<-summary(ord) 
    outmat %<>% c(setNames((ord.s %>% coefficients)[2,],c('yx.alt.ord.iptw.beta','yx.alt.ord.iptw.se','yx.alt.ord.iptw.t','yx.alt.ord.iptw.pv')))
    outmat %<>% c(setNames((ord %>% confint)[2,],c('yx.alt.ord.iptw.LL','yx.alt.ord.iptw.UL')));rm(ord); rm(ord.s)   
    
    rm(data1)
    
    #AIPW
    ord <- with(data, AIPW$new(Y=Y, A=X.Z.ha, W=Z.ho1, g.SL.library = "SL.glm.binom", Q.SL.library = "SL.glm.binom", k_split = 1)$fit())
    ord.s <- ord$summary(g.bound=0.0)
    outmat %<>% c(setNames(c(log(ord.s$estimates$OR[1]), ord.s$estimates$OR[2], NA, NA, log(ord.s$estimates$OR[3]), log(ord.s$estimates$OR[4])),
                           c('yx.alt.ord.aipw.beta','yx.alt.ord.aipw.se','yx.alt.ord.aipw.Z','yx.alt.ord.aipw.pv','yx.alt.ord.aipw.LL','yx.alt.ord.aipw.UL')))
    rm(ord); rm(ord.s)

  #Contamination  [1]    
    con<-glm(X.Z.ha~z0.1a,family=binomial(),data=data) 
    con.s<-summary(con)
    outmat %<>% c(setNames((con.s %>% coefficients)[2,],c('xz.alt.con1.beta','xz.alt.con1.se','xz.alt.con1.Z','xz.alt.con1.pv')))
    outmat %<>% c(setNames((con %>% confint)[2,],c('xz.alt.con1.LL','xz.alt.con1.UL')));rm(con); rm(con.s)     
    
    data1<-data
      #obtain propensity score and match pairs
      data1<-match.data( matchit(X.Z.ha~z0.1a, data=data, method='optimal',distance='glm', ratio=data1[1,'mat.rat']), 
                         data=data, distance="prop.score", weights = "wts", subclass = "subcls", drop.unmatched=FALSE )
      data1$iptw<-with(data1, ifelse(X.Z.ha==1,1/prop.score,1/(1-prop.score)))
    
    #Adjusted analysis
    con<-glm(Y~X.Z.ha+prop.score,family=binomial(),data=data1) 
    con.s<-summary(con)
    outmat %<>% c(setNames((con.s %>% coefficients)[2,],c('yx.alt.con1.adj.beta','yx.alt.con1.adj.se','yx.alt.con1.adj.t','yx.alt.con1.adj.pv')))
    outmat %<>% c(setNames((con %>% confint)[2,],c('yx.alt.con1.adj.LL','yx.alt.con1.adj.UL')));rm(con); rm(con.s)       
    
    #Conditional logistic
    con<-clogit(Y~X.Z.ha+strata(subcls),data=data1) 
    con.s<-summary(con)
    outmat %<>% c(setNames((con.s %>% coefficients)[1,c(1,3:5)],c('yx.alt.con1.cond.beta','yx.alt.con1.cond.se','yx.alt.con1.cond.Z','yx.alt.con1.cond.pv')))
    outmat %<>% c(setNames((con %>% confint)[1,],c('yx.alt.con1.cond.LL','yx.alt.con1.cond.UL')));rm(con); rm(con.s)        
    
    #IPTW logistic regression
    #con<-glm(Y~X.Z.ha,family=binomial(),weights=iptw,data=data1) 
    con<-svyglm(Y~X.Z.ha,design=svydesign(ids=~1,weights=~iptw,data=data1),family=quasibinomial())
    con.s<-summary(con)
    outmat %<>% c(setNames((con.s %>% coefficients)[2,],c('yx.alt.con1.iptw.beta','yx.alt.con1.iptw.se','yx.alt.con1.iptw.t','yx.alt.con1.iptw.pv')))
    outmat %<>% c(setNames((con %>% confint)[2,],c('yx.alt.con1.iptw.LL','yx.alt.con1.iptw.UL')));rm(con); rm(con.s)   
    
    rm(data1)
    
    #AIPW
    con <- with(data, AIPW$new(Y=Y, A=X.Z.ha, W=z0.1a, g.SL.library = "SL.glm.binom", Q.SL.library = "SL.glm.binom", k_split = 1)$fit())
    con.s <- con$summary(g.bound=0.0)
    outmat %<>% c(setNames(c(log(con.s$estimates$OR[1]), con.s$estimates$OR[2], NA, NA, log(con.s$estimates$OR[3]), log(con.s$estimates$OR[4])),
                           c('yx.alt.con1.aipw.beta','yx.alt.con1.aipw.se','yx.alt.con1.aipw.Z','yx.alt.con1.aipw.pv','yx.alt.con1.aipw.LL','yx.alt.con1.aipw.UL')))
    rm(con); rm(con.s)
    
  #Contamination  [2]    
    con<-glm(X.Z.ha~z0.2a,family=binomial(),data=data) 
    con.s<-summary(con)
    outmat %<>% c(setNames((con.s %>% coefficients)[2,],c('xz.alt.con2.beta','xz.alt.con2.se','xz.alt.con2.Z','xz.alt.con2.pv')))
    outmat %<>% c(setNames((con %>% confint)[2,],c('xz.alt.con2.LL','xz.alt.con2.UL')));rm(con); rm(con.s)     
    
    data1<-data
    #obtain propensity score and match pairs
      data1<-match.data( matchit(X.Z.ha~z0.2a, data=data, method='optimal',distance='glm', ratio=data1[1,'mat.rat']), 
                         data=data, distance="prop.score", weights = "wts", subclass = "subcls", drop.unmatched=FALSE )
      data1$iptw<-with(data1, ifelse(X.Z.ha==1,1/prop.score,1/(1-prop.score)))    
    
    #Adjusted analysis
    con<-glm(Y~X.Z.ha+prop.score,family=binomial(),data=data1) 
    con.s<-summary(con)
    outmat %<>% c(setNames((con.s %>% coefficients)[2,],c('yx.alt.con2.adj.beta','yx.alt.con2.adj.se','yx.alt.con2.adj.t','yx.alt.con2.adj.pv')))
    outmat %<>% c(setNames((con %>% confint)[2,],c('yx.alt.con2.adj.LL','yx.alt.con2.adj.UL')));rm(con); rm(con.s)       
    
    #Conditional logistic
    con<-clogit(Y~X.Z.ha+strata(subcls),data=data1) 
    con.s<-summary(con)
    outmat %<>% c(setNames((con.s %>% coefficients)[1,c(1,3:5)],c('yx.alt.con2.cond.beta','yx.alt.con2.cond.se','yx.alt.con2.cond.Z','yx.alt.con2.cond.pv')))
    outmat %<>% c(setNames((con %>% confint)[1,],c('yx.alt.con2.cond.LL','yx.alt.con2.cond.UL')));rm(con); rm(con.s)        
    
    #IPTW logistic regression
    #con<-glm(Y~X.Z.ha,family=binomial(),weights=iptw,data=data1) 
    con<-svyglm(Y~X.Z.ha,design=svydesign(ids=~1,weights=~iptw,data=data1),family=quasibinomial())
    con.s<-summary(con)
    outmat %<>% c(setNames((con.s %>% coefficients)[2,],c('yx.alt.con2.iptw.beta','yx.alt.con2.iptw.se','yx.alt.con2.iptw.t','yx.alt.con2.iptw.pv')))
    outmat %<>% c(setNames((con %>% confint)[2,],c('yx.alt.con2.iptw.LL','yx.alt.con2.iptw.UL')));rm(con); rm(con.s)   
    
    rm(data1)
    
    #AIPW
    con <- with(data, AIPW$new(Y=Y, A=X.Z.ha, W=z0.2a, g.SL.library = "SL.glm.binom", Q.SL.library = "SL.glm.binom", k_split = 1)$fit())
    con.s <- con$summary(g.bound=0.0)
    outmat %<>% c(setNames(c(log(con.s$estimates$OR[1]), con.s$estimates$OR[2], NA, NA, log(con.s$estimates$OR[3]), log(con.s$estimates$OR[4])),
                           c('yx.alt.con2.aipw.beta','yx.alt.con2.aipw.se','yx.alt.con2.aipw.Z','yx.alt.con2.aipw.pv','yx.alt.con2.aipw.LL','yx.alt.con2.aipw.UL')))
    rm(con); rm(con.s)
    
  #Contamination  [3]    
    con<-glm(X.Z.ha~z0.3a,family=binomial(),data=data) 
    con.s<-summary(con)
    outmat %<>% c(setNames((con.s %>% coefficients)[2,],c('xz.alt.con3.beta','xz.alt.con3.se','xz.alt.con3.Z','xz.alt.con3.pv')))
    outmat %<>% c(setNames((con %>% confint)[2,],c('xz.alt.con3.LL','xz.alt.con3.UL')));rm(con); rm(con.s)     
    
    data1<-data
      #obtain propensity score and match pairs
      data1<-match.data( matchit(X.Z.ha~z0.3a, data=data, method='optimal',distance='glm', ratio=data1[1,'mat.rat']), 
                         data=data, distance="prop.score", weights = "wts", subclass = "subcls", drop.unmatched=FALSE )
      data1$iptw<-with(data1, ifelse(X.Z.ha==1,1/prop.score,1/(1-prop.score)))    
    
    #Adjusted analysis
    con<-glm(Y~X.Z.ha+prop.score,family=binomial(),data=data1) 
    con.s<-summary(con)
    outmat %<>% c(setNames((con.s %>% coefficients)[2,],c('yx.alt.con3.adj.beta','yx.alt.con3.adj.se','yx.alt.con3.adj.t','yx.alt.con3.adj.pv')))
    outmat %<>% c(setNames((con %>% confint)[2,],c('yx.alt.con3.adj.LL','yx.alt.con3.adj.UL')));rm(con); rm(con.s)       
    
    #Conditional logistic
    con<-clogit(Y~X.Z.ha+strata(subcls),data=data1) 
    con.s<-summary(con)
    outmat %<>% c(setNames((con.s %>% coefficients)[1,c(1,3:5)],c('yx.alt.con3.cond.beta','yx.alt.con3.cond.se','yx.alt.con3.cond.Z','yx.alt.con3.cond.pv')))
    outmat %<>% c(setNames((con %>% confint)[1,],c('yx.alt.con3.cond.LL','yx.alt.con3.cond.UL')));rm(con); rm(con.s)        
    
    #IPTW logistic regression
    #con<-glm(Y~X.Z.ha,family=binomial(),weights=iptw,data=data1) 
    con<-svyglm(Y~X.Z.ha,design=svydesign(ids=~1,weights=~iptw,data=data1),family=quasibinomial())
    con.s<-summary(con)
    outmat %<>% c(setNames((con.s %>% coefficients)[2,],c('yx.alt.con3.iptw.beta','yx.alt.con3.iptw.se','yx.alt.con3.iptw.t','yx.alt.con3.iptw.pv')))
    outmat %<>% c(setNames((con %>% confint)[2,],c('yx.alt.con3.iptw.LL','yx.alt.con3.iptw.UL')));rm(con); rm(con.s)   
    
    rm(data1)
    
    #AIPW
    con <- with(data, AIPW$new(Y=Y, A=X.Z.ha, W=z0.3a, g.SL.library = "SL.glm.binom", Q.SL.library = "SL.glm.binom", k_split = 1)$fit())
    con.s <- con$summary(g.bound=0.0)
    outmat %<>% c(setNames(c(log(con.s$estimates$OR[1]), con.s$estimates$OR[2], NA, NA, log(con.s$estimates$OR[3]), log(con.s$estimates$OR[4])),
                           c('yx.alt.con3.aipw.beta','yx.alt.con3.aipw.se','yx.alt.con3.aipw.Z','yx.alt.con3.aipw.pv','yx.alt.con3.aipw.LL','yx.alt.con3.aipw.UL')))
    rm(con); rm(con.s)
    
  #Collinearity [1]
    col<-glm(X.Z.ha~Z.ho1+Zc.1a,family=binomial(),data=data) 
    col.s<-summary(col)
    outmat %<>% c(setNames((col.s %>% coefficients)[2,],c('xz.alt.col1.beta','xz.alt.col1.se','xz.alt.col1.Z','xz.alt.col1.pv')))
    outmat %<>% c(setNames((col %>% confint)[2,],c('xz.alt.col1.LL','xz.alt.col1.UL')));rm(col); rm(col.s)     
    
    data1<-data
    #obtain propensity score and match pairs
      data1<-match.data( matchit(X.Z.ha~Z.ho1+Zc.1a, data=data, method='optimal',distance='glm', ratio=data1[1,'mat.rat']), 
                         data=data, distance="prop.score", weights = "wts", subclass = "subcls", drop.unmatched=FALSE )
      data1$iptw<-with(data1, ifelse(X.Z.ha==1,1/prop.score,1/(1-prop.score)))     
    
    #Adjusted analysis
    col<-glm(Y~X.Z.ha+prop.score,family=binomial(),data=data1) 
    col.s<-summary(col)
    outmat %<>% c(setNames((col.s %>% coefficients)[2,],c('yx.alt.col1.adj.beta','yx.alt.col1.adj.se','yx.alt.col1.adj.t','yx.alt.col1.adj.pv')))
    outmat %<>% c(setNames((col %>% confint)[2,],c('yx.alt.col1.adj.LL','yx.alt.col1.adj.UL')));rm(col); rm(col.s)       
    
    #Conditional logistic
    col<-clogit(Y~X.Z.ha+strata(subcls),data=data1) 
    col.s<-summary(col)
    outmat %<>% c(setNames((col.s %>% coefficients)[1,c(1,3:5)],c('yx.alt.col1.cond.beta','yx.alt.col1.cond.se','yx.alt.col1.cond.Z','yx.alt.col1.cond.pv')))
    outmat %<>% c(setNames((col %>% confint)[1,],c('yx.alt.col1.cond.LL','yx.alt.col1.cond.UL')));rm(col); rm(col.s)        
      
    #IPTW logistic regression
    #col<-glm(Y~X.Z.ha,family=binomial(),weights=iptw,data=data1)
    col<-svyglm(Y~X.Z.ha,design=svydesign(ids=~1,weights=~iptw,data=data1),family=quasibinomial())
    col.s<-summary(col)
    outmat %<>% c(setNames((col.s %>% coefficients)[2,],c('yx.alt.col1.iptw.beta','yx.alt.col1.iptw.se','yx.alt.col1.iptw.t','yx.alt.col1.iptw.pv')))
    outmat %<>% c(setNames((col %>% confint)[2,],c('yx.alt.col1.iptw.LL','yx.alt.col1.iptw.UL')));rm(col); rm(col.s)   
    
    rm(data1)
    
    #AIPW
    col <- with(data, AIPW$new(Y=Y, A=X.Z.ha, W=Z.ho1+Zc.1a, g.SL.library = "SL.glm.binom", Q.SL.library = "SL.glm.binom", k_split = 1)$fit())
    col.s <- col$summary(g.bound=0.0)
    outmat %<>% c(setNames(c(log(col.s$estimates$OR[1]), col.s$estimates$OR[2], NA, NA, log(col.s$estimates$OR[3]), log(col.s$estimates$OR[4])),
                           c('yx.alt.col1.aipw.beta','yx.alt.col1.aipw.se','yx.alt.col1.aipw.Z','yx.alt.col1.aipw.pv','yx.alt.col1.aipw.LL','yx.alt.col1.aipw.UL')))
    rm(col); rm(col.s)
  
  #Collinearity [2]
    col<-glm(X.Z.ha~Z.ho1+Zc.1a+Zc.2a,family=binomial(),data=data) 
    col.s<-summary(col)
    outmat %<>% c(setNames((col.s %>% coefficients)[2,],c('xz.alt.col2.beta','xz.alt.col2.se','xz.alt.col2.Z','xz.alt.col2.pv')))
    outmat %<>% c(setNames((col %>% confint)[2,],c('xz.alt.col2.LL','xz.alt.col2.UL')));rm(col); rm(col.s)     
    
    data1<-data
    #obtain propensity score and match pairs
      data1<-match.data( matchit(X.Z.ha~Z.ho1+Zc.1a+Zc.2a, data=data, method='optimal',distance='glm', ratio=data1[1,'mat.rat']), 
                         data=data, distance="prop.score", weights = "wts", subclass = "subcls", drop.unmatched=FALSE )
      data1$iptw<-with(data1, ifelse(X.Z.ha==1,1/prop.score,1/(1-prop.score)))     
    
    #Adjusted analysis
    col<-glm(Y~X.Z.ha+prop.score,family=binomial(),data=data1) 
    col.s<-summary(col)
    outmat %<>% c(setNames((col.s %>% coefficients)[2,],c('yx.alt.col2.adj.beta','yx.alt.col2.adj.se','yx.alt.col2.adj.t','yx.alt.col2.adj.pv')))
    outmat %<>% c(setNames((col %>% confint)[2,],c('yx.alt.col2.adj.LL','yx.alt.col2.adj.UL')));rm(col); rm(col.s)       
    
    #Conditional logistic
    col<-clogit(Y~X.Z.ha+strata(subcls),data=data1) 
    col.s<-summary(col)
    outmat %<>% c(setNames((col.s %>% coefficients)[1,c(1,3:5)],c('yx.alt.col2.cond.beta','yx.alt.col2.cond.se','yx.alt.col2.cond.Z','yx.alt.col2.cond.pv')))
    outmat %<>% c(setNames((col %>% confint)[1,],c('yx.alt.col2.cond.LL','yx.alt.col2.cond.UL')));rm(col); rm(col.s)        
    
    #IPTW logistic regression
    #col<-glm(Y~X.Z.ha,family=binomial(),weights=iptw,data=data1) 
    col<-svyglm(Y~X.Z.ha,design=svydesign(ids=~1,weights=~iptw,data=data1),family=quasibinomial())
    col.s<-summary(col)
    outmat %<>% c(setNames((col.s %>% coefficients)[2,],c('yx.alt.col2.iptw.beta','yx.alt.col2.iptw.se','yx.alt.col2.iptw.t','yx.alt.col2.iptw.pv')))
    outmat %<>% c(setNames((col %>% confint)[2,],c('yx.alt.col2.iptw.LL','yx.alt.col2.iptw.UL')));rm(col); rm(col.s)   
    
    rm(data1)
    
    #AIPW
    col <- with(data, AIPW$new(Y=Y, A=X.Z.ha, W=Z.ho1+Zc.1a+Zc.2a, g.SL.library = "SL.glm.binom", Q.SL.library = "SL.glm.binom", k_split = 1)$fit())
    col.s <- col$summary(g.bound=0.0)
    outmat %<>% c(setNames(c(log(col.s$estimates$OR[1]), col.s$estimates$OR[2], NA, NA, log(col.s$estimates$OR[3]), log(col.s$estimates$OR[4])),
                           c('yx.alt.col2.aipw.beta','yx.alt.col2.aipw.se','yx.alt.col2.aipw.Z','yx.alt.col2.aipw.pv','yx.alt.col2.aipw.LL','yx.alt.col2.aipw.UL')))
    rm(col); rm(col.s)
    
  #Collinearity [3]
    col<-glm(X.Z.ha~Z.ho1+Zc.1a+Zc.2a+Zc.3a,family=binomial(),data=data) 
    col.s<-summary(col)
    outmat %<>% c(setNames((col.s %>% coefficients)[2,],c('xz.alt.col3.beta','xz.alt.col3.se','xz.alt.col3.Z','xz.alt.col3.pv')))
    outmat %<>% c(setNames((col %>% confint)[2,],c('xz.alt.col3.LL','xz.alt.col3.UL')));rm(col); rm(col.s)     
    
    data1<-data
    #obtain propensity score and match pairs
      data1<-match.data( matchit(X.Z.ha~Z.ho1+Zc.1a+Zc.2a+Zc.3a, data=data, method='optimal',distance='glm', ratio=data1[1,'mat.rat']), 
                         data=data, distance="prop.score", weights = "wts", subclass = "subcls", drop.unmatched=FALSE )
      data1$iptw<-with(data1, ifelse(X.Z.ha==1,1/prop.score,1/(1-prop.score)))     
    
    #Adjusted analysis
    col<-glm(Y~X.Z.ha+prop.score,family=binomial(),data=data1) 
    col.s<-summary(col)
    outmat %<>% c(setNames((col.s %>% coefficients)[2,],c('yx.alt.col3.adj.beta','yx.alt.col3.adj.se','yx.alt.col3.adj.t','yx.alt.col3.adj.pv')))
    outmat %<>% c(setNames((col %>% confint)[2,],c('yx.alt.col3.adj.LL','yx.alt.col3.adj.UL')));rm(col); rm(col.s)       
    
    #Conditional logistic
    col<-clogit(Y~X.Z.ha+strata(subcls),data=data1) 
    col.s<-summary(col)
    outmat %<>% c(setNames((col.s %>% coefficients)[1,c(1,3:5)],c('yx.alt.col3.cond.beta','yx.alt.col3.cond.se','yx.alt.col3.cond.Z','yx.alt.col3.cond.pv')))
    outmat %<>% c(setNames((col %>% confint)[1,],c('yx.alt.col3.cond.LL','yx.alt.col3.cond.UL')));rm(col); rm(col.s)        
    
    #IPTW logistic regression
    #col<-glm(Y~X.Z.ha,family=binomial(),weights=iptw,data=data1)
    col<-svyglm(Y~X.Z.ha,design=svydesign(ids=~1,weights=~iptw,data=data1),family=quasibinomial())
    col.s<-summary(col)
    outmat %<>% c(setNames((col.s %>% coefficients)[2,],c('yx.alt.col3.iptw.beta','yx.alt.col3.iptw.se','yx.alt.col3.iptw.t','yx.alt.col3.iptw.pv')))
    outmat %<>% c(setNames((col %>% confint)[2,],c('yx.alt.col3.iptw.LL','yx.alt.col3.iptw.UL')));rm(col); rm(col.s)   
    
    rm(data1)
    
    #AIPW
    col <- with(data, AIPW$new(Y=Y, A=X.Z.ha, W=Z.ho1+Zc.1a+Zc.2a+Zc.3a, g.SL.library = "SL.glm.binom", Q.SL.library = "SL.glm.binom", k_split = 1)$fit())
    col.s <- col$summary(g.bound=0.0)
    outmat %<>% c(setNames(c(log(col.s$estimates$OR[1]), col.s$estimates$OR[2], NA, NA, log(col.s$estimates$OR[3]), log(col.s$estimates$OR[4])),
                           c('yx.alt.col3.aipw.beta','yx.alt.col3.aipw.se','yx.alt.col3.aipw.Z','yx.alt.col3.aipw.pv','yx.alt.col3.aipw.LL','yx.alt.col3.aipw.UL')))
    rm(col); rm(col.s)

  return(outmat)
}

#est.del<-getests(dat.tst)
#length(est.del)
#est.del
#table(duplicated(names(est.del)))




















