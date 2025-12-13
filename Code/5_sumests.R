library(magrittr)

sumests<-function(output,...){
  delta.xz<-log(output$dxz[1])
  delta.xy<-log(output$dxy[1])
  N.pt<-output$n.per.arm[1]-3 #DF for the T-dist for the t test statistic for the adjusted model
  N.pt2<-output$n.per.arm[1]-2 #for the IPTW model
  
  #X~Z  
  alp.xz.ord <- with(output, sum(abs(xz.null.ord.Z)>=qnorm(.975))/length(xz.null.ord.Z))
  alp.xz.con1 <- with(output, sum(abs(xz.null.con1.Z)>=qnorm(.975))/length(xz.null.con1.Z))
  alp.xz.con2 <- with(output, sum(abs(xz.null.con2.Z)>=qnorm(.975))/length(xz.null.con2.Z))
  alp.xz.con3 <- with(output, sum(abs(xz.null.con3.Z)>=qnorm(.975))/length(xz.null.con3.Z))
  alp.xz.col1 <- with(output, sum(abs(xz.null.col1.Z)>=qnorm(.975))/length(xz.null.col1.Z))
  alp.xz.col2 <- with(output, sum(abs(xz.null.col2.Z)>=qnorm(.975))/length(xz.null.col2.Z))
  alp.xz.col3 <- with(output, sum(abs(xz.null.col3.Z)>=qnorm(.975))/length(xz.null.col3.Z))
  
  pwr.xz.ord <- with(output, sum(abs(xz.alt.ord.Z)>=qnorm(.975))/length(xz.alt.ord.Z))
  pwr.xz.con1 <- with(output, sum(abs(xz.alt.con1.Z)>=qnorm(.975))/length(xz.alt.con1.Z))
  pwr.xz.con2 <- with(output, sum(abs(xz.alt.con2.Z)>=qnorm(.975))/length(xz.alt.con2.Z))
  pwr.xz.con3 <- with(output, sum(abs(xz.alt.con3.Z)>=qnorm(.975))/length(xz.alt.con3.Z))
  pwr.xz.col1 <- with(output, sum(abs(xz.alt.col1.Z)>=qnorm(.975))/length(xz.alt.col1.Z))
  pwr.xz.col2 <- with(output, sum(abs(xz.alt.col2.Z)>=qnorm(.975))/length(xz.alt.col2.Z))
  pwr.xz.col3 <- with(output, sum(abs(xz.alt.col3.Z)>=qnorm(.975))/length(xz.alt.col3.Z))
  
  bias.xz.ord <- with(output, mean(xz.alt.ord.beta,na.rm=TRUE) - delta.xz)
  bias.xz.con1 <- with(output, mean(xz.alt.con1.beta,na.rm=TRUE) - delta.xz)
  bias.xz.con2 <- with(output, mean(xz.alt.con2.beta,na.rm=TRUE) - delta.xz)
  bias.xz.con3 <- with(output, mean(xz.alt.con3.beta,na.rm=TRUE) - delta.xz)
  bias.xz.col1 <- with(output, mean(xz.alt.col1.beta,na.rm=TRUE) - delta.xz)
  bias.xz.col2 <- with(output, mean(xz.alt.col2.beta,na.rm=TRUE) - delta.xz)
  bias.xz.col3 <- with(output, mean(xz.alt.col3.beta,na.rm=TRUE) - delta.xz)
  #X~Z 
  
  #Y~X
  pwr.xy.adj.ord <- with(output, sum(abs(yx.alt.ord.adj.t)>=qt(.975,N.pt))/length(yx.alt.ord.adj.t))
  pwr.xy.adj.con1 <- with(output, sum(abs(yx.alt.con1.adj.t)>=qt(.975,N.pt))/length(yx.alt.con1.adj.t))
  pwr.xy.adj.con2 <- with(output, sum(abs(yx.alt.con2.adj.t)>=qt(.975,N.pt))/length(yx.alt.con2.adj.t))
  pwr.xy.adj.con3 <- with(output, sum(abs(yx.alt.con3.adj.t)>=qt(.975,N.pt))/length(yx.alt.con3.adj.t))
  pwr.xy.adj.col1 <- with(output, sum(abs(yx.alt.col1.adj.t)>=qt(.975,N.pt))/length(yx.alt.col1.adj.t))
  pwr.xy.adj.col2 <- with(output, sum(abs(yx.alt.col2.adj.t)>=qt(.975,N.pt))/length(yx.alt.col2.adj.t))
  pwr.xy.adj.col3 <- with(output, sum(abs(yx.alt.col3.adj.t)>=qt(.975,N.pt))/length(yx.alt.col3.adj.t))
  
  pwr.xy.cond.ord <- with(output, sum(abs(yx.alt.ord.cond.Z)>=qnorm(.975))/length(yx.alt.ord.cond.Z))
  pwr.xy.cond.con1 <- with(output, sum(abs(yx.alt.con1.cond.Z)>=qnorm(.975))/length(yx.alt.con1.cond.Z))
  pwr.xy.cond.con2 <- with(output, sum(abs(yx.alt.con2.cond.Z)>=qnorm(.975))/length(yx.alt.con2.cond.Z))
  pwr.xy.cond.con3 <- with(output, sum(abs(yx.alt.con3.cond.Z)>=qnorm(.975))/length(yx.alt.con3.cond.Z))
  pwr.xy.cond.col1 <- with(output, sum(abs(yx.alt.col1.cond.Z)>=qnorm(.975))/length(yx.alt.col1.cond.Z))
  pwr.xy.cond.col2 <- with(output, sum(abs(yx.alt.col2.cond.Z)>=qnorm(.975))/length(yx.alt.col2.cond.Z))
  pwr.xy.cond.col3 <- with(output, sum(abs(yx.alt.col3.cond.Z)>=qnorm(.975))/length(yx.alt.col3.cond.Z))
  
  pwr.xy.iptw.ord <- with(output, sum(abs(yx.alt.ord.iptw.t)>=qt(.975,N.pt2))/length(yx.alt.ord.iptw.t))
  pwr.xy.iptw.con1 <- with(output, sum(abs(yx.alt.con1.iptw.t)>=qt(.975,N.pt2))/length(yx.alt.con1.iptw.t))
  pwr.xy.iptw.con2 <- with(output, sum(abs(yx.alt.con2.iptw.t)>=qt(.975,N.pt2))/length(yx.alt.con2.iptw.t))
  pwr.xy.iptw.con3 <- with(output, sum(abs(yx.alt.con3.iptw.t)>=qt(.975,N.pt2))/length(yx.alt.con3.iptw.t))
  pwr.xy.iptw.col1 <- with(output, sum(abs(yx.alt.col1.iptw.t)>=qt(.975,N.pt2))/length(yx.alt.col1.iptw.t))
  pwr.xy.iptw.col2 <- with(output, sum(abs(yx.alt.col2.iptw.t)>=qt(.975,N.pt2))/length(yx.alt.col2.iptw.t))
  pwr.xy.iptw.col3 <- with(output, sum(abs(yx.alt.col3.iptw.t)>=qt(.975,N.pt2))/length(yx.alt.col3.iptw.t))
  
  
  bias.xy.adj.ord<- with(output, mean(yx.alt.ord.adj.beta,na.rm=TRUE) - delta.xy)
  bias.xy.adj.con1<- with(output, mean(yx.alt.con1.adj.beta,na.rm=TRUE) - delta.xy)
  bias.xy.adj.con2<- with(output, mean(yx.alt.con2.adj.beta,na.rm=TRUE) - delta.xy)
  bias.xy.adj.con3<- with(output, mean(yx.alt.con3.adj.beta,na.rm=TRUE) - delta.xy)
  bias.xy.adj.col1<- with(output, mean(yx.alt.col1.adj.beta,na.rm=TRUE) - delta.xy)
  bias.xy.adj.col2<- with(output, mean(yx.alt.col2.adj.beta,na.rm=TRUE) - delta.xy)
  bias.xy.adj.col3<- with(output, mean(yx.alt.col3.adj.beta,na.rm=TRUE) - delta.xy)
  
  bias.xy.cond.ord<- with(output, mean(yx.alt.ord.cond.beta,na.rm=TRUE) - delta.xy)
  bias.xy.cond.con1<- with(output, mean(yx.alt.con1.cond.beta,na.rm=TRUE) - delta.xy)
  bias.xy.cond.con2<- with(output, mean(yx.alt.con2.cond.beta,na.rm=TRUE) - delta.xy)
  bias.xy.cond.con3<- with(output, mean(yx.alt.con3.cond.beta,na.rm=TRUE) - delta.xy)
  bias.xy.cond.col1<- with(output, mean(yx.alt.col1.cond.beta,na.rm=TRUE) - delta.xy)
  bias.xy.cond.col2<- with(output, mean(yx.alt.col2.cond.beta,na.rm=TRUE) - delta.xy)
  bias.xy.cond.col3<- with(output, mean(yx.alt.col3.cond.beta,na.rm=TRUE) - delta.xy)
  
  bias.xy.iptw.ord<- with(output, mean( yx.alt.ord.iptw.beta,na.rm=TRUE) - delta.xy)
  bias.xy.iptw.con1<- with(output, mean(yx.alt.con1.iptw.beta,na.rm=TRUE) - delta.xy)
  bias.xy.iptw.con2<- with(output, mean(yx.alt.con2.iptw.beta,na.rm=TRUE) - delta.xy)
  bias.xy.iptw.con3<- with(output, mean(yx.alt.con3.iptw.beta,na.rm=TRUE) - delta.xy)
  bias.xy.iptw.col1<- with(output, mean(yx.alt.col1.iptw.beta,na.rm=TRUE) - delta.xy)
  bias.xy.iptw.col2<- with(output, mean(yx.alt.col2.iptw.beta,na.rm=TRUE) - delta.xy)
  bias.xy.iptw.col3<- with(output, mean(yx.alt.col3.iptw.beta,na.rm=TRUE) - delta.xy)
  
  bias.xy.aipw.ord<- with(output, mean( yx.alt.ord.aipw.beta,na.rm=TRUE) - delta.xy)
  bias.xy.aipw.con1<- with(output, mean(yx.alt.con1.aipw.beta,na.rm=TRUE) - delta.xy)
  bias.xy.aipw.con2<- with(output, mean(yx.alt.con2.aipw.beta,na.rm=TRUE) - delta.xy)
  bias.xy.aipw.con3<- with(output, mean(yx.alt.con3.aipw.beta,na.rm=TRUE) - delta.xy)
  bias.xy.aipw.col1<- with(output, mean(yx.alt.col1.aipw.beta,na.rm=TRUE) - delta.xy)
  bias.xy.aipw.col2<- with(output, mean(yx.alt.col2.aipw.beta,na.rm=TRUE) - delta.xy)
  bias.xy.aipw.col3<- with(output, mean(yx.alt.col3.aipw.beta,na.rm=TRUE) - delta.xy)
  
  
  empse.xy.adj.ord<-with(output, sqrt((1/(sum(!is.na(yx.alt.ord.adj.beta),na.rm=TRUE)-1)) * sum((yx.alt.ord.adj.beta - mean(yx.alt.ord.adj.beta,na.rm=TRUE))^2,na.rm=TRUE)))
  empse.xy.adj.con1<-with(output, sqrt((1/(sum(!is.na(yx.alt.con1.adj.beta),na.rm=TRUE)-1)) * sum((yx.alt.con1.adj.beta - mean(yx.alt.con1.adj.beta,na.rm=TRUE))^2,na.rm=TRUE)))
  empse.xy.adj.con2<-with(output, sqrt((1/(sum(!is.na(yx.alt.con2.adj.beta),na.rm=TRUE)-1)) * sum((yx.alt.con2.adj.beta - mean(yx.alt.con2.adj.beta,na.rm=TRUE))^2,na.rm=TRUE)))
  empse.xy.adj.con3<-with(output, sqrt((1/(sum(!is.na(yx.alt.con3.adj.beta),na.rm=TRUE)-1)) * sum((yx.alt.con3.adj.beta - mean(yx.alt.con3.adj.beta,na.rm=TRUE))^2,na.rm=TRUE)))
  empse.xy.adj.col1<-with(output, sqrt((1/(sum(!is.na(yx.alt.col1.adj.beta),na.rm=TRUE)-1)) * sum((yx.alt.col1.adj.beta - mean(yx.alt.col1.adj.beta,na.rm=TRUE))^2,na.rm=TRUE)))
  empse.xy.adj.col2<-with(output, sqrt((1/(sum(!is.na(yx.alt.col2.adj.beta),na.rm=TRUE)-1)) * sum((yx.alt.col2.adj.beta - mean(yx.alt.col2.adj.beta,na.rm=TRUE))^2,na.rm=TRUE)))
  empse.xy.adj.col3<-with(output, sqrt((1/(sum(!is.na(yx.alt.col3.adj.beta),na.rm=TRUE)-1)) * sum((yx.alt.col3.adj.beta - mean(yx.alt.col3.adj.beta,na.rm=TRUE))^2,na.rm=TRUE)))
  
  empse.xy.cond.ord<-with(output, sqrt((1/(sum(!is.na(yx.alt.ord.cond.beta),na.rm=TRUE)-1)) * sum((yx.alt.ord.cond.beta - mean(yx.alt.ord.cond.beta,na.rm=TRUE))^2,na.rm=TRUE)))
  empse.xy.cond.con1<-with(output, sqrt((1/(sum(!is.na(yx.alt.con1.cond.beta),na.rm=TRUE)-1)) * sum((yx.alt.con1.cond.beta - mean(yx.alt.con1.cond.beta,na.rm=TRUE))^2,na.rm=TRUE)))
  empse.xy.cond.con2<-with(output, sqrt((1/(sum(!is.na(yx.alt.con2.cond.beta),na.rm=TRUE)-1)) * sum((yx.alt.con2.cond.beta - mean(yx.alt.con2.cond.beta,na.rm=TRUE))^2,na.rm=TRUE)))
  empse.xy.cond.con3<-with(output, sqrt((1/(sum(!is.na(yx.alt.con3.cond.beta),na.rm=TRUE)-1)) * sum((yx.alt.con3.cond.beta - mean(yx.alt.con3.cond.beta,na.rm=TRUE))^2,na.rm=TRUE)))
  empse.xy.cond.col1<-with(output, sqrt((1/(sum(!is.na(yx.alt.col1.cond.beta),na.rm=TRUE)-1)) * sum((yx.alt.col1.cond.beta - mean(yx.alt.col1.cond.beta,na.rm=TRUE))^2,na.rm=TRUE)))
  empse.xy.cond.col2<-with(output, sqrt((1/(sum(!is.na(yx.alt.col2.cond.beta),na.rm=TRUE)-1)) * sum((yx.alt.col2.cond.beta - mean(yx.alt.col2.cond.beta,na.rm=TRUE))^2,na.rm=TRUE)))
  empse.xy.cond.col3<-with(output, sqrt((1/(sum(!is.na(yx.alt.col3.cond.beta),na.rm=TRUE)-1)) * sum((yx.alt.col3.cond.beta - mean(yx.alt.col3.cond.beta,na.rm=TRUE))^2,na.rm=TRUE)))
  
  empse.xy.iptw.ord<-with(output, sqrt((1/(sum(!is.na(yx.alt.ord.iptw.beta),na.rm=TRUE)-1)) * sum((yx.alt.ord.iptw.beta - mean(yx.alt.ord.iptw.beta,na.rm=TRUE))^2,na.rm=TRUE)))
  empse.xy.iptw.con1<-with(output, sqrt((1/(sum(!is.na(yx.alt.con1.iptw.beta),na.rm=TRUE)-1)) * sum((yx.alt.con1.iptw.beta - mean(yx.alt.con1.iptw.beta,na.rm=TRUE))^2,na.rm=TRUE)))
  empse.xy.iptw.con2<-with(output, sqrt((1/(sum(!is.na(yx.alt.con2.iptw.beta),na.rm=TRUE)-1)) * sum((yx.alt.con2.iptw.beta - mean(yx.alt.con2.iptw.beta,na.rm=TRUE))^2,na.rm=TRUE)))
  empse.xy.iptw.con3<-with(output, sqrt((1/(sum(!is.na(yx.alt.con3.iptw.beta),na.rm=TRUE)-1)) * sum((yx.alt.con3.iptw.beta - mean(yx.alt.con3.iptw.beta,na.rm=TRUE))^2,na.rm=TRUE)))
  empse.xy.iptw.col1<-with(output, sqrt((1/(sum(!is.na(yx.alt.col1.iptw.beta),na.rm=TRUE)-1)) * sum((yx.alt.col1.iptw.beta - mean(yx.alt.col1.iptw.beta,na.rm=TRUE))^2,na.rm=TRUE)))
  empse.xy.iptw.col2<-with(output, sqrt((1/(sum(!is.na(yx.alt.col2.iptw.beta),na.rm=TRUE)-1)) * sum((yx.alt.col2.iptw.beta - mean(yx.alt.col2.iptw.beta,na.rm=TRUE))^2,na.rm=TRUE)))
  empse.xy.iptw.col3<-with(output, sqrt((1/(sum(!is.na(yx.alt.col3.iptw.beta),na.rm=TRUE)-1)) * sum((yx.alt.col3.iptw.beta - mean(yx.alt.col3.iptw.beta,na.rm=TRUE))^2,na.rm=TRUE)))
  
  empse.xy.aipw.ord<-with(output, sqrt((1/(sum(!is.na(yx.alt.ord.aipw.beta),na.rm=TRUE)-1)) * sum((yx.alt.ord.aipw.beta - mean(yx.alt.ord.aipw.beta,na.rm=TRUE))^2,na.rm=TRUE)))
  empse.xy.aipw.con1<-with(output, sqrt((1/(sum(!is.na(yx.alt.con1.aipw.beta),na.rm=TRUE)-1)) * sum((yx.alt.con1.aipw.beta - mean(yx.alt.con1.aipw.beta,na.rm=TRUE))^2,na.rm=TRUE)))
  empse.xy.aipw.con2<-with(output, sqrt((1/(sum(!is.na(yx.alt.con2.aipw.beta),na.rm=TRUE)-1)) * sum((yx.alt.con2.aipw.beta - mean(yx.alt.con2.aipw.beta,na.rm=TRUE))^2,na.rm=TRUE)))
  empse.xy.aipw.con3<-with(output, sqrt((1/(sum(!is.na(yx.alt.con3.aipw.beta),na.rm=TRUE)-1)) * sum((yx.alt.con3.aipw.beta - mean(yx.alt.con3.aipw.beta,na.rm=TRUE))^2,na.rm=TRUE)))
  empse.xy.aipw.col1<-with(output, sqrt((1/(sum(!is.na(yx.alt.col1.aipw.beta),na.rm=TRUE)-1)) * sum((yx.alt.col1.aipw.beta - mean(yx.alt.col1.aipw.beta,na.rm=TRUE))^2,na.rm=TRUE)))
  empse.xy.aipw.col2<-with(output, sqrt((1/(sum(!is.na(yx.alt.col2.aipw.beta),na.rm=TRUE)-1)) * sum((yx.alt.col2.aipw.beta - mean(yx.alt.col2.aipw.beta,na.rm=TRUE))^2,na.rm=TRUE)))
  empse.xy.aipw.col3<-with(output, sqrt((1/(sum(!is.na(yx.alt.col3.aipw.beta),na.rm=TRUE)-1)) * sum((yx.alt.col3.aipw.beta - mean(yx.alt.col3.aipw.beta,na.rm=TRUE))^2,na.rm=TRUE)))
  
  
  mse.xy.adj.ord<-with(output, mean((yx.alt.ord.adj.beta - delta.xy)^2, na.rm=TRUE))
  mse.xy.adj.con1<-with(output, mean((yx.alt.con1.adj.beta - delta.xy)^2, na.rm=TRUE))
  mse.xy.adj.con2<-with(output, mean((yx.alt.con2.adj.beta - delta.xy)^2, na.rm=TRUE))
  mse.xy.adj.con3<-with(output, mean((yx.alt.con3.adj.beta - delta.xy)^2, na.rm=TRUE))
  mse.xy.adj.col1<-with(output, mean((yx.alt.col1.adj.beta - delta.xy)^2, na.rm=TRUE))
  mse.xy.adj.col2<-with(output, mean((yx.alt.col2.adj.beta - delta.xy)^2, na.rm=TRUE))
  mse.xy.adj.col3<-with(output, mean((yx.alt.col3.adj.beta - delta.xy)^2, na.rm=TRUE))
  
  mse.xy.cond.ord<-with(output, mean((yx.alt.ord.cond.beta - delta.xy)^2, na.rm=TRUE))
  mse.xy.cond.con1<-with(output, mean((yx.alt.con1.cond.beta - delta.xy)^2, na.rm=TRUE))
  mse.xy.cond.con2<-with(output, mean((yx.alt.con2.cond.beta - delta.xy)^2, na.rm=TRUE))
  mse.xy.cond.con3<-with(output, mean((yx.alt.con3.cond.beta - delta.xy)^2, na.rm=TRUE))
  mse.xy.cond.col1<-with(output, mean((yx.alt.col1.cond.beta - delta.xy)^2, na.rm=TRUE))
  mse.xy.cond.col2<-with(output, mean((yx.alt.col2.cond.beta - delta.xy)^2, na.rm=TRUE))
  mse.xy.cond.col3<-with(output, mean((yx.alt.col3.cond.beta - delta.xy)^2, na.rm=TRUE))
  
  mse.xy.iptw.ord<-with(output, mean((yx.alt.ord.iptw.beta - delta.xy)^2, na.rm=TRUE))
  mse.xy.iptw.con1<-with(output, mean((yx.alt.con1.iptw.beta - delta.xy)^2, na.rm=TRUE))
  mse.xy.iptw.con2<-with(output, mean((yx.alt.con2.iptw.beta - delta.xy)^2, na.rm=TRUE))
  mse.xy.iptw.con3<-with(output, mean((yx.alt.con3.iptw.beta - delta.xy)^2, na.rm=TRUE))
  mse.xy.iptw.col1<-with(output, mean((yx.alt.col1.iptw.beta - delta.xy)^2, na.rm=TRUE))
  mse.xy.iptw.col2<-with(output, mean((yx.alt.col2.iptw.beta - delta.xy)^2, na.rm=TRUE))
  mse.xy.iptw.col3<-with(output, mean((yx.alt.col3.iptw.beta - delta.xy)^2, na.rm=TRUE))
  
  mse.xy.aipw.ord<-with(output, mean((yx.alt.ord.aipw.beta - delta.xy)^2, na.rm=TRUE))
  mse.xy.aipw.con1<-with(output, mean((yx.alt.con1.aipw.beta - delta.xy)^2, na.rm=TRUE))
  mse.xy.aipw.con2<-with(output, mean((yx.alt.con2.aipw.beta - delta.xy)^2, na.rm=TRUE))
  mse.xy.aipw.con3<-with(output, mean((yx.alt.con3.aipw.beta - delta.xy)^2, na.rm=TRUE))
  mse.xy.aipw.col1<-with(output, mean((yx.alt.col1.aipw.beta - delta.xy)^2, na.rm=TRUE))
  mse.xy.aipw.col2<-with(output, mean((yx.alt.col2.aipw.beta - delta.xy)^2, na.rm=TRUE))
  mse.xy.aipw.col3<-with(output, mean((yx.alt.col3.aipw.beta - delta.xy)^2, na.rm=TRUE))
  
  cov.xy.adj.ord<-with(output, (1/sum(!is.na(yx.alt.ord.adj.LL))) * sum(((yx.alt.ord.adj.LL<delta.xy)&(yx.alt.ord.adj.UL>delta.xy)),na.rm=TRUE))
  cov.xy.adj.con1<-with(output, (1/sum(!is.na(yx.alt.con1.adj.LL))) * sum(((yx.alt.con1.adj.LL<delta.xy)&(yx.alt.con1.adj.UL>delta.xy)),na.rm=TRUE))
  cov.xy.adj.con2<-with(output, (1/sum(!is.na(yx.alt.con2.adj.LL))) * sum(((yx.alt.con2.adj.LL<delta.xy)&(yx.alt.con2.adj.UL>delta.xy)),na.rm=TRUE))
  cov.xy.adj.con3<-with(output, (1/sum(!is.na(yx.alt.con3.adj.LL))) * sum(((yx.alt.con3.adj.LL<delta.xy)&(yx.alt.con3.adj.UL>delta.xy)),na.rm=TRUE))
  cov.xy.adj.col1<-with(output, (1/sum(!is.na(yx.alt.col1.adj.LL))) * sum(((yx.alt.col1.adj.LL<delta.xy)&(yx.alt.col1.adj.UL>delta.xy)),na.rm=TRUE))
  cov.xy.adj.col2<-with(output, (1/sum(!is.na(yx.alt.col2.adj.LL))) * sum(((yx.alt.col2.adj.LL<delta.xy)&(yx.alt.col2.adj.UL>delta.xy)),na.rm=TRUE))
  cov.xy.adj.col3<-with(output, (1/sum(!is.na(yx.alt.col3.adj.LL))) * sum(((yx.alt.col3.adj.LL<delta.xy)&(yx.alt.col3.adj.UL>delta.xy)),na.rm=TRUE))
  
  cov.xy.cond.ord<-with(output, (1/sum(!is.na(yx.alt.ord.cond.LL))) * sum(((yx.alt.ord.cond.LL<delta.xy)&(yx.alt.ord.cond.UL>delta.xy)),na.rm=TRUE))
  cov.xy.cond.con1<-with(output, (1/sum(!is.na(yx.alt.con1.cond.LL))) * sum(((yx.alt.con1.cond.LL<delta.xy)&(yx.alt.con1.cond.UL>delta.xy)),na.rm=TRUE))
  cov.xy.cond.con2<-with(output, (1/sum(!is.na(yx.alt.con2.cond.LL))) * sum(((yx.alt.con2.cond.LL<delta.xy)&(yx.alt.con2.cond.UL>delta.xy)),na.rm=TRUE))
  cov.xy.cond.con3<-with(output, (1/sum(!is.na(yx.alt.con3.cond.LL))) * sum(((yx.alt.con3.cond.LL<delta.xy)&(yx.alt.con3.cond.UL>delta.xy)),na.rm=TRUE))
  cov.xy.cond.col1<-with(output, (1/sum(!is.na(yx.alt.col1.cond.LL))) * sum(((yx.alt.col1.cond.LL<delta.xy)&(yx.alt.col1.cond.UL>delta.xy)),na.rm=TRUE))
  cov.xy.cond.col2<-with(output, (1/sum(!is.na(yx.alt.col2.cond.LL))) * sum(((yx.alt.col2.cond.LL<delta.xy)&(yx.alt.col2.cond.UL>delta.xy)),na.rm=TRUE))
  cov.xy.cond.col3<-with(output, (1/sum(!is.na(yx.alt.col3.cond.LL))) * sum(((yx.alt.col3.cond.LL<delta.xy)&(yx.alt.col3.cond.UL>delta.xy)),na.rm=TRUE))
  
  cov.xy.iptw.ord<-with(output, (1/sum(!is.na(yx.alt.ord.iptw.LL))) * sum(((yx.alt.ord.iptw.LL<delta.xy)&(yx.alt.ord.iptw.UL>delta.xy)),na.rm=TRUE))
  cov.xy.iptw.con1<-with(output, (1/sum(!is.na(yx.alt.con1.iptw.LL))) * sum(((yx.alt.con1.iptw.LL<delta.xy)&(yx.alt.con1.iptw.UL>delta.xy)),na.rm=TRUE))
  cov.xy.iptw.con2<-with(output, (1/sum(!is.na(yx.alt.con2.iptw.LL))) * sum(((yx.alt.con2.iptw.LL<delta.xy)&(yx.alt.con2.iptw.UL>delta.xy)),na.rm=TRUE))
  cov.xy.iptw.con3<-with(output, (1/sum(!is.na(yx.alt.con3.iptw.LL))) * sum(((yx.alt.con3.iptw.LL<delta.xy)&(yx.alt.con3.iptw.UL>delta.xy)),na.rm=TRUE))
  cov.xy.iptw.col1<-with(output, (1/sum(!is.na(yx.alt.col1.iptw.LL))) * sum(((yx.alt.col1.iptw.LL<delta.xy)&(yx.alt.col1.iptw.UL>delta.xy)),na.rm=TRUE))
  cov.xy.iptw.col2<-with(output, (1/sum(!is.na(yx.alt.col2.iptw.LL))) * sum(((yx.alt.col2.iptw.LL<delta.xy)&(yx.alt.col2.iptw.UL>delta.xy)),na.rm=TRUE))
  cov.xy.iptw.col3<-with(output, (1/sum(!is.na(yx.alt.col3.iptw.LL))) * sum(((yx.alt.col3.iptw.LL<delta.xy)&(yx.alt.col3.iptw.UL>delta.xy)),na.rm=TRUE))
  
  cov.xy.aipw.ord<-with(output, (1/sum(!is.na(yx.alt.ord.aipw.LL))) * sum(((yx.alt.ord.aipw.LL<delta.xy)&(yx.alt.ord.aipw.UL>delta.xy)),na.rm=TRUE))
  cov.xy.aipw.con1<-with(output, (1/sum(!is.na(yx.alt.con1.aipw.LL))) * sum(((yx.alt.con1.aipw.LL<delta.xy)&(yx.alt.con1.aipw.UL>delta.xy)),na.rm=TRUE))
  cov.xy.aipw.con2<-with(output, (1/sum(!is.na(yx.alt.con2.aipw.LL))) * sum(((yx.alt.con2.aipw.LL<delta.xy)&(yx.alt.con2.aipw.UL>delta.xy)),na.rm=TRUE))
  cov.xy.aipw.con3<-with(output, (1/sum(!is.na(yx.alt.con3.aipw.LL))) * sum(((yx.alt.con3.aipw.LL<delta.xy)&(yx.alt.con3.aipw.UL>delta.xy)),na.rm=TRUE))
  cov.xy.aipw.col1<-with(output, (1/sum(!is.na(yx.alt.col1.aipw.LL))) * sum(((yx.alt.col1.aipw.LL<delta.xy)&(yx.alt.col1.aipw.UL>delta.xy)),na.rm=TRUE))
  cov.xy.aipw.col2<-with(output, (1/sum(!is.na(yx.alt.col2.aipw.LL))) * sum(((yx.alt.col2.aipw.LL<delta.xy)&(yx.alt.col2.aipw.UL>delta.xy)),na.rm=TRUE))
  cov.xy.aipw.col3<-with(output, (1/sum(!is.na(yx.alt.col3.aipw.LL))) * sum(((yx.alt.col3.aipw.LL<delta.xy)&(yx.alt.col3.aipw.UL>delta.xy)),na.rm=TRUE))
 

  result<-data.frame(cbind(
            c(alp.xz.ord, pwr.xz.ord, bias.xz.ord, pwr.xy.adj.ord, pwr.xy.cond.ord, pwr.xy.iptw.ord, bias.xy.adj.ord, bias.xy.cond.ord, bias.xy.iptw.ord, bias.xy.aipw.ord, empse.xy.adj.ord, empse.xy.cond.ord, empse.xy.iptw.ord, empse.xy.aipw.ord, mse.xy.adj.ord,    mse.xy.cond.ord, mse.xy.iptw.ord, mse.xy.aipw.ord, cov.xy.adj.ord, cov.xy.cond.ord, cov.xy.iptw.ord, cov.xy.aipw.ord),
            c(alp.xz.con1,pwr.xz.con1,bias.xz.con1,pwr.xy.adj.con1,pwr.xy.cond.con1,pwr.xy.iptw.con1,bias.xy.adj.con1,bias.xy.cond.con1,bias.xy.iptw.con1,bias.xy.aipw.con1,empse.xy.adj.con1,empse.xy.cond.con1,empse.xy.iptw.con1,empse.xy.aipw.con1,mse.xy.adj.con1,   mse.xy.cond.con1,mse.xy.iptw.con1,mse.xy.aipw.con1,cov.xy.adj.con1,cov.xy.cond.con1,cov.xy.iptw.con1,cov.xy.aipw.con1),
            c(alp.xz.con2,pwr.xz.con2,bias.xz.con2,pwr.xy.adj.con2,pwr.xy.cond.con2,pwr.xy.iptw.con2,bias.xy.adj.con2,bias.xy.cond.con2,bias.xy.iptw.con2,bias.xy.aipw.con2,empse.xy.adj.con2,empse.xy.cond.con2,empse.xy.iptw.con2,empse.xy.aipw.con2,mse.xy.adj.con2,   mse.xy.cond.con2,mse.xy.iptw.con2,mse.xy.aipw.con2,cov.xy.adj.con2,cov.xy.cond.con2,cov.xy.iptw.con2,cov.xy.aipw.con2),
            c(alp.xz.con3,pwr.xz.con3,bias.xz.con3,pwr.xy.adj.con3,pwr.xy.cond.con3,pwr.xy.iptw.con3,bias.xy.adj.con3,bias.xy.cond.con3,bias.xy.iptw.con3,bias.xy.aipw.con3,empse.xy.adj.con3,empse.xy.cond.con3,empse.xy.iptw.con3,empse.xy.aipw.con3,mse.xy.adj.con3,   mse.xy.cond.con3,mse.xy.iptw.con3,mse.xy.aipw.con3,cov.xy.adj.con3,cov.xy.cond.con3,cov.xy.iptw.con3,cov.xy.aipw.con3),
            c(alp.xz.col1,pwr.xz.col1,bias.xz.col1,pwr.xy.adj.col1,pwr.xy.cond.col1,pwr.xy.iptw.col1,bias.xy.adj.col1,bias.xy.cond.col1,bias.xy.iptw.col1,bias.xy.aipw.col1,empse.xy.adj.col1,empse.xy.cond.col1,empse.xy.iptw.col1,empse.xy.aipw.col1,mse.xy.adj.col1,   mse.xy.cond.col1,mse.xy.iptw.col1,mse.xy.aipw.col1,cov.xy.adj.col1,cov.xy.cond.col1,cov.xy.iptw.col1,cov.xy.aipw.col1),
            c(alp.xz.col2,pwr.xz.col2,bias.xz.col2,pwr.xy.adj.col2,pwr.xy.cond.col2,pwr.xy.iptw.col2,bias.xy.adj.col2,bias.xy.cond.col2,bias.xy.iptw.col2,bias.xy.aipw.col2,empse.xy.adj.col2,empse.xy.cond.col2,empse.xy.iptw.col2,empse.xy.aipw.col2,mse.xy.adj.col2,   mse.xy.cond.col2,mse.xy.iptw.col2,mse.xy.aipw.col2,cov.xy.adj.col2,cov.xy.cond.col2,cov.xy.iptw.col2,cov.xy.aipw.col2),
            c(alp.xz.col3,pwr.xz.col3,bias.xz.col3,pwr.xy.adj.col3,pwr.xy.cond.col3,pwr.xy.iptw.col3,bias.xy.adj.col3,bias.xy.cond.col3,bias.xy.iptw.col3,bias.xy.aipw.col3,empse.xy.adj.col3,empse.xy.cond.col3,empse.xy.iptw.col3,empse.xy.aipw.col3,mse.xy.adj.col3,   mse.xy.cond.col3,mse.xy.iptw.col3,mse.xy.aipw.col3,cov.xy.adj.col3,cov.xy.cond.col3,cov.xy.iptw.col3,cov.xy.aipw.col3)
            ),
            row.names=c('XZ.Alpha','XZ.Power','XZ.Bias','PWR.Adj','PWR.Cond','PWR.IPTW','BIAS.Adj','BIAS.Cond','BIAS.IPTW','BIAS.AIPW','EmpSE.Adj','EmpSE.Cond','EmpSE.IPTW','EmpSE.AIPW','MSE.Adj','MSE.Cond','MSE.IPTW','MSE.AIPW','COV.Adj','COV.Cond','COV.IPTW','COV.AIPW'))
  names(result)<-c('Ordinary','Cont1','Cont2','Cont3','Corr1','Corr2','Corr3')
  
  result<-rbind(setNames(data.frame(t(output[1:7, c("n.per.arm","rand.rat_1","rand.rat_2","corr.3_1","corr.3_2","corr.3_3","cont.prop_1","cont.prop_2","cont.prop_3","file_set","dxz","dxy")])),
         c('Ordinary','Cont1','Cont2','Cont3','Corr1','Corr2','Corr3')), result)
  return(result)
}





              


result<-rbind(setNames(data.frame( t(output[1:3,c("file","thetaHR","delta_1","delta_2","mu_1","mu_2","sigma","betaHR_1","betaHR_2","Hx_relative_size")])
                                  ),
                       c("A1","A2","A3")),
              
              set_rownames(setNames(data.frame(rbind( rep(ceiling(mean(output$N)),3),
                                                      rep(ceiling(mean(output$E)),3),
                                                      rep(ceiling(mean(output$C)),3),
                                                      rep(ceiling(mean(output$Hx)),3) ) ),
                                    c("A1","A2","A3")),c("N_tot","E","C","Hx")),
              result)

return(result)










alp.xz.ord, pwr.xz.ord, bias.xz.ord, pwr.xy.adj.ord, pwr.xy.cond.ord, pwr.xy.iptw.ord, bias.xy.adj.ord, bias.xy.cond.ord, bias.xy.iptw.ord, bias.xy.aipw.ord, empse.xy.adj.ord, empse.xy.cond.ord, empse.xy.iptw.ord, empse.xy.aipw.ord, mse.xy.adj.ord,    mse.xy.cond.ord, mse.xy.iptw.ord, mse.xy.aipw.ord, cov.xy.adj.ord, cov.xy.cond.ord, cov.xy.iptw.ord, cov.xy.aipw.ord
alp.xz.con1,pwr.xz.con1,bias.xz.con1,pwr.xy.adj.con1,pwr.xy.cond.con1,pwr.xy.iptw.con1,bias.xy.adj.con1,bias.xy.cond.con1,bias.xy.iptw.con1,bias.xy.aipw.con1,empse.xy.adj.con1,empse.xy.cond.con1,empse.xy.iptw.con1,empse.xy.aipw.con1,mse.xy.adj.con1,   mse.xy.cond.con1,mse.xy.iptw.con1,mse.xy.aipw.con1,cov.xy.adj.con1,cov.xy.cond.con1,cov.xy.iptw.con1,cov.xy.aipw.con1
alp.xz.con2,pwr.xz.con2,bias.xz.con2,pwr.xy.adj.con2,pwr.xy.cond.con2,pwr.xy.iptw.con2,bias.xy.adj.con2,bias.xy.cond.con2,bias.xy.iptw.con2,bias.xy.aipw.con2,empse.xy.adj.con2,empse.xy.cond.con2,empse.xy.iptw.con2,empse.xy.aipw.con2,mse.xy.adj.con2,   mse.xy.cond.con2,mse.xy.iptw.con2,mse.xy.aipw.con2,cov.xy.adj.con2,cov.xy.cond.con2,cov.xy.iptw.con2,cov.xy.aipw.con2
alp.xz.con3,pwr.xz.con3,bias.xz.con3,pwr.xy.adj.con3,pwr.xy.cond.con3,pwr.xy.iptw.con3,bias.xy.adj.con3,bias.xy.cond.con3,bias.xy.iptw.con3,bias.xy.aipw.con3,empse.xy.adj.con3,empse.xy.cond.con3,empse.xy.iptw.con3,empse.xy.aipw.con3,mse.xy.adj.con3,   mse.xy.cond.con3,mse.xy.iptw.con3,mse.xy.aipw.con3,cov.xy.adj.con3,cov.xy.cond.con3,cov.xy.iptw.con3,cov.xy.aipw.con3
alp.xz.col1,pwr.xz.col1,bias.xz.col1,pwr.xy.adj.col1,pwr.xy.cond.col1,pwr.xy.iptw.col1,bias.xy.adj.col1,bias.xy.cond.col1,bias.xy.iptw.col1,bias.xy.aipw.col1,empse.xy.adj.col1,empse.xy.cond.col1,empse.xy.iptw.col1,empse.xy.aipw.col1,mse.xy.adj.col1,   mse.xy.cond.col1,mse.xy.iptw.col1,mse.xy.aipw.col1,cov.xy.adj.col1,cov.xy.cond.col1,cov.xy.iptw.col1,cov.xy.aipw.col1
alp.xz.col2,pwr.xz.col2,bias.xz.col2,pwr.xy.adj.col2,pwr.xy.cond.col2,pwr.xy.iptw.col2,bias.xy.adj.col2,bias.xy.cond.col2,bias.xy.iptw.col2,bias.xy.aipw.col2,empse.xy.adj.col2,empse.xy.cond.col2,empse.xy.iptw.col2,empse.xy.aipw.col2,mse.xy.adj.col2,   mse.xy.cond.col2,mse.xy.iptw.col2,mse.xy.aipw.col2,cov.xy.adj.col2,cov.xy.cond.col2,cov.xy.iptw.col2,cov.xy.aipw.col2
alp.xz.col3,pwr.xz.col3,bias.xz.col3,pwr.xy.adj.col3,pwr.xy.cond.col3,pwr.xy.iptw.col3,bias.xy.adj.col3,bias.xy.cond.col3,bias.xy.iptw.col3,bias.xy.aipw.col3,empse.xy.adj.col3,empse.xy.cond.col3,empse.xy.iptw.col3,empse.xy.aipw.col3,mse.xy.adj.col3,   mse.xy.cond.col3,mse.xy.iptw.col3,mse.xy.aipw.col3,cov.xy.adj.col3,cov.xy.cond.col3,cov.xy.iptw.col3,cov.xy.aipw.col3

















result<-data.frame(cbind(  cbind(  cbind(c(A1_E_bar,A1_Bias,A1_Perc_Bias,A1_E_EmpSE,A1_MSE,A1_coverage,A1_Avg_CI_length,
                                           A1_rejection,A1_MC_SE,MC_Coverage_SE,A1_AvgCI_MC_SE,A1_EmpSE_MC_SE,A1_MSE_MC_SE),
                                         
                                         c(A2_E_bar,A2_Bias,A2_Perc_Bias,A2_E_EmpSE,A2_MSE,A2_coverage,A2_Avg_CI_length,
                                           A2_rejection,A2_MC_SE,MC_Coverage_SE,A2_AvgCI_MC_SE,A2_EmpSE_MC_SE,A2_MSE_MC_SE) ),  
                                   
                                   c(A3_E_bar,A3_Bias,A3_Perc_Bias,A3_E_EmpSE,A3_MSE,A3_coverage,A3_Avg_CI_length,
                                     A3_rejection,A3_MC_SE,MC_Coverage_SE,A3_AvgCI_MC_SE,A3_EmpSE_MC_SE,A3_MSE_MC_SE))
),
row.names = c("E_bar","Bias","Percent_Bias","Emperical_SE","Mean_Squared_Error","Coverage",
              "Average_CI_length","Rejection_percentage","Monte_Carlo_bias_SE","Monte_Carlo_Coverage_SE","MC_SE_AvgCI","MC_SE_EmpSE","MC_SE_MSE"))
names(result)<-c("A1","A2","A3")




result<-rbind(setNames(data.frame(t(output[1:3,c("file","thetaHR","delta_1","delta_2","mu_1","mu_2","sigma","betaHR_1","betaHR_2","Hx_relative_size")])),
                       c("A1","A2","A3")),
              set_rownames(setNames(data.frame(rbind(rep(ceiling(mean(output$N)),3),
                                                     rep(ceiling(mean(output$E)),3),
                                                     rep(ceiling(mean(output$C)),3),
                                                     rep(ceiling(mean(output$Hx)),3))),
                                    c("A1","A2","A3")),c("N_tot","E","C","Hx")),
              result
)

return(result)






result<-data.frame(cbind(  cbind(  cbind(c(empse.xy.adj.ord,empse.xy.adj.con1,empse.xy.adj.con2,empse.xy.adj.con3,empse.xy.adj.col1,empse.xy.adj.col2,empse.xy.adj.col3,
                                           empse.xy.cond.ord,empse.xy.cond.con1,empse.xy.cond.con2,empse.xy.cond.con3,empse.xy.cond.col1,empse.xy.cond.col2,empse.xy.cond.col3,
                                           empse.xy.iptw.ord,empse.xy.iptw.con1,empse.xy.iptw.con2,empse.xy.iptw.con3,empse.xy.iptw.col1,empse.xy.iptw.col2,empse.xy.iptw.col3,
                                           empse.xy.aipw.ord,empse.xy.aipw.con1,empse.xy.aipw.con2,empse.xy.aipw.con3,empse.xy.aipw.col1,empse.xy.aipw.col2,empse.xy.aipw.col3),
                                         
                                         c(mse.xy.adj.ord,mse.xy.adj.con1,mse.xy.adj.con2,mse.xy.adj.con3,mse.xy.adj.col1,mse.xy.adj.col2,mse.xy.adj.col3,
                                           mse.xy.cond.ord,mse.xy.cond.con1,mse.xy.cond.con2,mse.xy.cond.con3,mse.xy.cond.col1,mse.xy.cond.col2,mse.xy.cond.col3,
                                           mse.xy.iptw.ord,mse.xy.iptw.con1,mse.xy.iptw.con2,mse.xy.iptw.con3,mse.xy.iptw.col1,mse.xy.iptw.col2,mse.xy.iptw.col3,
                                           mse.xy.aipw.ord,mse.xy.aipw.con1,mse.xy.aipw.con2,mse.xy.aipw.con3,mse.xy.aipw.col1,mse.xy.aipw.col2,mse.xy.aipw.col3) ),  
                                   
                                   c(cov.xy.adj.ord,cov.xy.adj.con1,cov.xy.adj.con2,cov.xy.adj.con3,cov.xy.adj.col1,cov.xy.adj.col2,cov.xy.adj.col3,
                                     cov.xy.cond.ord,cov.xy.cond.con1,cov.xy.cond.con2,cov.xy.cond.con3,cov.xy.cond.col1,cov.xy.cond.col2,cov.xy.cond.col3,
                                     cov.xy.iptw.ord,cov.xy.iptw.con1,cov.xy.iptw.con2,cov.xy.iptw.con3,cov.xy.iptw.col1,cov.xy.iptw.col2,cov.xy.iptw.col3,
                                     cov.xy.aipw.ord,cov.xy.aipw.con1,cov.xy.aipw.con2,cov.xy.aipw.con3,cov.xy.aipw.col1,cov.xy.aipw.col2,cov.xy.aipw.col3))))




c(alp.xz.ord,alp.xz.con1,alp.xz.con2,alp.xz.con3,alp.xz.col1,alp.xz.col2,alp.xz.col3,
  pwr.xz.ord,pwr.xz.con1,pwr.xz.con2,pwr.xz.con3,pwr.xz.col1,pwr.xz.col2,pwr.xz.col3,
  bias.xz.ord,bias.xz.con1,bias.xz.con2,bias.xz.con3,bias.xz.col1,bias.xz.col2,bias.xz.col3,
  
  pwr.xy.adj.ord,pwr.xy.adj.con1,pwr.xy.adj.con2,pwr.xy.adj.con3,pwr.xy.adj.col1,pwr.xy.adj.col2,pwr.xy.adj.col3,
  pwr.xy.cond.ord,pwr.xy.cond.con1,pwr.xy.cond.con2,pwr.xy.cond.con3,pwr.xy.cond.col1,pwr.xy.cond.col2,pwr.xy.cond.col3,
  pwr.xy.iptw.ord,pwr.xy.iptw.con1,pwr.xy.iptw.con2,pwr.xy.iptw.con3,pwr.xy.iptw.col1,pwr.xy.iptw.col2,pwr.xy.iptw.col3,
  
  bias.xy.adj.ord,bias.xy.adj.con1,bias.xy.adj.con2,bias.xy.adj.con3,bias.xy.adj.col1,bias.xy.adj.col2,bias.xy.adj.col3,
  bias.xy.cond.ord,bias.xy.cond.con1,bias.xy.cond.con2,bias.xy.cond.con3,bias.xy.cond.col1,bias.xy.cond.col2,bias.xy.cond.col3,
  bias.xy.iptw.ord,bias.xy.iptw.con1,bias.xy.iptw.con2,bias.xy.iptw.con3,bias.xy.iptw.col1,bias.xy.iptw.col2,bias.xy.iptw.col3,
  bias.xy.aipw.ord,bias.xy.aipw.con1,bias.xy.aipw.con2,bias.xy.aipw.con3,bias.xy.aipw.col1,bias.xy.aipw.col2,bias.xy.aipw.col3,
  
  empse.xy.adj.ord,empse.xy.adj.con1,empse.xy.adj.con2,empse.xy.adj.con3,empse.xy.adj.col1,empse.xy.adj.col2,empse.xy.adj.col3,
  empse.xy.cond.ord,empse.xy.cond.con1,empse.xy.cond.con2,empse.xy.cond.con3,empse.xy.cond.col1,empse.xy.cond.col2,empse.xy.cond.col3,
  empse.xy.iptw.ord,empse.xy.iptw.con1,empse.xy.iptw.con2,empse.xy.iptw.con3,empse.xy.iptw.col1,empse.xy.iptw.col2,empse.xy.iptw.col3,
  empse.xy.aipw.ord,empse.xy.aipw.con1,empse.xy.aipw.con2,empse.xy.aipw.con3,empse.xy.aipw.col1,empse.xy.aipw.col2,empse.xy.aipw.col3,
  
  mse.xy.adj.ord,mse.xy.adj.con1,mse.xy.adj.con2,mse.xy.adj.con3,mse.xy.adj.col1,mse.xy.adj.col2,mse.xy.adj.col3,
  mse.xy.cond.ord,mse.xy.cond.con1,mse.xy.cond.con2,mse.xy.cond.con3,mse.xy.cond.col1,mse.xy.cond.col2,mse.xy.cond.col3,
  mse.xy.iptw.ord,mse.xy.iptw.con1,mse.xy.iptw.con2,mse.xy.iptw.con3,mse.xy.iptw.col1,mse.xy.iptw.col2,mse.xy.iptw.col3,
  mse.xy.aipw.ord,mse.xy.aipw.con1,mse.xy.aipw.con2,mse.xy.aipw.con3,mse.xy.aipw.col1,mse.xy.aipw.col2,mse.xy.aipw.col3,
  
  cov.xy.adj.ord,cov.xy.adj.con1,cov.xy.adj.con2,cov.xy.adj.con3,cov.xy.adj.col1,cov.xy.adj.col2,cov.xy.adj.col3,
  cov.xy.cond.ord,cov.xy.cond.con1,cov.xy.cond.con2,cov.xy.cond.con3,cov.xy.cond.col1,cov.xy.cond.col2,cov.xy.cond.col3,
  cov.xy.iptw.ord,cov.xy.iptw.con1,cov.xy.iptw.con2,cov.xy.iptw.con3,cov.xy.iptw.col1,cov.xy.iptw.col2,cov.xy.iptw.col3,
  cov.xy.aipw.ord,cov.xy.aipw.con1,cov.xy.aipw.con2,cov.xy.aipw.con3,cov.xy.aipw.col1,cov.xy.aipw.col2,cov.xy.aipw.col3)



