library(ggplot2)

plotdat<-function(data.set, N, est, scen, scen.cat){
  
  d1<-as.data.frame(parameter_set_summaries[data.set[1]])
  d2<-as.data.frame(parameter_set_summaries[data.set[2]])
  d3<-as.data.frame(parameter_set_summaries[data.set[3]])
  d4<-as.data.frame(parameter_set_summaries[data.set[4]])
  d5<-as.data.frame(parameter_set_summaries[data.set[5]])
  d6<-as.data.frame(parameter_set_summaries[data.set[6]])
  d7<-as.data.frame(parameter_set_summaries[data.set[7]])
  
  if (!(est=="PWR"|est=="XZ"))  {
    out<-as.data.frame(
      rbind(cbind(rbind(d1[row.names(d1)==paste0(est,".Adj"),scen], #[,1] is for ordinary
                        d2[row.names(d2)==paste0(est,".Adj"),scen],
                        d3[row.names(d3)==paste0(est,".Adj"),scen],
                        d4[row.names(d4)==paste0(est,".Adj"),scen],
                        d5[row.names(d5)==paste0(est,".Adj"),scen],
                        d6[row.names(d6)==paste0(est,".Adj"),scen],
                        d7[row.names(d7)==paste0(est,".Adj"),scen]), 1:7, rep("ADJ",7)),
            
            cbind(rbind(d1[row.names(d1)==paste0(est,".Cond"),scen], 
                        d2[row.names(d2)==paste0(est,".Cond"),scen],
                        d3[row.names(d3)==paste0(est,".Cond"),scen],
                        d4[row.names(d4)==paste0(est,".Cond"),scen],
                        d5[row.names(d5)==paste0(est,".Cond"),scen],
                        d6[row.names(d6)==paste0(est,".Cond"),scen],
                        d7[row.names(d7)==paste0(est,".Cond"),scen]), 1:7, rep("COND",7)),
            
            cbind(rbind(d1[row.names(d1)==paste0(est,".IPTW"),scen], 
                        d2[row.names(d2)==paste0(est,".IPTW"),scen],
                        d3[row.names(d3)==paste0(est,".IPTW"),scen],
                        d4[row.names(d4)==paste0(est,".IPTW"),scen],
                        d5[row.names(d5)==paste0(est,".IPTW"),scen],
                        d6[row.names(d6)==paste0(est,".IPTW"),scen],
                        d7[row.names(d7)==paste0(est,".IPTW"),scen]), 1:7, rep("IPTW",7)),
            
            cbind(rbind(d1[row.names(d1)==paste0(est,".AIPW"),scen], 
                        d2[row.names(d2)==paste0(est,".AIPW"),scen],
                        d3[row.names(d3)==paste0(est,".AIPW"),scen],
                        d4[row.names(d4)==paste0(est,".AIPW"),scen],
                        d5[row.names(d5)==paste0(est,".AIPW"),scen],
                        d6[row.names(d6)==paste0(est,".AIPW"),scen],
                        d7[row.names(d7)==paste0(est,".AIPW"),scen]), 1:7, rep("AIPW",7))
      ))
  } else {
    if (est=="PWR") {
    out<-as.data.frame(
      rbind(cbind(rbind(d1[row.names(d1)==paste0(est,".Adj"),scen], #[,1] is for ordinary
                        d2[row.names(d2)==paste0(est,".Adj"),scen],
                        d3[row.names(d3)==paste0(est,".Adj"),scen],
                        d4[row.names(d4)==paste0(est,".Adj"),scen],
                        d5[row.names(d5)==paste0(est,".Adj"),scen],
                        d6[row.names(d6)==paste0(est,".Adj"),scen],
                        d7[row.names(d7)==paste0(est,".Adj"),scen]), 1:7, rep("ADJ",7)),
            
            cbind(rbind(d1[row.names(d1)==paste0(est,".Cond"),scen], 
                        d2[row.names(d2)==paste0(est,".Cond"),scen],
                        d3[row.names(d3)==paste0(est,".Cond"),scen],
                        d4[row.names(d4)==paste0(est,".Cond"),scen],
                        d5[row.names(d5)==paste0(est,".Cond"),scen],
                        d6[row.names(d6)==paste0(est,".Cond"),scen],
                        d7[row.names(d7)==paste0(est,".Cond"),scen]), 1:7, rep("COND",7)),
            
            cbind(rbind(d1[row.names(d1)==paste0(est,".IPTW"),scen], 
                        d2[row.names(d2)==paste0(est,".IPTW"),scen],
                        d3[row.names(d3)==paste0(est,".IPTW"),scen],
                        d4[row.names(d4)==paste0(est,".IPTW"),scen],
                        d5[row.names(d5)==paste0(est,".IPTW"),scen],
                        d6[row.names(d6)==paste0(est,".IPTW"),scen],
                        d7[row.names(d7)==paste0(est,".IPTW"),scen]), 1:7, rep("IPTW",7))
      ))      
    } else {
      out<-as.data.frame(
        rbind(cbind(rbind(d1[row.names(d1)==paste0(est,".Alpha"),scen], #[,1] is for ordinary
                          d2[row.names(d2)==paste0(est,".Alpha"),scen],
                          d3[row.names(d3)==paste0(est,".Alpha"),scen],
                          d4[row.names(d4)==paste0(est,".Alpha"),scen],
                          d5[row.names(d5)==paste0(est,".Alpha"),scen],
                          d6[row.names(d6)==paste0(est,".Alpha"),scen],
                          d7[row.names(d7)==paste0(est,".Alpha"),scen]), 1:7, rep("Alpha",7)),
              
              cbind(rbind(d1[row.names(d1)==paste0(est,".Power"),scen], 
                          d2[row.names(d2)==paste0(est,".Power"),scen],
                          d3[row.names(d3)==paste0(est,".Power"),scen],
                          d4[row.names(d4)==paste0(est,".Power"),scen],
                          d5[row.names(d5)==paste0(est,".Power"),scen],
                          d6[row.names(d6)==paste0(est,".Power"),scen],
                          d7[row.names(d7)==paste0(est,".Power"),scen]), 1:7, rep("Power",7)),
              
              cbind(rbind(d1[row.names(d1)==paste0(est,".Bias"),scen], 
                          d2[row.names(d2)==paste0(est,".Bias"),scen],
                          d3[row.names(d3)==paste0(est,".Bias"),scen],
                          d4[row.names(d4)==paste0(est,".Bias"),scen],
                          d5[row.names(d5)==paste0(est,".Bias"),scen],
                          d6[row.names(d6)==paste0(est,".Bias"),scen],
                          d7[row.names(d7)==paste0(est,".Bias"),scen]), 1:7, rep("Bias",7))
        )) 
    }
  }

  out$size<-paste0("N=",N,"/arm")
  out$scenario<-scen.cat
  colnames(out)[1]<-est
  colnames(out)[2]<-"Ratio"
  colnames(out)[3]<-"Estimator"
  out[,1]<-as.numeric(out[,1])
  out[,2]<-as.numeric(out[,2])
  return(out)
} 

plotdat2<-function(perf){
  
  tst30.ord  <-plotdat(data.set=1:7, N=30, est=perf, scen=1, scen.cat="Ordinary")
  tst30.cont1<-plotdat(data.set=1:7, N=30, est=perf, scen=2, scen.cat="Cont.(5%)")
  tst30.cont2<-plotdat(data.set=1:7, N=30, est=perf, scen=3, scen.cat="Cont.(10%)")
  tst30.cont3<-plotdat(data.set=1:7, N=30, est=perf, scen=4, scen.cat="Cont.(15%)")
  tst30.corr1<-plotdat(data.set=1:7, N=30, est=perf, scen=5, scen.cat="Corr.(rho=0.3)")
  tst30.corr2<-plotdat(data.set=1:7, N=30, est=perf, scen=6, scen.cat="(rho=0.3,0.4)")
  tst30.corr3<-plotdat(data.set=1:7, N=30, est=perf, scen=7, scen.cat="(rho=0.3,0.4,0.5)")
  
  tst50.ord  <-plotdat(data.set=8:14, N=50, est=perf, scen=1, scen.cat="Ordinary")
  tst50.cont1<-plotdat(data.set=8:14, N=50, est=perf, scen=2, scen.cat="Cont.(5%)")
  tst50.cont2<-plotdat(data.set=8:14, N=50, est=perf, scen=3, scen.cat="Cont.(10%)")
  tst50.cont3<-plotdat(data.set=8:14, N=50, est=perf, scen=4, scen.cat="Cont.(15%)")
  tst50.corr1<-plotdat(data.set=8:14, N=50, est=perf, scen=5, scen.cat="Corr.(rho=0.3)")
  tst50.corr2<-plotdat(data.set=8:14, N=50, est=perf, scen=6, scen.cat="(rho=0.3,0.4)")
  tst50.corr3<-plotdat(data.set=8:14, N=50, est=perf, scen=7, scen.cat="(rho=0.3,0.4,0.5)")
  
  tst70.ord  <-plotdat(data.set=15:21, N=70, est=perf, scen=1, scen.cat="Ordinary")
  tst70.cont1<-plotdat(data.set=15:21, N=70, est=perf, scen=2, scen.cat="Cont.(5%)")
  tst70.cont2<-plotdat(data.set=15:21, N=70, est=perf, scen=3, scen.cat="Cont.(10%)")
  tst70.cont3<-plotdat(data.set=15:21, N=70, est=perf, scen=4, scen.cat="Cont.(15%)")
  tst70.corr1<-plotdat(data.set=15:21, N=70, est=perf, scen=5, scen.cat="Corr.(rho=0.3)")
  tst70.corr2<-plotdat(data.set=15:21, N=70, est=perf, scen=6, scen.cat="(rho=0.3,0.4)")
  tst70.corr3<-plotdat(data.set=15:21, N=70, est=perf, scen=7, scen.cat="(rho=0.3,0.4,0.5)")
  
  tst90.ord  <-plotdat(data.set=22:28, N=90, est=perf, scen=1, scen.cat="Ordinary")
  tst90.cont1<-plotdat(data.set=22:28, N=90, est=perf, scen=2, scen.cat="Cont.(5%)")
  tst90.cont2<-plotdat(data.set=22:28, N=90, est=perf, scen=3, scen.cat="Cont.(10%)")
  tst90.cont3<-plotdat(data.set=22:28, N=90, est=perf, scen=4, scen.cat="Cont.(15%)")
  tst90.corr1<-plotdat(data.set=22:28, N=90, est=perf, scen=5, scen.cat="Corr.(rho=0.3)")
  tst90.corr2<-plotdat(data.set=22:28, N=90, est=perf, scen=6, scen.cat="(rho=0.3,0.4)")
  tst90.corr3<-plotdat(data.set=22:28, N=90, est=perf, scen=7, scen.cat="(rho=0.3,0.4,0.5)")
  
  tst110.ord<-plotdat(data.set=29:35, N=110, est=perf, scen=1, scen.cat="Ordinary")
  tst110.cont1<-plotdat(data.set=29:35, N=110, est=perf, scen=2, scen.cat="Cont.(5%)")
  tst110.cont2<-plotdat(data.set=29:35, N=110, est=perf, scen=3, scen.cat="Cont.(10%)")
  tst110.cont3<-plotdat(data.set=29:35, N=110, est=perf, scen=4, scen.cat="Cont.(15%)")
  tst110.corr1<-plotdat(data.set=29:35, N=110, est=perf, scen=5, scen.cat="Corr.(rho=0.3)")
  tst110.corr2<-plotdat(data.set=29:35, N=110, est=perf, scen=6, scen.cat="(rho=0.3,0.4)")
  tst110.corr3<-plotdat(data.set=29:35, N=110, est=perf, scen=7, scen.cat="(rho=0.3,0.4,0.5)")
  
  tst<-rbind(tst30.ord,tst30.cont1,tst30.cont2,tst30.cont3,tst30.corr1,tst30.corr2,tst30.corr3,
             tst50.ord,tst50.cont1,tst50.cont2,tst50.cont3,tst50.corr1,tst50.corr2,tst50.corr3,
             tst70.ord,tst70.cont1,tst70.cont2,tst70.cont3,tst70.corr1,tst70.corr2,tst70.corr3,
             tst90.ord,tst90.cont1,tst90.cont2,tst90.cont3,tst90.corr1,tst90.corr2,tst90.corr3,
             tst110.ord,tst110.cont1,tst110.cont2,tst110.cont3,tst110.corr1,tst110.corr2,tst110.corr3)
  tst$size<-factor(tst$size, levels=c('N=30/arm','N=50/arm','N=70/arm','N=90/arm','N=110/arm'))
  tst$scenario<-factor(tst$scenario, levels=c("Ordinary","Cont.(5%)","Cont.(10%)","Cont.(15%)",
                                              "Corr.(rho=0.3)", "(rho=0.3,0.4)","(rho=0.3,0.4,0.5)"))
  return(tst)
}


#BIAS ####
bias.dat<-plotdat2("BIAS")
ggplot(bias.dat, aes(x=Ratio, y=BIAS, group = Estimator, color=Estimator)) + geom_line() + geom_point() + 
  facet_grid(scenario~size)  + ggtitle("Bias: Y~X") +
  geom_hline(yintercept = 0, color = "blue", linetype = 2, linewidth = 1) +
  scale_x_continuous(breaks = seq(1, 7, by = 1)) + labs(x='Match Ratio') +
#coord_cartesian(ylim=c(-0.025, 0.15))
coord_cartesian(ylim=c(-0.025, 0.1))


#EmpSE####
empse.dat<-plotdat2("EmpSE")
ggplot(empse.dat, aes(x=Ratio, y=EmpSE, group = Estimator, color=Estimator)) + geom_line() + geom_point() + 
  facet_grid(scenario~size)  + ggtitle("Empirical Standard Error (EmpSE): Y~X") +
#  geom_hline(yintercept = 0, color = "blue", linetype = 2, linewidth = 1) +
  scale_x_continuous(breaks = seq(1, 7, by = 1)) +
#coord_cartesian(ylim=c(0, 1))
coord_cartesian(ylim=c(0.2, 0.4))


#MSE####
mse.dat<-plotdat2("MSE")
ggplot(mse.dat, aes(x=Ratio, y=MSE, group = Estimator, color=Estimator)) + geom_line() + geom_point() + 
  facet_grid(scenario~size)  + ggtitle("Mean Square Error (MSE): Y~X") +
#  geom_hline(yintercept = 0, color = "blue", linetype = 2, linewidth = 1) +
  scale_x_continuous(breaks = seq(1, 7, by = 1)) + labs(x='Match Ratio') +
  #coord_cartesian(ylim=c(0, 1))
  coord_cartesian(ylim=c(0.0375, .3))

#COV####
cov.dat<-plotdat2("COV")
ggplot(cov.dat, aes(x=Ratio, y=COV, group = Estimator, color=Estimator)) + geom_line() + geom_point() + 
  facet_grid(scenario~size)  + ggtitle("Coverage (COV): Y~X") +
  #  geom_hline(yintercept = 0, color = "blue", linetype = 2, linewidth = 1) +
  scale_x_continuous(breaks = seq(1, 7, by = 1))

#Power####
pwr.dat<-plotdat2("PWR")
ggplot(pwr.dat, aes(x=Ratio, y=PWR, group = Estimator, color=Estimator)) + geom_line() + geom_point() + 
  facet_grid(scenario~size)  + ggtitle("Power (PWR): Y~X") +
  geom_hline(yintercept = 0.8, color = "blue", linetype = 2, linewidth = 1) + labs(x='Match Ratio') +
  scale_x_continuous(breaks = seq(1, 7, by = 1))


xz.dat<-plotdat2("XZ")
ggplot(xz.dat[xz.dat$Estimator=="Alpha",], aes(x=Ratio, y=XZ, group = Estimator, color=Estimator)) + geom_line() + geom_point() + 
  facet_grid(scenario~size) + ggtitle("Alpha: X~Z") +
  geom_hline(yintercept = 0.05, color = "blue", linetype = 2, linewidth = 1)

ggplot(xz.dat[xz.dat$Estimator=="Power",], aes(x=Ratio, y=XZ, group = Estimator, color=Estimator)) + geom_line() + geom_point() + 
  facet_grid(scenario~size) + ggtitle("Power: X~Z") +
  geom_hline(yintercept = 0.8, color = "blue", linetype = 2, linewidth = 1)

ggplot(xz.dat[xz.dat$Estimator=="Bias",], aes(x=Ratio, y=XZ, group = Estimator, color=Estimator)) + geom_line() + geom_point() + 
  facet_grid(scenario~size) + ggtitle("Bias: X~Z") +
  geom_hline(yintercept = 0, color = "blue", linetype = 2, linewidth = 1)

#ggplot(xz.dat, aes(x=Ratio, y=XZ, group = Estimator, color=Estimator)) + geom_line() + geom_point() + 
#  facet_grid(scenario~size, scales = "free")


#Rough codes
ggplot(dat.plt2, aes(x=V2, y=V1, group = V3, color=V3)) + geom_line() +geom_point()
ggplot(dat.plt.bth, aes(x=V2, y=V1, group = V3, color=V3)) + geom_line() +geom_point() + 
  facet_wrap(~size) + coord_cartesian(ylim=c(0, 0.2))
