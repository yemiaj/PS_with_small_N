library(stringr)

# Read all parameter sets into the workspace and name them correctly
parameter_sets<-mapply(function(x,name){setNames(read.csv(x,header = F),
                                                 c("n.per.arm","rand.rat_1","rand.rat_2","corr.3_1","corr.3_2","corr.3_3","cont.prop_1","cont.prop_2","cont.prop_3","file_set","dxz","dxy",
                                                   "xz.null.ord.beta", "xz.null.ord.se", "xz.null.ord.Z", "xz.null.ord.pv", "xz.null.ord.LL", "xz.null.ord.UL", "xz.null.con1.beta", "xz.null.con1.se", "xz.null.con1.Z", "xz.null.con1.pv",
                                                   "xz.null.con1.LL", "xz.null.con1.UL", "xz.null.con2.beta", "xz.null.con2.se", "xz.null.con2.Z", "xz.null.con2.pv", "xz.null.con2.LL", "xz.null.con2.UL", "xz.null.con3.beta", 
                                                   "xz.null.con3.se", "xz.null.con3.Z", "xz.null.con3.pv", "xz.null.con3.LL", "xz.null.con3.UL", "xz.null.col1.beta", "xz.null.col1.se", "xz.null.col1.Z", "xz.null.col1.pv", 
                                                   "xz.null.col1.LL", "xz.null.col1.UL", "xz.null.col2.beta", "xz.null.col2.se", "xz.null.col2.Z", "xz.null.col2.pv", "xz.null.col2.LL", "xz.null.col2.UL", "xz.null.col3.beta",
                                                   "xz.null.col3.se", "xz.null.col3.Z", "xz.null.col3.pv", "xz.null.col3.LL", "xz.null.col3.UL", "xz.alt.ord.beta", "xz.alt.ord.se", "xz.alt.ord.Z", "xz.alt.ord.pv", "xz.alt.ord.LL",
                                                   "xz.alt.ord.UL", "yx.alt.ord.adj.beta", "yx.alt.ord.adj.se", "yx.alt.ord.adj.t", "yx.alt.ord.adj.pv", "yx.alt.ord.adj.LL", "yx.alt.ord.adj.UL", "yx.alt.ord.cond.beta", 
                                                   "yx.alt.ord.cond.se", "yx.alt.ord.cond.Z", "yx.alt.ord.cond.pv", "yx.alt.ord.cond.LL", "yx.alt.ord.cond.UL", "yx.alt.ord.iptw.beta", "yx.alt.ord.iptw.se", "yx.alt.ord.iptw.t", 
                                                   "yx.alt.ord.iptw.pv", "yx.alt.ord.iptw.LL", "yx.alt.ord.iptw.UL", "yx.alt.ord.aipw.beta", "yx.alt.ord.aipw.se", "yx.alt.ord.aipw.Z", "yx.alt.ord.aipw.pv", "yx.alt.ord.aipw.LL", 
                                                   "yx.alt.ord.aipw.UL", "xz.alt.con1.beta", "xz.alt.con1.se", "xz.alt.con1.Z", "xz.alt.con1.pv", "xz.alt.con1.LL", "xz.alt.con1.UL", "yx.alt.con1.adj.beta", "yx.alt.con1.adj.se",
                                                   "yx.alt.con1.adj.t", "yx.alt.con1.adj.pv", "yx.alt.con1.adj.LL", "yx.alt.con1.adj.UL", "yx.alt.con1.cond.beta", "yx.alt.con1.cond.se", "yx.alt.con1.cond.Z", "yx.alt.con1.cond.pv", 
                                                   "yx.alt.con1.cond.LL", "yx.alt.con1.cond.UL", "yx.alt.con1.iptw.beta", "yx.alt.con1.iptw.se", "yx.alt.con1.iptw.t", "yx.alt.con1.iptw.pv", "yx.alt.con1.iptw.LL", "yx.alt.con1.iptw.UL", 
                                                   "yx.alt.con1.aipw.beta", "yx.alt.con1.aipw.se", "yx.alt.con1.aipw.Z", "yx.alt.con1.aipw.pv", "yx.alt.con1.aipw.LL", "yx.alt.con1.aipw.UL", "xz.alt.con2.beta", "xz.alt.con2.se", 
                                                   "xz.alt.con2.Z", "xz.alt.con2.pv", "xz.alt.con2.LL", "xz.alt.con2.UL", "yx.alt.con2.adj.beta", "yx.alt.con2.adj.se", "yx.alt.con2.adj.t", "yx.alt.con2.adj.pv", "yx.alt.con2.adj.LL", 
                                                   "yx.alt.con2.adj.UL", "yx.alt.con2.cond.beta", "yx.alt.con2.cond.se", "yx.alt.con2.cond.Z", "yx.alt.con2.cond.pv", "yx.alt.con2.cond.LL", "yx.alt.con2.cond.UL", "yx.alt.con2.iptw.beta", 
                                                   "yx.alt.con2.iptw.se", "yx.alt.con2.iptw.t", "yx.alt.con2.iptw.pv", "yx.alt.con2.iptw.LL", "yx.alt.con2.iptw.UL", "yx.alt.con2.aipw.beta", "yx.alt.con2.aipw.se", "yx.alt.con2.aipw.Z", 
                                                   "yx.alt.con2.aipw.pv", "yx.alt.con2.aipw.LL", "yx.alt.con2.aipw.UL", "xz.alt.con3.beta", "xz.alt.con3.se", "xz.alt.con3.Z", "xz.alt.con3.pv", "xz.alt.con3.LL", "xz.alt.con3.UL", 
                                                   "yx.alt.con3.adj.beta", "yx.alt.con3.adj.se", "yx.alt.con3.adj.t", "yx.alt.con3.adj.pv", "yx.alt.con3.adj.LL", "yx.alt.con3.adj.UL", "yx.alt.con3.cond.beta", "yx.alt.con3.cond.se", 
                                                   "yx.alt.con3.cond.Z", "yx.alt.con3.cond.pv", "yx.alt.con3.cond.LL", "yx.alt.con3.cond.UL", "yx.alt.con3.iptw.beta", "yx.alt.con3.iptw.se", "yx.alt.con3.iptw.t", "yx.alt.con3.iptw.pv", 
                                                   "yx.alt.con3.iptw.LL", "yx.alt.con3.iptw.UL", "yx.alt.con3.aipw.beta", "yx.alt.con3.aipw.se", "yx.alt.con3.aipw.Z", "yx.alt.con3.aipw.pv", "yx.alt.con3.aipw.LL", "yx.alt.con3.aipw.UL", 
                                                   "xz.alt.col1.beta", "xz.alt.col1.se", "xz.alt.col1.Z", "xz.alt.col1.pv", "xz.alt.col1.LL", "xz.alt.col1.UL", "yx.alt.col1.adj.beta", "yx.alt.col1.adj.se", "yx.alt.col1.adj.t", 
                                                   "yx.alt.col1.adj.pv", "yx.alt.col1.adj.LL", "yx.alt.col1.adj.UL", "yx.alt.col1.cond.beta", "yx.alt.col1.cond.se", "yx.alt.col1.cond.Z", "yx.alt.col1.cond.pv", "yx.alt.col1.cond.LL", 
                                                   "yx.alt.col1.cond.UL", "yx.alt.col1.iptw.beta", "yx.alt.col1.iptw.se", "yx.alt.col1.iptw.t", "yx.alt.col1.iptw.pv", "yx.alt.col1.iptw.LL", "yx.alt.col1.iptw.UL", "yx.alt.col1.aipw.beta", 
                                                   "yx.alt.col1.aipw.se", "yx.alt.col1.aipw.Z", "yx.alt.col1.aipw.pv", "yx.alt.col1.aipw.LL", "yx.alt.col1.aipw.UL", "xz.alt.col2.beta", "xz.alt.col2.se", "xz.alt.col2.Z", "xz.alt.col2.pv", 
                                                   "xz.alt.col2.LL", "xz.alt.col2.UL", "yx.alt.col2.adj.beta", "yx.alt.col2.adj.se", "yx.alt.col2.adj.t", "yx.alt.col2.adj.pv", "yx.alt.col2.adj.LL", "yx.alt.col2.adj.UL", 
                                                   "yx.alt.col2.cond.beta", "yx.alt.col2.cond.se", "yx.alt.col2.cond.Z", "yx.alt.col2.cond.pv", "yx.alt.col2.cond.LL", "yx.alt.col2.cond.UL", "yx.alt.col2.iptw.beta", "yx.alt.col2.iptw.se", 
                                                   "yx.alt.col2.iptw.t", "yx.alt.col2.iptw.pv", "yx.alt.col2.iptw.LL", "yx.alt.col2.iptw.UL", "yx.alt.col2.aipw.beta", "yx.alt.col2.aipw.se", "yx.alt.col2.aipw.Z", "yx.alt.col2.aipw.pv", 
                                                   "yx.alt.col2.aipw.LL", "yx.alt.col2.aipw.UL", "xz.alt.col3.beta", "xz.alt.col3.se", "xz.alt.col3.Z", "xz.alt.col3.pv", "xz.alt.col3.LL", "xz.alt.col3.UL", "yx.alt.col3.adj.beta", 
                                                   "yx.alt.col3.adj.se", "yx.alt.col3.adj.t", "yx.alt.col3.adj.pv", "yx.alt.col3.adj.LL", "yx.alt.col3.adj.UL", "yx.alt.col3.cond.beta", "yx.alt.col3.cond.se", "yx.alt.col3.cond.Z", 
                                                   "yx.alt.col3.cond.pv", "yx.alt.col3.cond.LL", "yx.alt.col3.cond.UL", "yx.alt.col3.iptw.beta", "yx.alt.col3.iptw.se", "yx.alt.col3.iptw.t", "yx.alt.col3.iptw.pv", "yx.alt.col3.iptw.LL", 
                                                   "yx.alt.col3.iptw.UL", "yx.alt.col3.aipw.beta", "yx.alt.col3.aipw.se", "yx.alt.col3.aipw.Z", "yx.alt.col3.aipw.pv", "yx.alt.col3.aipw.LL", "yx.alt.col3.aipw.UL",
                                                   "rseed1","rseed2","rseed3","rseed4","rseed5","rseed6","rseed7","node","procedure_time","completion_time"))}, 
                       x=list.files(path=paste0(getwd(),"/ResExgAbstract"),full.names=T), name=str_remove_all(list.files(path=paste0(getwd(),"/ResExgAbstract")),pattern=".csv"), SIMPLIFY = FALSE)

# Renaming parameter set list
names(parameter_sets)<-str_remove_all(str_remove_all(names(parameter_sets),pattern=paste0(getwd(),"/ResExgAbstract/")), pattern='.csv')

# Reordering parameter set list
parameter_sets<-parameter_sets[order(as.integer(str_remove_all(names(parameter_sets),pattern="parameter_set_")))]  

parameter_sets<-lapply(parameter_sets,function(x){x<-x[, c("n.per.arm","rand.rat_1","rand.rat_2","corr.3_1","corr.3_2","corr.3_3","cont.prop_1","cont.prop_2","cont.prop_3","file_set","dxz","dxy",
                                                           "xz.null.ord.beta", "xz.null.ord.se", "xz.null.ord.Z", "xz.null.ord.pv", "xz.null.ord.LL", "xz.null.ord.UL", "xz.null.con1.beta", "xz.null.con1.se", "xz.null.con1.Z", "xz.null.con1.pv",
                                                           "xz.null.con1.LL", "xz.null.con1.UL", "xz.null.con2.beta", "xz.null.con2.se", "xz.null.con2.Z", "xz.null.con2.pv", "xz.null.con2.LL", "xz.null.con2.UL", "xz.null.con3.beta", 
                                                           "xz.null.con3.se", "xz.null.con3.Z", "xz.null.con3.pv", "xz.null.con3.LL", "xz.null.con3.UL", "xz.null.col1.beta", "xz.null.col1.se", "xz.null.col1.Z", "xz.null.col1.pv", 
                                                           "xz.null.col1.LL", "xz.null.col1.UL", "xz.null.col2.beta", "xz.null.col2.se", "xz.null.col2.Z", "xz.null.col2.pv", "xz.null.col2.LL", "xz.null.col2.UL", "xz.null.col3.beta",
                                                           "xz.null.col3.se", "xz.null.col3.Z", "xz.null.col3.pv", "xz.null.col3.LL", "xz.null.col3.UL", "xz.alt.ord.beta", "xz.alt.ord.se", "xz.alt.ord.Z", "xz.alt.ord.pv", "xz.alt.ord.LL",
                                                           "xz.alt.ord.UL", "yx.alt.ord.adj.beta", "yx.alt.ord.adj.se", "yx.alt.ord.adj.t", "yx.alt.ord.adj.pv", "yx.alt.ord.adj.LL", "yx.alt.ord.adj.UL", "yx.alt.ord.cond.beta", 
                                                           "yx.alt.ord.cond.se", "yx.alt.ord.cond.Z", "yx.alt.ord.cond.pv", "yx.alt.ord.cond.LL", "yx.alt.ord.cond.UL", "yx.alt.ord.iptw.beta", "yx.alt.ord.iptw.se", "yx.alt.ord.iptw.t", 
                                                           "yx.alt.ord.iptw.pv", "yx.alt.ord.iptw.LL", "yx.alt.ord.iptw.UL", "yx.alt.ord.aipw.beta", "yx.alt.ord.aipw.se", "yx.alt.ord.aipw.Z", "yx.alt.ord.aipw.pv", "yx.alt.ord.aipw.LL", 
                                                           "yx.alt.ord.aipw.UL", "xz.alt.con1.beta", "xz.alt.con1.se", "xz.alt.con1.Z", "xz.alt.con1.pv", "xz.alt.con1.LL", "xz.alt.con1.UL", "yx.alt.con1.adj.beta", "yx.alt.con1.adj.se",
                                                           "yx.alt.con1.adj.t", "yx.alt.con1.adj.pv", "yx.alt.con1.adj.LL", "yx.alt.con1.adj.UL", "yx.alt.con1.cond.beta", "yx.alt.con1.cond.se", "yx.alt.con1.cond.Z", "yx.alt.con1.cond.pv", 
                                                           "yx.alt.con1.cond.LL", "yx.alt.con1.cond.UL", "yx.alt.con1.iptw.beta", "yx.alt.con1.iptw.se", "yx.alt.con1.iptw.t", "yx.alt.con1.iptw.pv", "yx.alt.con1.iptw.LL", "yx.alt.con1.iptw.UL", 
                                                           "yx.alt.con1.aipw.beta", "yx.alt.con1.aipw.se", "yx.alt.con1.aipw.Z", "yx.alt.con1.aipw.pv", "yx.alt.con1.aipw.LL", "yx.alt.con1.aipw.UL", "xz.alt.con2.beta", "xz.alt.con2.se", 
                                                           "xz.alt.con2.Z", "xz.alt.con2.pv", "xz.alt.con2.LL", "xz.alt.con2.UL", "yx.alt.con2.adj.beta", "yx.alt.con2.adj.se", "yx.alt.con2.adj.t", "yx.alt.con2.adj.pv", "yx.alt.con2.adj.LL", 
                                                           "yx.alt.con2.adj.UL", "yx.alt.con2.cond.beta", "yx.alt.con2.cond.se", "yx.alt.con2.cond.Z", "yx.alt.con2.cond.pv", "yx.alt.con2.cond.LL", "yx.alt.con2.cond.UL", "yx.alt.con2.iptw.beta", 
                                                           "yx.alt.con2.iptw.se", "yx.alt.con2.iptw.t", "yx.alt.con2.iptw.pv", "yx.alt.con2.iptw.LL", "yx.alt.con2.iptw.UL", "yx.alt.con2.aipw.beta", "yx.alt.con2.aipw.se", "yx.alt.con2.aipw.Z", 
                                                           "yx.alt.con2.aipw.pv", "yx.alt.con2.aipw.LL", "yx.alt.con2.aipw.UL", "xz.alt.con3.beta", "xz.alt.con3.se", "xz.alt.con3.Z", "xz.alt.con3.pv", "xz.alt.con3.LL", "xz.alt.con3.UL", 
                                                           "yx.alt.con3.adj.beta", "yx.alt.con3.adj.se", "yx.alt.con3.adj.t", "yx.alt.con3.adj.pv", "yx.alt.con3.adj.LL", "yx.alt.con3.adj.UL", "yx.alt.con3.cond.beta", "yx.alt.con3.cond.se", 
                                                           "yx.alt.con3.cond.Z", "yx.alt.con3.cond.pv", "yx.alt.con3.cond.LL", "yx.alt.con3.cond.UL", "yx.alt.con3.iptw.beta", "yx.alt.con3.iptw.se", "yx.alt.con3.iptw.t", "yx.alt.con3.iptw.pv", 
                                                           "yx.alt.con3.iptw.LL", "yx.alt.con3.iptw.UL", "yx.alt.con3.aipw.beta", "yx.alt.con3.aipw.se", "yx.alt.con3.aipw.Z", "yx.alt.con3.aipw.pv", "yx.alt.con3.aipw.LL", "yx.alt.con3.aipw.UL", 
                                                           "xz.alt.col1.beta", "xz.alt.col1.se", "xz.alt.col1.Z", "xz.alt.col1.pv", "xz.alt.col1.LL", "xz.alt.col1.UL", "yx.alt.col1.adj.beta", "yx.alt.col1.adj.se", "yx.alt.col1.adj.t", 
                                                           "yx.alt.col1.adj.pv", "yx.alt.col1.adj.LL", "yx.alt.col1.adj.UL", "yx.alt.col1.cond.beta", "yx.alt.col1.cond.se", "yx.alt.col1.cond.Z", "yx.alt.col1.cond.pv", "yx.alt.col1.cond.LL", 
                                                           "yx.alt.col1.cond.UL", "yx.alt.col1.iptw.beta", "yx.alt.col1.iptw.se", "yx.alt.col1.iptw.t", "yx.alt.col1.iptw.pv", "yx.alt.col1.iptw.LL", "yx.alt.col1.iptw.UL", "yx.alt.col1.aipw.beta", 
                                                           "yx.alt.col1.aipw.se", "yx.alt.col1.aipw.Z", "yx.alt.col1.aipw.pv", "yx.alt.col1.aipw.LL", "yx.alt.col1.aipw.UL", "xz.alt.col2.beta", "xz.alt.col2.se", "xz.alt.col2.Z", "xz.alt.col2.pv", 
                                                           "xz.alt.col2.LL", "xz.alt.col2.UL", "yx.alt.col2.adj.beta", "yx.alt.col2.adj.se", "yx.alt.col2.adj.t", "yx.alt.col2.adj.pv", "yx.alt.col2.adj.LL", "yx.alt.col2.adj.UL", 
                                                           "yx.alt.col2.cond.beta", "yx.alt.col2.cond.se", "yx.alt.col2.cond.Z", "yx.alt.col2.cond.pv", "yx.alt.col2.cond.LL", "yx.alt.col2.cond.UL", "yx.alt.col2.iptw.beta", "yx.alt.col2.iptw.se", 
                                                           "yx.alt.col2.iptw.t", "yx.alt.col2.iptw.pv", "yx.alt.col2.iptw.LL", "yx.alt.col2.iptw.UL", "yx.alt.col2.aipw.beta", "yx.alt.col2.aipw.se", "yx.alt.col2.aipw.Z", "yx.alt.col2.aipw.pv", 
                                                           "yx.alt.col2.aipw.LL", "yx.alt.col2.aipw.UL", "xz.alt.col3.beta", "xz.alt.col3.se", "xz.alt.col3.Z", "xz.alt.col3.pv", "xz.alt.col3.LL", "xz.alt.col3.UL", "yx.alt.col3.adj.beta", 
                                                           "yx.alt.col3.adj.se", "yx.alt.col3.adj.t", "yx.alt.col3.adj.pv", "yx.alt.col3.adj.LL", "yx.alt.col3.adj.UL", "yx.alt.col3.cond.beta", "yx.alt.col3.cond.se", "yx.alt.col3.cond.Z", 
                                                           "yx.alt.col3.cond.pv", "yx.alt.col3.cond.LL", "yx.alt.col3.cond.UL", "yx.alt.col3.iptw.beta", "yx.alt.col3.iptw.se", "yx.alt.col3.iptw.t", "yx.alt.col3.iptw.pv", "yx.alt.col3.iptw.LL", 
                                                           "yx.alt.col3.iptw.UL", "yx.alt.col3.aipw.beta", "yx.alt.col3.aipw.se", "yx.alt.col3.aipw.Z", "yx.alt.col3.aipw.pv", "yx.alt.col3.aipw.LL", "yx.alt.col3.aipw.UL",
                                                           "rseed1","rseed2","rseed3","rseed4","rseed5","rseed6","rseed7","node","procedure_time","completion_time") ]})  
# final<-Reduce(rbind,parameter_sets)
parameter_set_summaries<-lapply(parameter_sets,sumests)






























