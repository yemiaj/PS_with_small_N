library(parallel)

# 1. Setting up the cluster -----------------------------------------------

# Making the cluster and loading all needed packages for each one

n.core<-parallel::detectCores()-2
cl <- makeCluster(n.core)

clusterCall(cl, function(){
  needed_cluster_packages<-c("lubridate","MASS","MatchIt","survival","survey","AIPW","SuperLearner","magrittr")
  for (i in needed_cluster_packages){
    setwd("C:/Users/Owner/OneDrive - The University of Colorado Denver/Research/psm/ResExgAbstract")
    library(i,character.only = TRUE)
  }
  return("Completed")})

# Getting simulation written functions loaded to cluster
clusterExport(cl, varlist = c("datgen","getests","SL.glm.binom","collests"))

# Setting the RngStream to ensure simulation cluster independence
RNGkind("L'Ecuyer-CMRG")
clusterEvalQ(cl, { library(MASS); RNGkind("L'Ecuyer-CMRG") })
clusterEvalQ(cl, { library(MASS); RNGkind("L'Ecuyer-CMRG") })

# 2. Creating the parameter set data frame --------------------------------
# Input data frame
param_set<-expand.grid(n.per.arm=seq(30,110,20), rand.rat_1=1, rand.rat_2=seq(1:7),corr.3_1=.3,corr.3_2=.4,corr.3_3=.5,cont.prop_1=.05,cont.prop_2=.10,cont.prop_3=.15)
param_set<-param_set[order(param_set$n.per.arm),]
param_set<-cbind(param_set, file_set=1:nrow(param_set))
n.delta12.use<-data.frame(N=seq(30,110,20), dxz=c(2.352,1.864,1.670,1.563,1.492), dxy=c(5.032,3.292,2.696,2.383,2.187))
param_set<-merge(param_set,n.delta12.use, by.x = 'n.per.arm', by.y='N')
#del<-expand.grid(n.per.arm=seq(30,110,20), rand.rat_1=1, rand.rat_2=seq(1:7),corr.3_1=.3,corr.3_2=.4,corr.3_3=.5,cont.prop_1=.05,cont.prop_2=.10,cont.prop_3=.15)
#del1<-merge(del,n.delta12.use, by.x = 'n.per.arm', by.y='N')
param_set<-data.frame(t(param_set))

#Save param_set to workspace for posterity
write.csv(param_set, 'param_set.csv')

# 3. Setting up the random number generation ------------------------------

# This is based on the clusterSetRNGStream function from
# the parallel package, copyrighted by The R Core Team
getseeds <- function(ntasks, iseed) {
  RNGkind("L'Ecuyer-CMRG")
  set.seed(iseed)
  seeds <- vector("list", ntasks)
  seeds[[1]] <- .Random.seed
  for (i in seq_len(ntasks - 1)) {
    seeds[[i + 1]] <- nextRNGSubStream(seeds[[i]])
  }
  seeds
}

seeds<-getseeds(n.core,2201959)

# Assigning the seeds to the cluster
clusterApply(cl,seeds[1:n.core],function(x){assign(".Random.seed", x, envir=.GlobalEnv)})

# Creating node identifiers
clusterApply(cl,1:n.core, function(x){assign("node", x, envir=.GlobalEnv)})

# 4. Running the simulation and stopping the cluster ----------------------

# Running the cluster for the simulation
parLapply(cl, param_set, collests, nsim=1000)

# Stopping the cluster
stopCluster(cl)
rm(cl)


#Sys.sleep(5)
#gc()
#Sys.sleep(5)

#system('sudo shutdown -h now', wait = FALSE)




collests<-function(param_vector,nsim=1000,time_required_1=NA,...) {
  
  n.per.arm<-param_vector[1]
  rand.rat<- param_vector[c(2,3)]
  corr.3<-param_vector[c(4,5,6)]
  cont.prop<-param_vector[c(7,8,9)]
  file_set<-param_vector[10]
}  
  