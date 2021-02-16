library(parallel)
library(foreach)
library(doParallel)

#setup parallel
cores = detectCores()
cl = makeCluster(cores[1]-1) #avoid overload
registerDoParallel(cl)

nsims = 1000 
datasims = foreach(i=1:nsims, .combine=cbind) %dopar% {



}







#stop cluster
stopCluster(cl)