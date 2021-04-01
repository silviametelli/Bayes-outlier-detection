library(parallel)
library(foreach)
library(doParallel)

#setup parallel
cores = detectCores()
cl = makeCluster(cores[1]-1) #avoid overload
registerDoParallel(cl)

#stop cluster
stopCluster(cl)