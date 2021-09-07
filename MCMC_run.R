#-----------------------------------* MCMC model estimation via jags #-----------------------------------*
MCMC_run=function(model){
  params = c("LOR", "OR", "LORref", "ORref", "tau", "dev_res_tot") 
  
  preparedData <- make.jagsNMA.data(studyid = id, t = treat,
                                   r = n_response, n = n_random, 
                                   type = network$data_type,
                                   reference = network$ref,
                                   data = data.frame(network)[,1:4]
                                   )
    jags_object <<- jags(data=preparedData,inits=NULL, parameters.to.save=params,
                      n.chains=2, n.iter=10000, n.burnin=1000, DIC=TRUE, 
                      model.file=model)
  return(jags_object)
}
#---------------------------------------------------------------------------------------------------------*