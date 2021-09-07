#---------------------------------------------------------------------------------------------------------*

# TITLE: Bayesian model-based outlier detection in network meta analysis 
# AUTHOR: Silvia Metelli
# DATE: 15/06/2020
# PURPOSE: Detecting outlying studies on NMA with binary data using Bayes factors and posterior predictive checks 

#---------------------------------------------------------------------------------------------------------*
library(netmeta)
library(dplyr)
library(plyr)
library(R2jags)
library(R2WinBUGS)
library(rjags)
library(coda)
library(INLA)
library(nmaINLA)
library(jagsUI)
#---------------------------------------------------------------------------------------------------------*

setwd("/Users/silvia/Google Drive/Paris AP/NMA Project/NMA Outliers/OUTLIER DETECTION CODES")

##### elements to be passed to the functions:
## 1. treatments vector
## 2. study IDs 
## 3. number of event vector (if binary data)
## 4. number of randomised participant vector (if binary data)
## 5. OPTIONAL: reference treatment (if different from 1)

#### simple MCMC model run 
source("NMAmodel_binary.R")
source("example_data_binary.R")
source("make.jagsNMA.data.R")
source("MCMC_run.R")

#### type of data and parameters to save
network <- list()
network$id <- current_data$study
network$treat <- as.numeric(as.factor(current_data$t))
network$n_response <- current_data$responders
network$n_random <- current_data$sampleSize
network$ref <- current_data_ref
network$data_type <- "binary"

jags_object <- MCMC_run("NMAmodel_binary") ## run jags module
results <- jags_object$BUGSoutput$summary ## all results
traceplot(jags_object)

## get results: CIs and league table
all_names <- rownames(results)
results_all_final <- results[,c(1,3,7,2)]
leaguetable <- get_results(jags_object,"OR")

results_all_final
leaguetable

#-----------------------------------* run Bayes factors search  #-----------------------------------*

for(i in 1:max(network$id)){
  
  BF_list=BF_serch()
  paste0("BF in favor of mean-shift model: ", 
         round(1/mean(filter(ggs(codaSamples), Parameter == "m")$value) ,3))}

# calculating bayes factor
posterior.M <- unlist(jags.bf.out[,"M"])
posterior.odds <- mean(posterior.M==1)/mean(posterior.M==2)
prior.odds <- prior1/(1-prior1)
bayes.factor <- posterior.odds/prior.odds
bayes.factor
#------------------------------------* run Bayes factors search #------------------------------------* 

ppc_list=ppc_search()

pp.check(x, observed, simulated, xlab=NULL, ylab=NULL, main=NULL)

#### save results 


