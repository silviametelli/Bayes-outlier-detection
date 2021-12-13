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
library(ggplot2)
library(bayesplot)
#---------------------------------------------------------------------------------------------------------*

#setwd("/Users/silvia/Google Drive/Paris AP/NMA Project/NMA Outliers/Bayes NMA outliers codes")

##### elements to be passed:
## 1. treatments vector
## 2. study IDs 
## 3. number of event vector (if binary data)
## 4. number of randomized participant vector (if binary data)
## 5. OPTIONAL: reference treatment (if different from 1)

#### simple MCMC model run 
source("NMAmodel_binary.R")
source("example_data_binary.R")
source("make.jagsNMA.data.R")
source("pppc_search.R")
source("MCMC_run.R")

#### type of data and parameters to save
network <- list()
network$id <- current_data$study
network$n <- length(unique(current_data$study))
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

#----------------------------------------* run NMA model  #----------------------------------------*

jags_object <- MCMC_run("NMAmodel_binary", ref=sort(unique(network$treat))[3], network=network) ## run jags module
results <- jags_object$BUGSoutput$summary ## all results
traceplot(jags_object)

## get results: CIs and league table
all_names <- rownames(results)
results_all_final <- results[,c(1,3,7,2)]
leaguetable <- get_results(jags_object,"OR")


#-----------------------------------* run Bayes factors search  #-----------------------------------*

ALL_BFs <- list()
ALL <- list()
bf <- c()
N <- network$n 
for(i in N){
  network <- list()
  current_data <- current_data[-which(current_data$study==1), ]
  network$id <- current_data$study
  network$n <- length(unique(current_data$study))
  network$n <- length(unique(network$id))
  network$treat <- as.numeric(as.factor(current_data$t))
  network$n_response <- current_data$responders
  network$n_random <- current_data$sampleSize
  network$ref <- current_data_ref
  network$data_type <- "binary"
  jags_object <- MCMC_run("bf", network=network) ## run jags module
  ALL_BFs[[i]] <- jags_object
  m1 <- cbind(jags_object$BUGSoutput$summary [[1]], jags_object$BUGSoutput$summary [[2]],
              jags_object$BUGSoutput$summary [[3]], jags_object$BUGSoutput$summary [[4]])  
  network <- list()
  current_data <- current_data[-which(current_data$study==1), ]
  network$id <- current_data$study
  network$n <- length(unique(current_data$study))
  network$n <- length(unique(network$id))
  network$treat <- as.numeric(as.factor(current_data$t))
  network$n_response <- current_data$responders
  network$n_random <- current_data$sampleSize
  network$ref <- current_data_ref
  network$data_type <- "binary"
  jags_object <- MCMC_run("NMAmodel_binary", network=network) ## run jags module
  ALL[[i]] <- jags_object
  m0 <- cbind(jags_object$BUGSoutput$summary [[1]], jags_object$BUGSoutput$summary [[2]],
              jags_object$BUGSoutput$summary [[3]], jags_object$BUGSoutput$summary [[4]])
  bf[i]  = c(bf, (mean(m1)/mean(m0)) / ((1-mean(m1))/(1-mean(m0))) )
  
}

#----------------------------------------* run pppc search #-----------------------------------------* 

#L
jags_object <- MCMC_run("pppc1",  network=network, 
                        params=c("OR", "tau", "yrep") ) 
results <- jags_object$BUGSoutput$summary

#DO
jags_object <- MCMC_run("pppc2",  network=network, 
                        params=c("OR", "tau", "yrep") ) 
results <- jags_object$BUGSoutput$summary
yrep <- jags_object$BUGSoutput$summary[,8]
yrep <- yrep[1:1000]
#####

d <- function(x){
  sum <- 0
  for (i in 1:length(x)){
    (x[i] - median(x))/mad(x)
  }
  sum = sum + val
} 

# defining a method
x <- list(y = y, yrep = yrep)
class(x) <- "ppc"
pp_check.ppc <- function(object, ..., type = c("multiple", "overlaid")) {
  y <- object[["y"]]
  yrep <- object[["yrep"]]
  switch(match.arg(type),
         multiple = ppc_hist(y, yrep[1:min(8, nrow(yrep)),, drop = FALSE]),
         overlaid = ppc_dens_overlay(y, yrep))
}
pp_check(x)
pp_check(x, type = "overlaid")
#
ppc_stat(y, yrep, stat = "d")
ppc_stat(y, yrep, stat = "max")

