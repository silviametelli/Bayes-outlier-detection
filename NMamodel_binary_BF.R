NMAmodel_binary_BF=function(){    
  
  #M ~ dcat(mPriorProb[]) ## to get BFs
  #mPriorProb[1] <- .5
  #mPriorProb[2] <- .5
 
  for(i in 1:ns){ ## loop through studies
    w[i,1] <- 0 ## adjustment for multi-arm set to 0 in baseline arm 
    delta[i,t[i,1]] <- 0 ## treatment effect set to 0 in baseline arm
    mu[i] ~ dnorm(0, 0.001) ## vague normal priors for baseline effects (LORs in baseline arm of study i)
    
    for(k in 1:na[i]){ ## loop through arms within each study i 
      r[i,t[i,k]] ~ dbin(p[i,t[i,k]], n[i,t[i,k]]) ## number of events (binomial likelihood)
      rhat[i,t[i,k]] <- p[i,t[i,k]] * n[i,t[i,k]] ## expected number of events 
      dev[i, k] <- 2 * (r[i,t[i,k]] * (log(r[i,t[i,k]]) -log(rhat[i,t[i,k]])) 
                        + (n[i,t[i,k]]-r[i,t[i,k]])*(log(n[i,t[i,k]]-r[i,t[i,k]]) - log(n[i,t[i,k]]-rhat[i,t[i,k]])))  # Arm deviance contribution
    }
    
    logit(p[i,t[i,1]]) <- mu[i]
    dev_res[i] <- sum(dev[i,1:na[i]])  ## residual deviance D_res for each study
    
    for(k in 2:na[i]){
      logit(p[i,t[i,k]]) <- mu[i] + delta[i,t[i,k]] + nu[i,t[i,k]] ## model for linear predictor
      delta[i,t[i,k]] ~ dnorm(md[i,t[i,k]], precd[i,t[i,k]]) ## distribution of random effects (trial-specific LORs)
      
      ## account for correlation in multi-arm trials 
      md[i,t[i,k]] <- mean[i,k] + sw[i,k]  ## mean of LOR distributions (with multi-arm correction)
      nu[i,t[i,k]] <- mean_nu[i,k] + sw[i,k]  ## mean of LOR distributions (with multi-arm correction)
      w[i,k] <- (delta[i,t[i,k]] - mean[i,k])
      sw[i,k] <- sum(w[i,1:(k-1)]) / (k-1) ## cumulative adjustment for multi-arm trials
      precd[i, t[i, k]] <- prec * 2 * (k-1)/k ## precision of LORs (with multi-arm correction)
      mean[i,k] <- (d[t[i,k]] - d[t[i,1]]) ## consistency 
      mean_nu[i,k] <- (b[t[i,k]] - b[t[i,1]]) ## consistency 
    }
  }
  
  dev_res_tot <- sum(dev_res[]) ## total residual deviance 
  
  ##prior distribution for log-odds in baseline arm of study i
  for (i in 1:ns) {u[i] ~ dnorm(0,.001)}
  ## prior for heterogeneity (precision for tau)
  tau ~ dunif(0,5)
  prec <- 1 / pow(tau, 2)
  tau.sq <- pow(tau, 2)
  ## priors for basic parameters 
  d[ref] <- 0 ## treatment effect set to 0 for reference treatment 
  b[ref] <- 0
  for(k in 1:(ref-1)) {
    d[k] ~ dnorm(0,0.001)
    b[k] ~ dnorm(0,0.001)
    }
  for(k in (ref+1):nt) {
    d[k] ~ dnorm(0,0.001)
    b[k] ~ dnorm(0,0.001)
    }
  
  ##ORs/LORs for reference
  for(j in 1:(ref-1)){ 
    ORref[j] <- exp(d[j]-d[ref])
    LORref[j] <- d[j]-d[ref]
  }
  for(j in (ref+1):nt) {
    ORref[j] <- exp(d[j]-d[ref])
    LORref[j] <- d[j]-d[ref]
  }
  
  ##ORs/LORs for all pairs of contrasts
  for(i in 1:(nt-1)){
    for (j in (i+1):nt) {
      OR[j,i] <- exp(d[j]-d[i])
      LOR[j,i] <- d[j]-d[i]
    }
  }
}
