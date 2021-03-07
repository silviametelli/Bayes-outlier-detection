library(tidyverse)
library(mvtnorm)

#---------------------------------* generate data  *-----------------------------------#

set.seed(123)

simulate_nma_data <- function(data, n.iter=10000){
  
  tau2 <- c(0.000, 0.032, 0.096, 0.287)
  n.outliers <- c(1, 3) ### n. outliers
  Np.min <- 50 ### min n. pts per arm
  Np.max <- 200 ### max n. pts per arm
  ### true params
  trueLOR <- c(0, seq(1/(N.t-1), 1, by = 1/(N.t-1)))
  trueOR <- c(1,exp(LOR))
  all_scenarios <- list()
  
  for (t in 1:length(tau2)){
    simulated_data_t  <- list()
    for (o in 1:length(n.outliers)){
      simulated_data_o  <- list()
      for (i in 1:n.iter){ 
        nma <- list()
        data$n1 <- data$n2 <- round(runif(length(data$studlab), Np.min, Np.max), 0)
        for(j in 1:max(data$studlab)){
          studlab.temp_id <- unique(data$studlab)[j]
          studlab.temp <- data[which(data$studlab == studlab.temp_id), ]
          Sigma.temp <- matrix(tau2[t]/2, nrow(studlab.temp), nrow(studlab.temp)) 
                        + diag(tau2[t], nrow(studlab.temp))
          true_theta.temp <- trueLOR[studlab.temp$t1] - trueLOR[studlab.temp$t2]
          theta.temp <- rmvnorm(1, true_theta.temp, Sigma.temp)
          if(t==1 | t==2){
            se.theta.temp <- runif(1,0.5,1)
            }else if(t==3){se.theta.temp <- runif(1,1,2)}else{se.theta.temp <- runif(1,2,3)}
          p.temp_ <- runif(1, 0.4, 0.6)
          p.ref <- rep(p.temp_, nrow(studlab.temp))
          studlab.temp <- studlab.temp %>% 
                          mutate(p.ref=p.ref, true_theta=true_theta.temp, 
                                 theta=as.vector(theta.temp), se_theta=se.theta.temp)
          studlab.temp$p1 <- rep(p.temp_, nrow(studlab.temp))
          studlab.temp$x1 <- rbinom(1, prob=p.temp_, size=data$n1[j])
          studlab.temp$p2 <- (studlab.temp$p1 * exp(studlab.temp$theta))/
                             (1 - studlab.temp$p1 + studlab.temp$p1 *  exp(studlab.temp$theta))
          studlab.temp$x2 <- rbinom(nrow(studlab.temp), prob=studlab.temp$p2, data$n2[j]) 
          nma <- rbind(nma, studlab.temp)
        } 
        ## contamination (NB: assumes normal effect sizes - not heavy tail)
        se.max <- max(nma$se_theta)
        C1 <- 2*sqrt(se.max+tau2[t])
        C2 <- 3*sqrt(se.max+tau2[t])
        studlab_out <- sample(1:max(nma$studlab),n.outliers[o])
        nma_out <- nma[which(nma$studlab %in% studlab_out), ]
        if(rbernoulli(1, 0.5)){
          if(rbernoulli(1, 0.5)){
            nma_out[!duplicated(nma_out$studlab), ]$theta <- 
                                        rnorm(n.outliers[o], nma_out$true_theta[1] + C1, tau2[t])}
          else{nma_out[!duplicated(nma_out$studlab), ]$theta <- 
                                        rnorm(n.outliers[o], nma_out$true_theta[1] - C1, tau2[t])}
        }else{if(rbernoulli(1, 0.5)){nma_out[!duplicated(nma_out$studlab),]$theta <- 
                                        rnorm(n.outliers[o], nma_out$true_theta[1] + C2, tau2[t])}
              else{nma_out[!duplicated(nma_out$studlab), ]$theta <- 
                                        rnorm(n.outliers[o], nma_out$true_theta[1] + C2, tau2[t])}}
        nma_out$p2 <- (nma_out$p1 * exp(nma_out$theta))/(1 - nma_out$p1 + nma_out$p1 * exp(nma_out$theta))
        nma_out$x2 <- rbinom(nrow(nma_out), prob=nma_out$p2, data$n2[j]) 
        nma[which(nma$studlab %in% studlab_out), ] <- nma_out
        simulated_data_o[[i]] <- nma
        rm(nma)
      }
      simulated_data_t[[o]] <- simulated_data_o
    }
    all_scenarios[[t]] <- simulated_data_t
  }
   return(all_scenarios)
}


#-------------------------* generate network geometries (D0,D1,D2,D3,D4) *-------------------------#

## balanced design fully connected (D0)
N.t <- 5 #### n.treats
N.s <- 10 #### n.stud per comparison
t1.1 <- c()
t2.1 <- c()
for (i in 1:(N.t-1)){
  for (k in (i+1):(N.t)){
    for(j in 1:N.s){
      t1.1 <- c(t1.1,i)
      t2.1 <- c(t2.1,k) 
    }}}
## balanced design fairly connected (D1)
t1.1 <- c(rep(1,10),rep(1,10),rep(2,10),rep(3,10),rep(2,10),rep(2,10))
t2.1 <- c(rep(2,10),rep(4,10),rep(3,10),rep(4,10),rep(4,10),rep(5,10))
## unbalanced design well-connected (D2)
t1.2 <- c(rep(1,6),rep(1,9),rep(2,3),rep(3,2),rep(2,4),rep(2,1),rep(1,7),rep(3,1),rep(1,2))
t2.2 <- c(rep(2,6),rep(4,9),rep(3,3),rep(4,2),rep(4,4),rep(5,1),rep(3,7),rep(5,1),rep(5,2))
## unbalanced design fairly connected (D3)
t1.3 <- c(rep(1,2),rep(1,8),rep(2,3),rep(3,4),rep(2,4),rep(2,1),rep(1,5))
t2.3 <- c(rep(2,2),rep(5,8),rep(3,3),rep(4,4),rep(4,4),rep(5,1),rep(4,5))
## unbalanced design poorly connected (D4)
t1.4 <- c(rep(1,1),rep(2,5),rep(2,4),rep(3,2),rep(3,1),rep(2,2))
t2.4 <- c(rep(2,1),rep(5,5),rep(4,4),rep(4,2),rep(4,1),rep(3,2))

#-----------------------------------* generate network plots *---------------------------------#

png("~/simulated_nets.png", 1000, 300)
par(mfrow=c(1,4))
networkplot(as.character(t1.1), as.character(t2.1))
networkplot(as.character(t1.2), as.character(t2.2))
networkplot(as.character(t1.3), as.character(t2.3))
networkplot(as.character(t1.4), as.character(t2.4))
dev.off()

## studlab (two [85%]; three-arm [15%])
n.studlab.1 <- dim(cbind(t1.1,t2.1))[1] - round(dim(cbind(t1.1,t2.1))[1]*0.15,0) 
n.studlab.2 <- dim(cbind(t1.2,t2.2))[1] - round(dim(cbind(t1.2,t2.2))[1]*0.15,0) 
n.studlab.3 <- dim(cbind(t1.3,t2.3))[1] - round(dim(cbind(t1.3,t2.3))[1]*0.15,0) 
n.studlab.4 <- dim(cbind(t1.4,t2.4))[1] - round(dim(cbind(t1.4,t2.4))[1]*0.15,0) 

studlab.1 <- seq(1,dim(cbind(t1.1,t2.1))[1])
studlab.1[c(11,12,13,14,15,41,42,51,52)] <- c(1,2,3,4,5,29,30,49,50)
data.D1 <- data.frame(t1.1, t2.1, as.numeric(as.factor(studlab.1)), LOR)
colnames(data.D1) <- c("t1", "t2", "studlab", "LOR")
data.D1 <- data.D1[order(data.D1$studlab), ]
print(length(c(11,12,13,14,15,41,42,51,52)) == length(t1.1) - n.studlab.1)

studlab.2 <- seq(1,dim(cbind(t1.2,t2.2))[1])
studlab.2[c(7,8,21,34,35)] <- c(1,2,16,31,32)
data.D2 <- data.frame(t1.2, t2.2, as.numeric(as.factor(studlab.2)), LOR)
colnames(data.D2) <- c("t1", "t2", "studlab", "LOR")
data.D2 <- data.D2[order(data.D2$studlab), ]

studlab.3 <- seq(1,dim(cbind(t1.3,t2.3))[1])
studlab.3[c(18,19,1,2)] <- c(11,12,26,27)
data.D3 <- data.frame(t1.3, t2.3, as.numeric(as.factor(studlab.3)), LOR)
colnames(data.D3) <- c("t1", "t2", "studlab", "LOR")
data.D3 <- data.D3[order(data.D3$studlab), ]

studlab.4 <- seq(1,dim(cbind(t1.4,t2.4))[1])
studlab.4[c(7,15)] <- c(2,9)
data.D4 <- data.frame(t1.4, t2.4, as.numeric(as.factor(studlab.4)), LOR)
colnames(data.D4) <- c("t1", "t2", "studlab", "LOR")
data.D4 <- data.D4[order(data.D4$studlab), ]


# *--------------------------- save scenarios ------------------------------* #

##D1
scenarios.S1_S8 = simulate_nma_data(data.D1, n.iter=1000)
save(scenarios.S1_S8, file = "Simulated_Data/Scenarios_S1-S8.RData")
##D2
scenarios.S9_S16 = simulate_nma_data(data.D2, n.iter=1000)
save(scenarios.S9_S16, file = "Simulated_Data/Scenarios_S9-S16.RData")
##D3
scenarios.S17_S24 = simulate_nma_data(data.D3, n.iter=1000)
save(scenarios.S17_S24, file = "Simulated_Data/Scenarios_S17-S24.RData")
##D4
scenarios.S25_S32 = simulate_nma_data(data.D4, n.iter=1000)
save(scenarios.S25_S32, file = "Simulated_Data/Scenarios_S25_S32.RData")


