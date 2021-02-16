#-------------------------------------------* generate network plots *--------------------------------------------------------------#
## balanced design: fully connected
N.t=5 #### n.treatments
N.s=10 #### n.studies per comparison
t1.1=c()
t2.1=c()
for (i in 1:(N.t-1)){
  for (k in (i+1):(N.t)){
    for(j in 1:N.s){
      t1.1=c(t1.1,i)
      t2.1=c(t2.1,k) 
    }}}
## balanced design (fairly well connected)
t1.1=c(rep(1,10),rep(1,10),rep(2,10),rep(3,10),rep(2,10),rep(2,10))
t2.1=c(rep(2,10),rep(4,10),rep(3,10),rep(4,10),rep(4,10),rep(5,10))
## unbalanced design (1)
t1.2=c(rep(1,6),rep(1,9),rep(2,3),rep(3,2),rep(2,4),rep(2,1),rep(1,7),rep(3,1),rep(1,2))
t2.2=c(rep(2,6),rep(4,9),rep(3,3),rep(4,2),rep(4,4),rep(5,1),rep(3,7),rep(5,1),rep(5,2))
## unbalanced design (2)
t1.3=c(rep(1,2),rep(1,8),rep(2,3),rep(3,4),rep(2,4),rep(2,1),rep(1,5))
t2.3=c(rep(2,2),rep(5,8),rep(3,3),rep(4,4),rep(4,4),rep(5,1),rep(4,5))
## unbalanced design (3)
t1.4=c(rep(1,1),rep(2,5),rep(2,4),rep(3,2),rep(3,1),rep(2,2))
t2.4=c(rep(2,1),rep(5,5),rep(4,4),rep(4,2),rep(4,1),rep(3,2))


#--------------------------------------* generate data for the following 4 scenarios *------------------------------------#

#### 1/ balanced design (Ns=10)
t1.1=c(rep(1,10),rep(1,10),rep(2,10),rep(3,10),rep(2,10),rep(2,10))
t2.1=c(rep(2,10),rep(4,10),rep(3,10),rep(4,10),rep(4,10),rep(5,10))

tau=c(0, 0.032, 0.096, 0.287) ####  heterogeneity 
Np.min=50 #### min pts per arm
Np.max=200 #### max pts per arm
data=list()
logOR=list()
OR=list()

for (i in 1:N.sim){    
  logOR[[i]]=seq(from =1/(NT-1), to = 1, by = 1/(NT-1)) 
  OR[[i]]=c(1,exp(logOR[[i]]))
  data1[[i]]=data.frame(t1.1,t2.1)
  data1[[i]]$studlab=c(1:(N.stud))
  data1[[i]]$n1=data1[[i]]$n2=round(runif(N.stud,Npmin,Npmax))
}


#### 2/ Well-connected network 


#### generate data 2
data=list()
logOR=list()
OR=list()

#### 3/ Fairly-connected network 


#### generate data 3
data=list()
logOR=list()
OR=list()

#### 4/ Poorly-connected network 



#### generate data 4
data=list()
logOR=list()
OR=list()