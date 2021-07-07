get_results <- function(JAGSobject, 
                        parameter=parameter, 
                        treatnames=NA, 
                        rounding=3){
                resultstable=JAGSobject$BUGSoutput$summary
                names=rownames(resultstable)
                rowsmatching=substr(names,1,nchar(parameter))
                rowstokeep=startsWith(rowsmatching,parameter)
                
                resultstabletokeep=resultstable[rowstokeep,c(1,3,7,2)]
                rowstokeep2=startsWith(dimnames(resultstabletokeep)[[1]], paste(parameter,"[",sep=""))
                resultstabletokeep=resultstabletokeep[rowstokeep2,]
                library(stringr)
                tosplit=unlist(strsplit(rownames(resultstabletokeep),","))
                tosplit2=as.numeric(str_extract(tosplit, "[0-9]+"))
                nroftreatments=max(tosplit2)
                location=matrix(tosplit2,ncol=2,byrow=T)
                meanmat=CIs=sdmat=matrix(NA,nrow=nroftreatments,ncol=nroftreatments)
  
                for(i in 1:nrow(location)){
                  meanmat[location[i,1],location[i,2]]=resultstabletokeep[i,1]
                  sdmat[location[i,1],location[i,2]]=resultstabletokeep[i,4]
                  CIs[location[i,1],location[i,2]]=resultstabletokeep[i,3]#high CI
                  CIs[location[i,2],location[i,1]]=resultstabletokeep[i,2]#low CI
                  
                }
                meanmat=round(meanmat,rounding)
                CIs=round(CIs,rounding)
                Ttreat=dim(meanmat)[1]
                toprintmat=matrix(nrow=Ttreat,ncol=Ttreat)
                
                for(i in c(1:(Ttreat-1)))
                {for (j in c((i+1):Ttreat))
                {
                  toprintmat[i,j]=paste(meanmat[i,j],"(",CIs[j,i],",",CIs[i,j],")",sep="")
                  toprintmat[j,i]=paste(c(-meanmat[i,j]),"(",c(-CIs[i,j]),",",c(-CIs[j,i]),")",sep="")
                }}
                if(!missing(treatnames)){
                  diag(meanmat)=treatnames
                  diag(CI)=treatnames
                  diag(toprintmat)=treatnames
                }
                
                list(Means=meanmat,CI=CIs, leaguetable=toprintmat)
}