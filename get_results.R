get_results <- function(JAGSobject, 
                        parameter=parameter, 
                        treatnames=NA, 
                        rounding=3){
          
                if(!require(stringr)) {install.packages("stringr"); library(stringr)}
  
                results_all <- JAGSobject$BUGSoutput$summary
                names <- rownames(results_all)
                rowsmatching <- ubstr(names,1,nchar(parameter))
                filter_rows_first <- startsWith(rowsmatching,parameter)
                
                slctd_results <- results_all[filter_rows_first,c(1,3,7,2)]
                filter_rows <- startsWith(dimnames(slctd_results)[[1]], paste(parameter,"[",sep=""))
                slctd_results <- slctd_results[filter_rows,]
                tosplit <- unlist(strsplit(rownames(slctd_results),","))
                tosplit2 <- as.numeric(str_extract(tosplit, "[0-9]+"))
                trts_nbr <- max(tosplit2)
                location <- matrix(tosplit2,ncol=2,byrow=T)
                meanmat <- CIs <- sdmat <- matrix(NA,nrow=trts_nbr,ncol=trts_nbr)
  
                for(i in 1:nrow(location)){
                  meanmat[location[i,1],location[i,2]] <- slctd_results[i,1]
                  sdmat[location[i,1],location[i,2]] <- slctd_results[i,4]
                  CIs[location[i,1],location[i,2]] <- slctd_results[i,3] #upper CI
                  CIs[location[i,2],location[i,1]] <- slctd_results[i,2] #lower CI
                  
                }
                meanmat <- round(meanmat,rounding)
                CIs <- round(CIs,rounding)
                Ttreat <- dim(meanmat)[1]
                toprintmat <- matrix(nrow=Ttreat,ncol=Ttreat)
                
                for(i in c(1:(Ttreat-1))){
                  for (j in c((i+1):Ttreat)){
                  toprintmat[i,j] <- paste(meanmat[i,j],"(",CIs[j,i],",",CIs[i,j],")",sep="")
                  toprintmat[j,i] <- paste(c(-meanmat[i,j]),"(",c(-CIs[i,j]),",",c(-CIs[j,i]),")",sep="")
                }}
                if(!missing(treatnames)){
                  diag(meanmat) <- treatnames
                  diag(CI) <- treatnames
                  diag(toprintmat) <- treatnames
                }
                
                list(Means=meanmat, CI=CIs, 
                     leaguetable=toprintmat)
}

