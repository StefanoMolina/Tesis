gibbs.factor.1 <- function(y, k, v, c0, M)
{
  library(matrixStats)
  
  B_aux<-gibbs.factor.B.prior(y, c0, k)
  
  s<-colVars(as.matrix(y))
  
  Sigma_aux<-gibbs.factor.Sigma.prior(v,s)
  
  
  f_rep<-array(data=NA,dim=c(M,nrow(y),k))
  Sigma_rep<-array(data=NA,dim=c(M,nrow(Sigma_aux),ncol(Sigma_aux)))
  B_rep<-array(data=NA,dim=c(M,nrow(B_aux),ncol(B_aux)))
  
  
  for(j in 1:M){
   
    
    F_aux<-gibbs.factor.f(Sigma_aux, B_aux ,y, k)
    
    Sigma_aux<-gibbs.factor.sigma(F_aux, B_aux ,y, v, s, k)
    
    B_aux<-gibbs.factor.B(Sigma_aux, F_aux ,y, c0, k, B_aux )
    
    f_rep[j, ,]<-F_aux
    Sigma_rep[j, , ]<-Sigma_aux
    B_rep[j, , ]<-B_aux
    
    
    
  }
  
  
  result <- list(FF=f_rep, Sigma=Sigma_rep, B=B_rep )    
  return(result)
  
}
