gibbs.factor.1 <- function(y, k, v, c0, M, dynamic)
{
 
  
  # y   -> matriz de datos
  # k   -> numero de factores
  # v   -> hiperparametro para Ginv en gibbs.factor.sigma
  # c0  -> hiperparametro para normal en gibbs.factor.B
  # M   -> numero de iteraciones de Gibbs
  
  library(matrixStats)
  
  B_aux<-gibbs.factor.B.prior(y, c0, k)
  
  #s<-colVars(as.matrix(y))
  s<-matrix(.5, ncol(y), 1)
  
  Sigma_aux<-gibbs.factor.Sigma.prior(v,s)
  
  samp<-M/20
  
  f_rep<-array(data=NA,dim=c(samp,nrow(y),k))
  Sigma_rep<-array(data=NA,dim=c(samp,nrow(Sigma_aux),ncol(Sigma_aux)))
  B_rep<-array(data=NA,dim=c(samp,nrow(B_aux),ncol(B_aux)))
  
  n<-1
  for(j in 1:M){
    
    if(dynamic==1 & j>1){
    F_aux<-gibbs.factor.f.dynamic(Sigma_aux, B_aux ,y, k, fi)
    }
    else{
    F_aux<-gibbs.factor.f(Sigma_aux, B_aux ,y, k)
    }
    
    Sigma_aux<-gibbs.factor.sigma(F_aux, B_aux ,y, v, s, k)
    
    B_aux<-gibbs.factor.B(Sigma_aux, F_aux ,y, c0, k, B_aux )
    print(B_aux)
    
    if(j>M/2 & j%%10==0){
    f_rep[n, ,]<-F_aux
    Sigma_rep[n, , ]<-Sigma_aux
    B_rep[n, , ]<-B_aux
    n<-n+1
    print(n)
    }
  fi<-F_aux  
    
  }
  
  
  
  result <- list(FF=f_rep, Sigma=Sigma_rep, B=B_rep )    
  return(result)
  
}