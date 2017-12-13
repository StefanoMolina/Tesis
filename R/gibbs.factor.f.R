gibbs.factor.f<-function(S, B ,y, k){
  
  library(mvtnorm)
  
  
  f<-matrix(0,nrow(y),k)
  B<-as.matrix(B)
  y<-as.matrix(y)
  S<-as.matrix(S)
  
  for (j in 1:nrow(y))
  {
    
    
    m<-solve(diag(1,k) + t(B) %*% S %*% B)
    f[j,]<-rmvnorm(1, mean=m %*% t(B) %*% S %*% y[j,],
                   sigma=m)
    
  }     
  
  
  return(f)        
}
