gibbs.factor.f.dynamic <-function(S, B ,y, k){
  
  library(mvtnorm)
  
  %La funci??n genera el vector de factores condicionado a las matrices Sigma, Y y F.
  
  f<-matrix(0,k,ncol(y))
  
  for (j in 1:ncol(y))
  {
    
    Sinv<-solve(S)
    f[,j]<-rmvnorm(1, 
                   mean=(solve(diag(1,k) + t(B) %*% Sinv %*% B) %*% (t(B)%*% Sinv %*%y[,j]+f[,j-1]),
                   sigma=solve(diag(1,k)+t(B) %*% Sinv %*% B)
  }     
  
  
  return(f)        
}

