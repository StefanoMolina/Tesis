gibbs.factor.f <-function(S, F ,y){
  
  library(mvtnorm)
  
  %La función genera el vector de factores condicionado a las matrices Sigma y F y el vector y.
  
  
  k<-ncol(F)
    Sinv<-solve(S)
  f<-rmvnorm(1, mean=(solve(diag(1,k) + t(F) %*% Sinv %*% F) %*% F %*% Sinv * y),
          sigma=solve(diag(1,k)+t(F) %*% Sinv %*% F)
          
  return(f)        
}

