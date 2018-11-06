gibbs.factor.f <-function(S, B ,y, k){
  
  library(mvtnorm)
  
  
  f<-matrix(0,k,ncol(y))
  B<-as.matrix(B)
  y<-as.matrix(y)
  
  for (j in 1:ncol(y))
  {
  
    Sinv<-as.matrix(solve(S))
    m<-solve(diag(1,k) + t(B) %*% Sinv %*% B)
  f[,j]<-rmvnorm(1, mean=m %*% t(B) %*% Sinv %*% y[j,],
          sigma=m)
          
  }     
          
          
  return(f)        
}

