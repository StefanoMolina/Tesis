gibbs.factor.f.dynamic <-function(S, B ,y, k){
  
  library(mvtnorm)
  
  f<-matrix(0,k,ncol(y))
  B<-as.matrix(B)
  y<-as.matrix(y)
  S<-as.matrix(S)
  
  for (j in 1:nrow(y))
  {
    SS<-solve(diag(1,k)+t(B) %*% S %*% B)
    f[,j]<-rmvnorm(1, 
                   mean=(SS %*% (t(B) %*% S %*% y[j,] + f[,j-1])),
                   sigma=SS)
  }     
  
  
  return(f)        
}

