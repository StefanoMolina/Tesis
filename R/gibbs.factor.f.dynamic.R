gibbs.factor.f.dynamic<-function(S, B ,y, k, fi){
  
  library(mvtnorm)
  
  f<-matrix(0,nrow(y),k)
  B<-as.matrix(B)
  y<-as.matrix(y)
  S<-as.matrix(S)
  

    for (j in 1:nrow(y))
    {
      SS<-solve(diag(1,k)+t(B) %*% S %*% B)
      f[j,]<-rmvnorm(1, 
                     mean=(SS %*% (t(B) %*% S %*% y[j,] + fi[j,])),
                     sigma=SS)
    }   
  
  
  
  
  
  
  return(f)        
}
