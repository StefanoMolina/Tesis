gibbs.factor.B <-function(S, F ,y, c0, k, B_0){
  
  library(mvtnorm)
  obs<-nrow(y)
  
  B<-matrix(0,obs,k)
  
  c<-c0
  m<-B_0[1,]
  m0<-colMeans(B_0)
  cinv<-solve(c0)
  
  for(j in 1:k){
    
    b<-rmvnorm(1, mean = m, sigma = solve(c) )
              
    if (b[j]<0) 
    {
       b<-b*0
    }
    
    m<-c %*% (cinv %*% m0 + 1/(S[j,j]) %*% t(F[,1:j]) %*% y[,j])
    c<-cinv %*% matrix(1,j,j) + 1/S[j,j] %*% t(F[,1:j]) %*% F[,1:j]
    
    B[j,]<-t(b)
  }
  
  for(j in k+1:obs){
    
    b<-rmvnorm(1, mean = m, sigma = solve(c) )
    
    
    m<-c %*% (cinv %*% m0 + 1/(S[j,j]) %*% t(F) %*% y[j,])
    c<-cinv %*% matrix(1,j,j) + 1/S[j,j] %*% t(F) %*% F
    
    B[j,]<-t(b)
  }
  
  return(B)
}
  