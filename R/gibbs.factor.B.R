gibbs.factor.B <-function(S, F ,y, m0, c0, k){
  
  library(mvtnorm)
  obs<-nrow(y)
  
  B<-matrix(0,obs,k)
  m<-m0
  c<-c0
  cinv<-solve(c0)
  
  for(j in 1:k){
    
    b<-rmvnorm(1, mean = m, sigma = solve(c) )
              
    if (b[j]<0) 
    {
       b<-b*0
    }
    
    
      
    m<-c %*% (cinv %*% m0 + 1/(S[j,j]) %*% t(F[j,]) %*% y[j,])
    c<-cinv %*% matrix(1,j,j) + 1/S[j,j] %*% t(F[j,]) %*% F[j,]
    
    B[j,]<-t(b)
  }
  
  for(j in k+1:obs){
    
    b<-rmvnorm(1, mean = m, sigma = solve(c) )
    
    
    m<-c %*% (cinv %*% m0 + 1/(S[i,i]) %*% t(F) %*% y[i,])
    c<-cinv %*% matrix(1,i,i) + 1/S[i,i] %*% t(F) %*% F
    
    B[j,]<-t(b)
  }
  