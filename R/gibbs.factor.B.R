gibbs.factor.B <-function(S, FF ,y, c0, k, B_0){
  
  library(mvtnorm)
  obs<-ncol(y)
  
  
  B<-matrix(0,obs,k)
  
  c<-c0
  m<-B_0[1,]
  m0<-colMeans(B_0)
  cinv<-solve(c0)
  
  for(j in 1:obs){
    
    if(j<=k){
     
    Sinv<-drop(S[j,j])
    c<-cinv[1:j,1:j] + Sinv * (t(as.matrix(FF[,1:j])) %*% as.matrix(FF[,1:j]))
    cc<-cinv %*% m0
    m<-c %*% (cc[1:j] + Sinv * t(as.matrix(FF[,1:j])) %*% as.matrix(y[,j]))
    
    b<-rmvnorm(1, mean = m, sigma = solve(c) )
         
    if (b[j]<0) 
    {
       b<-b*0
    }

    
    B[j,]<-b
  }
  
  else if(j>k){
    
    Sinv<-drop(S[j,j])
    c<-cinv[1:k,1:k] + Sinv * t(FF) %*% FF
    m<-c %*% (cinv %*% m0 + Sinv * t(as.matrix(FF)) %*% as.matrix(y[,j]))
    
    
    b<-rmvnorm(1, mean = m, sigma = solve(c) )
    
    

    
    B[j,]<-t(b)
  }
  }
  
  return(B)
}
  