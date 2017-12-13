gibbs.factor.sigma <-function(FF, B ,y, v, s, k){
  
  library(invgamma)
  library(OpenMx)
  
  n<-ncol(y)
  t<-nrow(y)
  
  sigm<-matrix(0,n,1)
  
  
  for(i in 1:n)
  {
    
    d=t(y[,i]-FF %*% B[i,]) %*% as.matrix(y[,i]-FF %*% B[i,])
    
    sigm[i]<-rinvgamma(1, shape=(v+t)/2, rate =(v*s[i]^2+d)/2)
    
  }
  
  sig<-vec2diag(sigm)
  Sinv<-solve(sig)
  
  return(Sinv)
}