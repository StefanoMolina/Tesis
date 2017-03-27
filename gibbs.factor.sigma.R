gibbs.factor.sigma <-function(F, B ,y, v, s, k){
  
  library(invgamma)
  
  n<-nrow(y)
  
  sigm<-matrix(0,n,1)
  sig<-diag(1,n)
  
  for(i in 0:n)
  {
    d=t(y[,i]-F*B)(y[,i]-F*B)
    sigm[i]<-rinvgamma(1, shape=(v+n)/2, rate=(v*s^2+d)/2)
    
  }
  
  sig<-sig*sigm
  
  
}