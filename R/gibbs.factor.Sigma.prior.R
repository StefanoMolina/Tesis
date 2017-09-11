gibbs.factor.Sigma.prior<-function(v, s){
  
  library(invgamma)
  library(OpenMx)
  library(matrixStats)
  
  aux_s<-matrix(0,)
  
  for (i in 1:5){
    aux_s[i]<-rinvgamma(1, shape=v/2, rate= (v*s[i]^2)/2)
    
  }
    sig<-solve(vec2diag(aux_s))
  
  return(sig)
}