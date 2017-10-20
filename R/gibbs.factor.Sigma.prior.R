gibbs.factor.Sigma.prior<-function(v, s){
  
  library(invgamma)
  library(OpenMx)
  library(matrixStats)
  
  
  sig<-solve(vec2diag(rinvgamma(5, shape= v/2, rate= .1)))
  
  return(sig)
}