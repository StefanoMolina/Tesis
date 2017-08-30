gibbs.factor.B.prior <-function(y, c0, k){
  
  library(mvtnorm)
  p<-ncol(y)
  c<-c0[1,1]
  
  B<-matrix(0,p,k)
  
  for(i in 1:p){
    for(j in 1:k){
      if(i==j){
        bij<-rnorm(n = 1,mean = 0,sd = c)
        if(bij>0){
          B[i,j]<-bij
        }
        else{
          B[i,j]<-0
        }
      }
      else if(j>i){
        B[i,j]<-0
      }
      else{
        B[i,j]<-rnorm(n = 1,mean = 0,sd = c)
      }
    }
  }
  return(B)
}
