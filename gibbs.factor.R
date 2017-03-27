gibbs.factor <-function(y, k, v, s, c0, mu0, M ){
  %y   -> matriz de datos
  %k   -> numero de factores
  %v   -> hiperparametro para Ginv en gibbs.factor.sigma
  %s   -> hiperparametro para Ginv en gibbs.factor.sigma
  %c0  -> hiperparametro para normal en gibbs.factor.B
  %mu0 -> hiperparametro para normal en gibbs.factor.B
  %M   -> numero de iteraciones de Gibbs
  
  
  %El algoritmo inicia obteniendo los valores iniciales para F, B y Sigma
  
  AF<-factanal(y, 1, data = NULL, covmat = diag(1,ncol(y)), n.obs = ncol(y))
  
  AF$loadings<-B_aux
  AF$correlation<-Sigma_aux
  AF$factors<-F_aux
  
  %Se crean los arreglos de tres dimensiones para almacenar las matrices generadas por el algoritmo de Gibbs
  
  f_rep<-array(data=NA,dim=c(M,nrow(F_aux),ncol(F_aux)))
  sigma_rep<-array(data=NA,dim=c(M,nrow(Sigma_aux),ncol(Sigma_aux)))
  B_rep<-array(data=NA,dim=c(M,nrow(B_aux),ncol(B_aux)))
  
  
  %Muestro de Gibbs
  for(j in 1:M){
    F_aux<-gibbs.factor.F(S, B ,y)
    
    B_aux<-gibbs.factor.B()
    
    Sigma_aux<-gibbs.factor.sigma(F, B ,y, v, s, k)
    
    f_rep[j, ,]<-F_aux
    Sigma_rep[j, , ]<-Sigma_aux
    B_rep[j, , ]<-B_aux
    
  }
   
  %Despliegue de resultados
  
  result <- list(F=f_rep, Sigma=Sigma_rep, B=B_rep )    
  return(result)
  
}