gibbs_bigaussian <- function(M=10000, rho=0.98){
  #	Repositorio
  repositorio <- matrix(NA, ncol=2, nrow=M)
  #	Valores iniciales
  x <- 0
  y <- 0
  repositorio[1, ] <- c(x, y)
  #	Iteraciones
  m <- 2
  for(m in 2:M){
    x <- rnorm(1, rho * y, sqrt(1 - rho^2))
    y <- rnorm(1, rho * x, sqrt(1 - rho^2))
    repositorio[m, ] <- c(x, y)
  }
  #	Resultado
  names(repositorio) <- c("x1","x2")
  return(repositorio)
}
