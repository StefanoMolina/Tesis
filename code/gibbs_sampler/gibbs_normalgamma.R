gibbs_normalgamma <- function(M=50000,thin=1000){
  #	Repositorio
	repositorio <- matrix(NA, ncol=2, nrow=M)
  #	Valores iniciales de la cadena
	x <- 1
  y <- 1
  #	Iteraciones
	m <- 1
	for(m in 1:M) {
		j <- 1
        for (j in 1:thin) {
            x <- rgamma(1,3,y*y+4)
            y <- rnorm(1,1/(x+1),1/sqrt(2*x+2))
        }
        repositorio[m,] <- c(x,y)
    }
	#	Resultado
  names(repositorio) <- c("x","y")
  return(repositorio)
 }