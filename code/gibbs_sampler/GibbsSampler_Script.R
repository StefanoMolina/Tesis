#
#	Gibbs sampler: Ejemplos
#	

rm(list = ls())

setwd("C:/JCMO.Trabajo/@Estudiantes/Stefano Molina/Tesis/code/gibbs_sampler/")
path.work <- 'C:/JCMO.Trabajo/@Estudiantes/Stefano Molina/Tesis/code/gibbs_sampler/'
 
set.seed(1097)

# Funciones
source("gibbs_normalgamma.R")
source("gibbs_bigaussian.R")

#	----------------------------------------------------
#		Exemplo 1: Distribución Normal-Gamma
#	----------------------------------------------------
M <- 10000
thin <- 1000
ejemplo1.out <-gibbs_normalgamma(M,thin)

# Histograma de "x"
hist(ejemplo1.out[,1],M/100)

# Histograma de "x"
hist(ejemplo1.out[,2],M/100)

# Trayectorias de "x"
plot(ejemplo1.out[,1],type='l')

# Trayectorias de "y"
plot(ejemplo1.out[,2],type='l')

# Medias erg?dicas de "x"
plot(as.matrix(cumsum(ejemplo1.out[,1]))/as.matrix(c(1:M)))

# Medias erg?dicas de "y"
plot(as.matrix(cumsum(ejemplo1.out[,2]))/as.matrix(c(1:M)))

#	Gráficas de diagnóstico
par(mfrow=c(3,2))
	#	Dispersión
	plot(ejemplo1.out,col=1:M)
	#	Trayectorias conjuntas
	plot(ejemplo1.out,type="l")
	#	Trayectorias individuales
	plot(ts(ejemplo1.out[,1]))
	plot(ts(ejemplo1.out[,2]))
	#	Histogramas
	hist(ejemplo1.out[,1],40)
	hist(ejemplo1.out[,2],40)

par(mfrow=c(1,1))  

#	Gráficas de diagn?stico (autocorrelaci?n)
par(mfrow=c(3,2))
	#	Trayectorias individuales
	plot(ts(ejemplo1.out[,1]))
	plot(ts(ejemplo1.out[,2]))
	#	Trayectorias individuales
	acf(ts(ejemplo1.out[,1]))
	acf(ts(ejemplo1.out[,2]))
	# Medias erg?dicas de "x"
	plot(as.matrix(cumsum(ejemplo1.out[,1]))/as.matrix(c(1:M)))
	# Medias erg?dicas de "y"
	plot(as.matrix(cumsum(ejemplo1.out[,2]))/as.matrix(c(1:M)))
par(mfrow=c(1,1))  

#	----------------------------------------------------
#		Exemplo 2: Distribucion Normal Bivariada
#	----------------------------------------------------
M <- 10000
rho <- 0
ejemplo2.out <- gibbs_bigaussian(M,rho)

#	Gr?ficas de diagn?stico
par(mfrow=c(3,2))
	#	Dispersi?n
	plot(ejemplo2.out,col=1:M)
	#	Trayectorias conjuntas
	plot(ejemplo2.out,type="l")
	#	Trayectorias individuales
	plot(ts(ejemplo2.out[,1]))
	plot(ts(ejemplo2.out[,2]))
	#	Histogramas
	hist(ejemplo2.out[,1],40)
	hist(ejemplo2.out[,2],40)
par(mfrow=c(1,1))  

#	Gr?ficas de diagn?stico (autocorrelaci?n)
par(mfrow=c(3,2))
	#	Trayectorias individuales
	plot(ts(ejemplo2.out[,1]))
	plot(ts(ejemplo2.out[,2]))
	#	Trayectorias individuales
	acf(ts(ejemplo2.out[,1]))
	acf(ts(ejemplo2.out[,2]))
	#	Histogramas
	hist(ejemplo2.out[,1],40)
	hist(ejemplo2.out[,2],40)
par(mfrow=c(1,1))  

#
#		FIN de "GibbsSampler_Script.R"