%Simulacion de distribucio normal matricial

%La simulacion comienza utilizando el paquete mvtnorm que simula distribuciones normales
%multivariadas, estableciendo las matrices de varianza por derecha e izquierda, la matriz
%de medias y definiendo las dimensiones de la matriz final

library(mvtnorm)
library(KFAS)

%Dadas las matrices de varianzas y covarianzas por izquierda y derecha, Sp y Sn, se puede 
%calcular la distribucion normal matricial siempre y cuando Sp y Sn sean SPD

Sp;

Sn;

M;

Spl=chol(Sp);
Snl=chol(Sn);

sigma <- matrix(c(1), ncol=1)
X<-matrix(rmvnorm(n*p,0,sigma), ncol=p)

%Se transforma la matriz X en la matriz Y 

Y<- M+ Snl %*% X %*% Spl
Y

Para verificar que las matrices son ortogonales sin desplazamiento

Y<-Y-M
t(Y[1,]) %*% Y[2,]

