# Modelos Probabilistas Aplicados
# Tarea 11
# Tema: Problemas teoricos - practicos
# Johanna Bolaños Zuñiga

#%%%%%%%%%%%%%%%%%%%%%%%%%% Packages %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

if (!require("magick")) {
    install.packages('magick')
}

library(magick)

#%%%%%%%%%%%%%%% Convolucion %%%%%%%%%%%%%%%%

imagen <- image_read("darkly.jpg")
imagen <- imagen %>% image_convert(colorspace = 'gray')
imagenSobel <- imagen %>% image_convolve('Sobel')
image_browse(imagenSobel)


#%%%%%%%%%%%%%%%%%%% Prueba chi cuadrada %%%%%%%%%%%

k=10
alfas <-c(0.06, 0.07, 0.08, 0.09, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35)
observadas <- c(105, 99, 113, 124, 110, 90, 106, 84, 82, 87)
observadas <- c(8, 5, 6, 13, 9, 10, 11, 12, 11, 15)
esperadas <- rep(100,k)
val <- sum((observadas-esperadas)**2 / esperadas)
p <- 1-pchisq(val, k - 1)

tabla <- as.table(rbind(observadas))
#View(tabla)
resultado <- chisq.test(tabla)
resultado

#%%%%%%%%%%%%%%%%%%% Covarianza y Varianza %%%%%%%%%%%

a=5
b=100
c=20.5
d=-0.85

x <- runif(100)
y <- runif(100)

#%%%%% Covarianza %%%%%%%
x1 <- (a*x)+b
y1 <- (c*y)+d
covLadoIzq <- cov(x1,y1)
covLadoDer <- a*c*cov(x,y)

#%%%% varianza %%%%%%%%%%
u <- rexp(100,0.05)
v <- rnorm(100,100,20)
w <- u+v

varLadoIzq <- var(w)
varLadoDer <- var(u) + var(v) + 2*cov(u,v)

#%%%%%%%%%%%%%%%%% Simulacion %%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%% Covarianza %%%%%%%%%%%%

n=100
rep=100
a=5
b=100
c=20.5
d=-0.85

tablaIzq = numeric()
tablaDer = numeric()

while (length(tablaIzq) < rep) {
x <- rnorm(n,100,20)
y <- rpois(n,0.5)+x
	x1 <- (a*x)+b
	y1 <- (c*y)+d
	covLadoIzq <- cov(x1,y1)
	tablaIzq = c(tablaIzq, covLadoIzq)
	covLadoDer <- a*c*cov(x,y)
	tablaDer <- c(tablaDer, covLadoDer)
}

#%%%%%%%%%% Histogramas %%%%%%%%%%%%%%%%%%%%%%

labelY  <- c("Frecuencia")
labelXIzq  <- c("Valores Cov(aX + b, cY + d)")
labelXDer  <- c("Valores acCov(X,Y)")
png("norm-pois.png", width=9000,height=6000,res=900)
par(mfcol = c(1, 2))
hist(tablaIzq, cex.lab=1.5, col=palette("Paired"), labels=T, main=NULL, xlab=labelXIzq, ylab=labelY, ylim=c(0,30))
hist(tablaDer, cex.lab=1.5, col=palette("Dark 2"), labels=T, main=NULL, xlab=labelXDer, ylab=labelY, ylim=c(0,30))
dev.off()

#%%%%%%%%%%% Varianza %%%%%%%%%

tablaIzqVar = numeric()
tablaDerVar = numeric()

while (length(tablaIzqVar) < rep) {
x <- rexp(n,0.8)
y <- rpois(n,0.5)+x
	w <- x+y
	varLadoIzq <- var(w)
	tablaIzqVar = c(tablaIzqVar, varLadoIzq)
	varLadoDer <- var(x) + var(y) + 2*cov(x,y)
	tablaDerVar <- c(tablaDerVar, varLadoDer)
}

#%%%%%%%%%% Histogramas %%%%%%%%%%%%%%%%%%%%%%

labelY  <- c("Frecuencia")
labelXIzq  <- c("Valores Var(X+Y)")
labelXDer  <- c("Valores Var(X) + Var(Y) + 2Cov(X,Y)")
png("var-unif-exp.png", width=9000,height=6000,res=900)
par(mfcol = c(1, 2))
hist(tablaIzqVar, cex.lab=1.3, col=palette("Dark 2"), labels=T, main=NULL, xlab=labelXIzq, ylab=labelY, ylim=c(0,40))
hist(tablaDerVar, cex.lab=1.3, col=palette("Dark 2"), labels=T, main=NULL, xlab=labelXDer, ylab=labelY, ylim=c(0,40))
dev.off()

