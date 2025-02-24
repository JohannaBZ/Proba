# Modelos Probabilistas Aplicados
# Tarea 5
# Tema: Generaci�n de pseudoaleatorios con disttibuci�n uniforme y normal
# Johanna Bola�os Zu�iga


#%%%%%%%%%%%%%%% Packages %%%%%%%%%%%%
if (!require("swfscMisc")) {
    install.packages("swfscMisc")
}

if (!require("ggpubr")) {
    install.packages("ggpubr")
}

require(swfscMisc)
require(ggpubr)

#%%%%%%%%%%%%%%% generador de pseudo uniformes $$$$$$$

uniforme = function(n, semilla) {
    a = 8001
    c = 2651
    m = 2048
    datos = numeric()
    x = semilla
    while (length(datos) < n) {
        x = (a * x + c) %% m
        datos = c(datos, x)
    }
    return(datos/(m-1))
}


#%%%%%%%%%%%%%%% generador de pseudo normales $$$$$$$

gaussian = function(mu, sigma, semilla) {
    	u = uniforme(2, semilla);
      z0 = sqrt(-2 * log(u[1])) * cos(2 * pi * u[2]);
      z1 = sqrt(-2 * log(u[1])) * sin(2 * pi * u[2]);
      datos = c(z0, z1);
      return (datos * sigma + mu);
}


n=4500
m = 20
desv = 15
pseudos = numeric()
semilla = 21

while (length(pseudos) < n) {
	pseudos = c(pseudos, gaussian(m, desv, semilla))
	semilla=semilla+1
}


#%%%%%%%%%%%%%% Aleatorios con distribucion normal %%%%%%%%%%%

original = rnorm(n, m, desv)

#%%%%%%%%%%%%% Histogras %%%%%%%%%%%%%

labelY  <- c("Frecuencia")
labelX  <- c("Valores")
col <- c("blue")
col1 <- c("green")
labelM <- c("Generador (transformaci�n de Box-Muller)")
labelMnormal <- c("Distribuci�n normal")
png("uniformeNormal.png", width=10000,height=6000,res=900)
par(mfcol = c(1, 2))
hist(pseudos, col=col, main=labelM, xlab=labelX, ylab=labelY)
hist(original, col=col1, main=labelMnormal, xlab=labelX, ylab=labelY)
dev.off()

#%%%%%%%%%%%%% greficos q-q %%%%%%%%%%%%%%%%%%%%

labelY  <- c("Valores")
labelX  <- c("Valores")
labelXnorm  <- c("Cuantiles te�ricos de una distribuci�n normal")
col <- c("blue")
secuencia <- seq(-40,80,20)
png("uniformeNormal1.png", width=10000,height=8000,res=900)
par(mfcol = c(1, 2))
hist(pseudos, col=col, main=labelM, xlab=labelX, ylab=labelY)
qqnorm(pseudos, main=NULL, xlab= labelXnorm, ylab=labelY, col=col, ylim=c(-40,80))
qqline(pseudos)
dev.off()



