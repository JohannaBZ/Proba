# Modelos Probabilistas Aplicados
# Tarea 5
# Tema: Generación de pseudoaleatorios con disttibución uniforme y normal
# Johanna Bolaños Zuñiga

gaussian = function(mu, sigma) {
    u = runif(2);
    z0 = sqrt(-2 * log(u[1])) * cos(2 * pi * u[2]);
    z1 = sqrt(-2 * log(u[1])) * sin(2 * pi * u[2]);
    datos = c(z0, z1);
    return (datos * sigma + mu);
}

n=4500
m = 20
desv = 15
pseudos = numeric()

while (length(pseudos) < n) {
	pseudos = c(pseudos, gaussian(m, desv))
}

#%%%%%%%%%%%% Histograma %%%%%%%%
#labelY  <- c("Frecuencia")
#labelX  <- c("Valores")
#col <- c("blue")
#png("pseudonormal.png", width=3500,height=3000,res=600)
#hist(pseudos, col=col, main=NULL, xlab=labelX, ylab=labelY)
#dev.off()

#%%%%%%%%%Aleatorios con distribución normal %%%%%%%%%%
original = rnorm(n, m, desv)
#labelY  <- c("Frecuencia")
#labelX  <- c("Valores")
#col <- c("green")
#png("distnormal.png", width=3500,height=3000,res=600)
#hist(original, col=col, main=NULL, xlab=labelX, ylab=labelY)
#dev.off()

#%%%%%%%%%%%%% Histogramas %%%%%%%%%%%%%%%%%%%%
labelY  <- c("Frecuencia")
labelX  <- c("Valores")
col <- c("blue")
col1 <- c("green")
labelM <- c("Generador (Transformación de Box-Muller")
labelMnorm <- c("Distribución normal")
png("comparativonormal.png", width=10000,height=6000,res=700)
par(mfcol = c(1, 2))
hist(pseudos, col=col, main=labelM, xlab=labelX, ylab=labelY)
hist(original, col=col1, main=labelMnorm, xlab=labelX, ylab=labelY)
dev.off()

