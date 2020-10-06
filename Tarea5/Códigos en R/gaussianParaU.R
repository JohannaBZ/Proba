# Modelos Probabilistas Aplicados
# Tarea 5
# Tema: Generación de pseudoaleatorios con disttibución uniforme y normal
# Johanna Bolaños Zuñiga


#%%%%%%%%%%%%% Dependencia directa de u1 y u2 %%%%%%%%%%%

gaussianmod = function(mu, sigma) {
    u1 = runif(1);
    u2 = 2*u1;
    z0 = sqrt(-2 * log(u1)) * cos(2 * pi * u2);
    z1 = sqrt(-2 * log(u1)) * sin(2 * pi * u2);
    datos = c(z0, z1);
    return (datos * sigma + mu);
}

n=4500
m = 20
desv = 15
pseudos = numeric()

while (length(pseudos) < n) {
	pseudos = c(pseudos, gaussianmod(m, desv))
}

#Histograma
#labelY  <- c("Frecuencia")
#labelX  <- c("Valores")
#col <- c("blue")
#png("dependirecta.png", width=3500,height=3000,res=600)
#hist(pseudos, col=col, main=NULL, xlab=labelX, ylab=labelY)
#dev.off()


#%%%%%%%%%%%%% Dependencia indirecta de u1 y u2 %%%%%%%%%%%

gaussianmod1 = function(mu, sigma) {
    u1 = runif(1);
    u2 = runif(1);
    while(u1 < u2){
	u1 = runif(1);
    }
    z0 = sqrt(-2 * log(u1)) * cos(2 * pi * u2);
    z1 = sqrt(-2 * log(u1)) * sin(2 * pi * u2);
    datos = c(z0, z1);
    return (datos * sigma + mu);
}

n=4500
m = 20
desv = 15
pseudos1 = numeric()

while (length(pseudos1) < n) {
	pseudos1 = c(pseudos1, gaussianmod1(m, desv))
}

#Histograma
#labelY  <- c("Frecuencia")
#labelX  <- c("Valores")
#col <- c("blue")
#png("depenindirecta.png", width=3500,height=3000,res=600)
#hist(pseudos1, col=col, main=NULL, xlab=labelX, ylab=labelY)
#dev.off()


#%%%%%%%%%%%%% funcion rnorm %%%%%%%%%%%
#Aleatorios con distr uniforme
original = rnorm(n, m, desv)
#labelY  <- c("Frecuencia")
#labelX  <- c("Valores")
#col <- c("green")
#png("normal.png", width=3500,height=3000,res=600)
#hist(original, col=col, main=NULL, xlab=labelX, ylab=labelY)
#dev.off()

#%%%%%%%%%%%%%%%%%% Histogramas %%%%%%%%%%

labelY  <- c("Frecuencia")
labelX  <- c("Valores")
col <- c("purple")
col1 <- c("orange")
col2 <- c("green")
labelMdirecta <- c("Dependencia directa entre U1 y U2")
labelMindirecta <- c("Dependencia indirecta entre U1 y U2")
labelMnorm <- c("Distribución normal")
png("comparativo.png", width=10000,height=6000,res=700)
par(mfcol = c(1, 3))
hist(pseudos, col=col, main=labelMdirecta, xlab=labelX, ylab=labelY)
hist(pseudos1, col=col1, main=labelMindirecta, xlab=labelX, ylab=labelY)
hist(original, col=col2, main=labelMnorm, xlab=labelX, ylab=labelY)
dev.off()
