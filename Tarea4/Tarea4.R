# Modelos Probabilistas Aplicados
# Tarea 1
# Tema: Analisis de texto - Distribuciones
# Johanna Bolaños Zuñiga

# Parametros
meta = 1                  
lambda = 2			
mu = exp(-la)		
cuni = numeric()
cexp = numeric()
repl = 1000	


#Exponencial
for (replica in 1:repl) {
    dexp = numeric()
    while (sum(dexp) < meta) {
        dexp = c(dexp, rexp(1, la))
    }
    cexp = c(cexp, length(dexp))
}

#Uniforme
for (replica in 1:repl) {
    duni = c(1)
    while (prod(duni) > mu) {
        duni = c(duni, runif(1))
    }
    cuni = c(cuni, length(duni))
}


#Histogramas
labelY  <- c("Frecuencia")
labelX  <- c("Valores")
colexp <- c("green")
coluni <- c("blue")
colpoi <- c("red")
labelMexp <- c("Dist Exponencial con n=100")
labelMuni <- c("Dist Uniforme con n=100")
labelMpoi <- c("Dist Poisson con n=100")
#png("1000n-la2.png", width=25000,height=10000,res=1000)
par(mfcol = c(1, 3))
hist(cuni, col=coluni, main=labelMuni, xlab=labelX, ylab=labelY)
hist(cexp, col=colexp, main=labelMexp, xlab=labelX, ylab=labelY)
hist(rpois(repl, la), col=colpoi, main=labelMpoi, xlab=labelX, ylab=labelY)
#dev.off()
