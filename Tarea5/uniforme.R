# Modelos Probabilistas Aplicados
# Tarea 5
# Tema: Generación de pseudoaleatorios con disttibución uniforme y normal
# Johanna Bolaños Zuñiga


# %%%%%%%% Packages %%%%%%

if (!require("swfscMisc")) {
    install.packages("swfscMisc")
}

require(swfscMisc)

#%%%%%%%%%%%%%%% cumpliendo las condiciones para a, c y m %%%%%%%

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

datos = uniforme(2000, 70)
#labelY  <- c("Frecuencia")
#labelX  <- c("Valores")
#col <- c("blue")
#png("ejemplo2.png", width=3000,height=2500,res=600)
#hist(datos, col=col, main=NULL, xlab=labelX, ylab=labelY)
#dev.off()

#%%%%%%%%%%%%% omitiendo la condicion de a %%%%%%%%%%%

uniforme1 = function(n, semilla) {
    a = 8000
    c = 2651
    m = 2048
    datos1 = numeric()
    x = semilla
    while (length(datos1) < n) {
        x = (a * x + c) %% m
        datos1 = c(datos1, x)
    }
    return(datos1/(m-1))
}

datos1 = uniforme1(2000, 87)
#labelY  <- c("Frecuencia")
#labelX  <- c("Valores")
#col <- c("blue")
#png("ejemplo2.png", width=3000,height=2500,res=600)
#hist(datos1, col=col, main=NULL, xlab=labelX, ylab=labelY)
#dev.off()


#%%%%%%%%%%%%% funcion runif %%%%%%%%%%%

original = runif(n)
#labelY  <- c("Frecuencia")
#labelX  <- c("Valores")
#col <- c("green")
#png("normal.png", width=3500,height=3000,res=600)
#hist(original, col=col, main=NULL, xlab=labelX, ylab=labelY)
#dev.off()




#%%%%%%%%%%%%%%%%%% Histogramas %%%%%%%%%%
labelY  <- c("Frecuencia")
labelX  <- c("Valores")
col <- c("blue")
col1 <- c("red")
col2 <- c("green")
labelMomitiendo <- c("Generador omitiendo la condicion de a")
labelMcumpliendo <- c("Generador cumpliendo las condiciones de los parámetros")
labelMunif <- c("Distribución uniforme")
png("comparativounif.png", width=10000,height=6000,res=700)
par(mfcol = c(1, 3))
hist(datos1, col=col, main=labelMomitiendo, xlab=labelX, ylab=labelY)
hist(datos, col=col1, main=labelMcumpliendo, xlab=labelX, ylab=labelY)
hist(original, col=col2, main=labelMunif, xlab=labelX, ylab=labelY)
dev.off()









