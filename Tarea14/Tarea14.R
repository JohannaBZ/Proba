# Modelos Probabilistas Aplicados
# Tarea 14
# Tema: Teorema del límite central
# Johanna Bolaños Zuñiga


#%%%%%%%%%%%%%%%% Packages %%%%%%%%%%%%%%%%%%
if (!require("readr")) {
    install.packages('readr')
}

require("readr")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

datosBase <- read_csv("sales_data_sample.csv", col_names = TRUE)
ventas <- data.frame(datosBase$SALES)
colnames(ventas) <-c("datos")

muestras = 10
tamaño = 30
acumulado <- numeric()

for(i in 1:muestras){
	datos<-sample(ventas$datos, tamaño)
	acumulado=c(acumulado, mean(datos))
}


palabra1 <-c("Promedio de ventas, media =")
palabra2 <- round(mean(acumulado4),2)
labelX <- paste(palabra1, palabra2, sep=" ")


png("10-30.png", width=6500,height=6000,res=900)
hist(acumulado,  col="mediumorchid", ylab="Frecuencia", xlab = labelX, main = NULL)
abline(v=mean(acumulado), col = "red", lwd=2, lty=1)
dev.off()

