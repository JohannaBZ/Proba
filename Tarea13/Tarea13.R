# Modelos Probabilistas Aplicados
# Tarea 13
# Tema: Ley de los números grandes
# Johanna Bolaños Zuñiga


exp <- 3:7
replicas <- 1000
asegurados <- 10**exp[5]
acumulado <- numeric()

for(i in 1:replicas){
	datos<-sample(0:1, size=asegurados, prob=c(249/250, 1/250), replace=TRUE)
	acumulado=c(acumulado, mean(datos))
}

png("1000.png", width=9500,height=10500,res=2000)
plot(density(acumulado), main="", xlab="Promedio de perdida del siniestro z en 1,000 replicas\n con n=1,000 asegurados", ylab="Densidad")
abline(v = mean(acumulado), col="red", lwd=2, lty=1)
dev.off()

