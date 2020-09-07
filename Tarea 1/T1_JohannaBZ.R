# Modelos Probabilistas Aplicados
# Tarea 1
# Tema: Índice Nacional de Precios al Consumidor (INPC)
# Johanna Bolaños Zuñiga

principalesCiudades<-read.table("Precios.dat", header = TRUE, encoding="UTF-8")
colores<-c("green","white","red")
titulo<-"Índice Nacional de Precios al Consumidor (INPC) en México\n Enero 2019 - 2020"
labelx<-"Ciudades principales"
labely<-"Índice (base segunda quincena julio 2018)"
png("PrincipalesCiudades.png")
boxplot(principalesCiudades,
main=titulo,
xlab=labelx,
ylab=labely,
col=colores)
dev.off()


totalCiudades<-read.table("INPC.dat", header = TRUE, encoding="UTF-8")

png("TotalCiudades.png", width=6000, height=4000, res=500)
boxplot(totalCiudades,
main=titulo,
cex.axis=0.5,
xlab="", las=2, 
ylab=labely,
col=colores)
dev.off()