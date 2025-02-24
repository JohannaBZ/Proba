# Modelos Probabilistas Aplicados
# Tarea 1
# Tema: �ndice Nacional de Precios al Consumidor (INPC)
# Johanna Bola�os Zu�iga

principalesCiudades<-read.table("Precios.dat", header = TRUE, encoding="UTF-8")
colores<-c("green","white","red")
titulo<-"�ndice Nacional de Precios al Consumidor (INPC) en M�xico\n Enero 2019 - 2020"
labelx<-"Ciudades principales"
labely<-"�ndice (base segunda quincena julio 2018)"
png("PrincipalesCiudades.png", width=2000,height=1600,res=300)
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