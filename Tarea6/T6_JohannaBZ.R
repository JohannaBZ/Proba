# Modelos Probabilistas Aplicados
# Tarea 6
# Tema: Pruebas estadísticas
# Johanna Bolaños Zuñiga

# %%%%%%%%%%%%%%%%%%%% base de datos %%%%%%%%%%%%%%%%%%

totalCiudades <-read.table("INPC.txt", header = TRUE, encoding="UTF-8")
#View(totalCiudades)
#%%%%% Conjunto 1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

agosto2018 <- as.numeric(totalCiudades[44:44,])
julio2019<- as.numeric(totalCiudades[55:55,])
agosto2020<- as.numeric(totalCiudades[68:68,])

#%%%%%% Pruebas Conjunto 1 %%%%%%%%%%%%%%%%%%%%%%%%%%%

shapiro.test(agosto2018)
shapiro.test(julio2019)
shapiro.test(agosto2020)
t.test(agosto2018, mu=100.43)
t.test(agosto2018, y = agosto2020,  mu=0)
var.test(agosto2018, julio2019)
cor.test(julio2019, agosto2020)

#%%%%% Conjunto 2 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

monterrey <- as.numeric(totalCiudades$Monterrey)
cdMexico <- as.numeric(totalCiudades$Cd.México)
guadalajara <- as.numeric(totalCiudades$Guadalajara)

#%%%%%% Pruebas Conjunto 2 %%%%%%%%%%%%%%%%%%%%%%%%

shapiro.test(monterrey)
shapiro.test(cdMexico)
shapiro.test(guadalajara)
wilcox.test(monterrey, mu=98.5, conf.int = TRUE)
wilcox.test(monterrey, guadalajara, conf.int = TRUE)
ks.test(cdMexico, guadalajara)

# %%%%%%% Graficos q-q %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
labelX  <- c("Cuantiles teóricos de una distrinución normal")
labelYagos  <- c("Índices (base segunda quincena agosto 2018) en agosto 2018")
labelYjul  <- c("Índices (base segunda quincena julio 2019) en julio 2019")
labelYagos2  <- c("Índices (base segunda quincena agosto 2020) en agosto 2020")
png("normalesC2.png", width=7000,height=5000,res=1000)
par(mfcol = c(1, 3))
qqnorm(agosto2018, main = NULL, freq = F, xlab = labelX, ylab = labelYagos, col = coloragosto)
qqline(agosto2018)
qqnorm(julio2019, main = NULL, xlab = labelX, ylab = labelYjul, col = colorjulio)
qqline(julio2019)
qqnorm(agosto2020, main=NULL, xlab= labelX, ylab=labelYagos2, col=coloragos2)
qqline(agosto2020)
dev.off()

# %%%%%%% Diagrama de cajas y bigotes %%%%%%%%%%%%%%%%%%%

# %%%%%%%% meses %%%%%%%%%%%

graficos = list(agosto2018, agosto2020)
names(graficos ) = c("Agosto 2018", "Agosto 2020")
colores <- c("green", "red")
labelX  <- c("Meses analizados")
labelY <- c("Índices (base segunda quincena agosto 2020)") 
png("comparativosT.png", width=5000,height=4000,res=600)
boxplot(graficos, ylab = labelY , xlab= labelX, cex.lab=1, cex.axis=1, col=colores)
dev.off()

# %%%%%%% ciudades %%%%%%%%%%%%

graficos = list(monterrey, guadalajara)
names(graficos ) = c("Monterrey", "Guadalajara")
colores <- c("yellow", "blue")
labelX  <- c("Ciudades analizadas con datos desde enero 2015 a agosto 2020")
labelY <- c("Índices (base segunda quincena agosto 2020)") 
png("comparativosW.png", width=5000,height=4000,res=600)
boxplot(graficos, ylab = labelY , xlab= labelX, cex.lab=1, cex.axis=1, col=colores)
dev.off()

#%%%%%%%%%%%%%%%% chi-cuadrado %%%%%%%%%%%%

tablacontingencia <- data.frame(totalCiudades[13:24,33:33], totalCiudades[13:24,21:21])
names(tablacontingencia) <- c("Monterrey", "Guadalajara")
rownames(tablacontingencia) <- c("Ene2016", "Feb2016", "Mar2016", "Abr2016", "May2016", "Jun2016", "Jul2016", "Ago2016", "Sep2016", "Oct2016", "Nov2016", "Dic2016")
#View(tablacontingencia)

chisq.test(tablacontingencia, correct=TRUE)
qchisq (0.95, 11)


#%%%%%%%%%%%%% plots %%%%%%%%%%%%%%%%%%%%%%%%

png("correlacion.png", width=3000,height=2500,res=600)
plot(julio2019,agosto2020, xlab = "Julio 2019", ylab = "Agosto 2020", col = "red")
dev.off()
