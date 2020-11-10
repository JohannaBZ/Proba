# Modelos Probabilistas Aplicados
# Tarea 10
# Tema: Problemas teoricos - practicos
# Johanna Bolaños Zuñiga


#%%%%%%%%%%%%%%%%%%%%%%%%%% Packages %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

if (!require("ggplot2")) {
    install.packages('ggplot2')
}

if (!require("dplyr")) {
    install.packages('dplyr')
}

require(ggplot2)
require(dplyr)

#%%%%%%%%%%%%%%%%%%%%%%% Probelma de las cartas %%%%%%%%%%%%%%%%%%%%%

#cartas enumeradas del 2 al 10. gana 1 dolar si sale impar, pierde 1 dolar si sale par
#i son las veces que saca las cartas

datos <- numeric()
valorEsperados <- numeric()
 
for(i in 1:1000){
	cartas = sample(2:10, i, replace = TRUE, prob = c(1/9, 1/9, 1/9, 1/9, 1/9, 1/9, 1/9, 1/9, 1/9));
	for(j in 1:length(cartas)){
		if(cartas[j]%%2==0){
			datos <- c(datos,-1); 
		}
		else {
			datos <- c(datos,1);
		}
	}
	tabla <-data.frame(table(datos)/length(cartas));
	#View(tabla);
	mu=sum(as.numeric(paste(tabla$datos))*tabla$Freq);
	#print(mu);
	valorEsperados = c(valorEsperados, mu);
	#View(datos);
	datos <- numeric()
}

#View(table(valorEsperados))

png("cartas1000.png", width=6000,height=8000,res=900)
hist(valorEsperados, col="dodgerblue3",  xaxt="n", breaks=seq(-1,0.7,0.2), main=NULL, xlab="Valores esperados", ylab="Frecuencia", ylim=c(0,1000), labels=T)
axis(side=1, at=seq(-1,0.7,0.2), labels=seq(-1,0.7,0.2))
dev.off()


#%%%%%%%%%%%%%%%%%%%%%%%% Problema dado cargado %%%%%%%%%%%%%%%%%%%%%%%%%%%

valorEsperados = numeric()
varianza = numeric()
desviacion = numeric()

#%%%% VE, Var y Desv %%%%

#View(valorEsperados)
#View(varianza)
#View(desviacion)

png("dadosVE-2.png", width=5000,height=6000,res=900)
hist(valorEsperados, labels=T,  cex.lab=1.2, xaxt="n", breaks=seq(3.2,5,0.2), col="blue", ylim=c(0,40), main=NULL, xlab="Valores esperados", ylab=labelY)
axis(side=1, at=seq(3.2,5,0.2), labels=seq(3.2,5,0.2))
dev.off()

png("dadosVar-2.png", width=5000,height=6000,res=900)
hist(varianza, labels=T,  cex.lab=1.2,  xaxt="n", breaks=seq(0.7,3.2,0.1), col="green4", ylim=c(0,18), main=NULL, xlab="Varianzas", ylab=labelY)
axis(side=1, at=seq(0.7,3.2,0.1), labels=seq(0.7,3.2,0.1))
dev.off()

png("dadosDesv-2.png", width=5000,height=6000,res=900)
hist(desviacion , labels=T,col="red", cex.lab=1.2, xaxt="n", ylim=c(0,35), main=NULL, xlab="Desviaciones estándar", ylab=labelY)
axis(side=1, at=seq(0.8,2,0.1), labels=seq(0.8,2,0.1))
dev.off()

#%%%%% freq de caras del dado %%%%

#View(tbdatos80)

x10 <- tbdatos10$Freq
piepercent10 <- round(100*x10/sum(x10), 1)
labelPie10 <- paste(piepercent10, "%", sep=" ")

x50 <- tbdatos50$Freq
piepercent50 <- round(100*x50/sum(x50), 1)
labelPie50 <- paste(piepercent50, "%", sep=" ")

x80 <- tbdatos80$Freq
piepercent80 <- round(100*x80/sum(x80), 1)
labelPie80 <- paste(piepercent80, "%", sep=" ")

#%%%%% Grafica pie %%%%%%

png("dados10-.png",  width=3500,height=4500, res=800)
pie(tbdatos10$Freq, labels = labelPie10, main=NULL, col = rainbow(length(x10)))
legend("bottom", as.character(tbdatos10$cara), cex = 0.8,
   fill = rainbow(length(x10)), title = "Cara del dado", horiz=T)
dev.off()

png("dados50-.png", width=3500,height=4500, res=800)
pie(tbdatos50$Freq, labels = labelPie50, main=NULL, col = rainbow(length(x50)))
legend("bottom", as.character(tbdatos50$cara), cex = 0.8, 
   fill = rainbow(length(x50)), title = "Cara del dado", horiz=T)
dev.off()

png("dados80-.png", width=3500,height=4500, res=800)
pie(tbdatos80$Freq, labels = labelPie80, main=NULL, col = rainbow(length(x80)))
legend("bottom", as.character(tbdatos80$cara), cex = 0.8,
   fill = rainbow(length(x80)), title = "Cara del dado", horiz=T)
dev.off()


#%%%%%%%%%%%%%%%%%% Problema de las bombillas %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#Valor esperado analitico
la = 0.05
integral <- integrate(function(t) t^3*la^2*exp(-la*t), lower = 0, upper = Inf)
valorEsperado <- integral$value
valorEsperado


#en un lote de 10 bombillas, donde el valor de vida util de cada una de ellas
#sigue una distribución uniforme 10 y 100, exponencial con una lambda de 0.5 y normal con media de 100 y sd de 20

tabla <- data.frame(x=character(), y=numeric())
tablaVar <- data.frame(x=character(), y=numeric())
for(i in 1:4){
	if(i==1){
		t <- rexp(100,0.05)
		varEsperado = integrate(function(t) t^2*la^2*exp(-la*t), lower = 0, upper = Inf);
		datos <- data.frame(i,varEsperado$value)
		names(datos)=c("t","muEsperado")
		mean <- mean(datos$muEsperado)
		tabla <- rbind(tabla,data.frame(x="Exponencial",y=mean))
		varianza = integrate(function(t) t^3*la^2*exp(-la*t), lower = 0, upper = Inf);
		datosVarianza <- data.frame(i,varianza$value)
		names(datosVarianza)=c("t","varianza")
		meanVar <- mean(datosVarianza$varianza)
		tablaVar <- rbind(tablaVar,data.frame(x="Exponencial",y=meanVar))
	}
	if(i==2){
		t <- runif(100, min=10, max=100)
		varEsperado = integrate(function(t) t^2*la^2*exp(-la*t), lower = 0, upper = Inf);
		datos <- data.frame(i,varEsperado$value)
		names(datos)=c("t","muEsperado")
		mean2 <- mean(datos$muEsperado)
		tabla <- rbind(tabla,data.frame(x="Uniforme",y=mean2))
		varianza = integrate(function(t) t^3*la^2*exp(-la*t), lower = 0, upper = Inf);
		datosVarianza <- data.frame(i,varianza$value)
		names(datosVarianza)=c("t","varianza")
		meanVar2 <- mean(datosVarianza$varianza)
		tablaVar <- rbind(tablaVar,data.frame(x="Uniforme",y=meanVar2))
	}
	if(i==3){
		t <- rnorm(100,100,20)
		varEsperado = integrate(function(t) t^2*la^2*exp(-la*t), lower = 0, upper = Inf);
		datos <- data.frame(i,varEsperado$value)
		names(datos)=c("t","muEsperado")
		mean3 <- mean(datos$muEsperado)
		tabla <- rbind(tabla,data.frame(x="Normal",y=mean3))
		varianza = integrate(function(t) t^3*la^2*exp(-la*t), lower = 0, upper = Inf);
		datosVarianza <- data.frame(i,varianza$value)
		names(datosVarianza)=c("t","varianza")
		meanVar3 <- mean(datosVarianza$varianza)
		tablaVar <- rbind(tablaVar,data.frame(x="Normal",y=meanVar3))
	}
	if(i==4){
		t <- rnorm(100,0.2)
		varEsperado = integrate(function(t) t^2*la^2*exp(-la*t), lower = 0, upper = Inf);
		datos <- data.frame(i,varEsperado$value)
		names(datos)=c("t","muEsperado")
		mean4 <- mean(datos$muEsperado)
		tabla <- rbind(tabla,data.frame(x="Poisson",y=mean4))
		varianza = integrate(function(t) t^3*la^2*exp(-la*t), lower = 0, upper = Inf);
		datosVarianza <- data.frame(i,varianza$value)
		names(datosVarianza)=c("t","varianza")
		meanVar4 <- mean(datosVarianza$varianza)
		tablaVar <- rbind(tablaVar,data.frame(x="Poisson",y=meanVar4))
	}
}

#View(tabla)
#View(tablaVar)
colores <- c("green4", "blue", "orange", "red" )
labelX <- c("Tipo de distribución de la vida útil de 100 bombillas ACME")
labelY <- c("Valor esperado de la vida útil de las bombillas ACME")

png("VEBombillas.png", width=5000,height=6000,res=900)
barplot(tabla$y~tabla$x, 
	xlab=labelX, 
	ylab=labelY,
	col=colores, axis.lty = 1)
dev.off()

labelYVar <- c("Varianza de la vida útil de las bombillas ACME")
png("VarBombillas.png", width=5000,height=6000,res=900)
barplot(tablaVar$y~tablaVar$x, 
	xlab=labelX, 
	ylab=labelYVar,
	col=colores,
	ylim=c(0,2500), axis.lty = 1)
dev.off()

#%%%%%%%%%%%% seleccionar entre -1, 0 y 1 %%%%%%%%%%%%%%%%%%

n=10000
datos = numeric()
for(i in 1:n){
	datos = c(datos, sample(-1:1,1))
}

tabla <- data.frame(table(datos)/n)
mu=sum(as.numeric(paste(tabla$datos))*tabla$Freq);
varianza = var(datos)
desviacion = sd(datos)

png("seleccion.png", width=2000,height=2500,res=500)
barplot(table(datos), col=rainbow(5), xlab = "Valores de X", ylab = "Frecuancia")
dev.off()



