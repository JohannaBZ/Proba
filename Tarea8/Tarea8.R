# Modelos Probabilistas Aplicados
# Tarea 5
# Tema: Teorema de Bayes
# Johanna Bolaños Zuñiga

#%%%%%%%%%%%%%%%% Packages %%%%%%%%%%%%%%%%%%
if (!require("dplyr")) {
    install.packages('dplyr')
}

if (!require("ggplot2")) {
    install.packages('ggplot2')
}

if (!require("reshape2")) {
    install.packages('reshape2')
}
require(dplyr)
require(ggridges)
require(ggplot2)
require(reshape2)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

s <- as.numeric(c("0.2", "0.8", "0.99"));
e <- as.numeric(c("0.99", "0.999"));
prev <- as.numeric(c("0.00691", "0.02", "0.8"));

datos <- data.frame(prevalencia=numeric(), especificidad=numeric(), sensibilidad=numeric(), positivas=numeric(), negativas=numeric())

#%%%%% llenado del data.frame

for(i in 1:length(prev)){
	for(j in 1:length(e)){
		for(k in 1:length(s)){
			datos <- rbind(datos,data.frame(prevalencia=prev[i], especificidad=e[j], sensibilidad=s[k], positivas=round((s[k]*prev[i])/((s[k]*prev[i])+((1-e[j])*(1-prev[i]))),4), negativas=round(1-(e[j]*(1-prev[i]))/(((1-s[k])*prev[i])+(e[j]*(1-prev[i]))),4)))
		}
	}
}

#View(datos)

#%%%%%%%%%%%%%%%%% Guardo la tabla %%%%%%%%%%%%%%%%%%%

write.csv(datos , file = "datos.csv")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tabla <- data.frame((datos$prevalencia)*100, (datos$sensibilidad)*100, (datos$positivas)*100)
colnames(tabla) <- c("Prevalencia","Sensibilidad", "Positivas")
#View(tabla)

tabladim <-dcast(tabla,Sensibilidad~Prevalencia,mean,value.var="Positivas")
#View(tabladim)
colnames(tabladim) <- c("s", "p1","p2", "p3")
promedio <- data.frame(tabladim$p1, tabladim$p2, tabladim$p3)
rownames(promedio) <- c("20","80", "99")
#View(promedio)

maxtabladim <- as.matrix(promedio)

png("vpp.png", width=6000,height=4000,res=700)
barplot(t(maxtabladim) ,  main = NULL,
     ylab = "Sensibilidad (%)", 
     xlab = "Valor predictivo positivo promedio (%)", 
     col = c("darksalmon", "lightgreen", "lightslateblue"), 
     axis.lty = 1, 
     horiz=T, las=1)
legend(x = "bottomright", 
       legend = c("0.698","2", "80"), 
       fill = c("darksalmon", "lightgreen", "lightslateblue"), 
       title = "Prevalencia (%)")
dev.off()



