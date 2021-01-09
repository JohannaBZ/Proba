# Modelos Probabilistas Aplicados
# Proyecto final
# Johanna Bolaños Zuñiga

#%%%%%%%%%%%%%%%% Packages %%%%%%%%%%%%%%%%%%
if (!require("dplyr")) {
    install.packages('dplyr')
}

require(dplyr)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%% Cargo datos %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

datos <-read.table("datos.txt", header = T, encoding="UTF-8")
datosp <- filter(datos, tamano == "Pequeñas")
datosm <- filter(datos, tamano == "Medianas1")
datosm2 <- filter(datos, tamano == "Medianas2")
datosg <- filter(datos, tamano == "Grandes")
#View(datos)

#%%%%%%%%%%%%%%%%%%%%%%% calcula brechas %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dif <- (datos$exacto - datos$metaheu)/datos$exacto

#################### Prueba hipótesis de medias %%%%%%%%%%%%%%%%%

#%%%%% Para miu diferente 0
qt(p = 0.95 + 0.05/2, df = length(dif)-1, lower.tail = T)

t.test(dif, alternative='two.sided',
       conf.level=0.95, mu=0)



#%%%%%% Para miu > d
qt(p = 0.90 + 0.1/2, df = length(difg)-1, lower.tail = T)

t.test(dif, alternative='greater',
       conf.level=0.95, mu=(mean(dif)*0.80))

mejoras <- data.frame(porcentaje=c(0, 35, 90, 70, 80),
                    instancias=c("Pequeñas", "Medianas 1", "Medianas 2", "Grandes", "Total"))

png("mejoras.png", width=4000,height=4500, res=600)
plot(mejoras$porcentaje, xaxt = "n", type="b", pch = 16, col= "red", 
	xlab="Categorías instancias", 
	ylab="Porcentaje (%) de mejora de la solución por el metaheurístico",
	ylim=c(0,95))
axis(1, at = 1:5, labels = mejoras$instancias)
text(mejoras$porcentaje,
     labels = paste(mejoras$porcentaje, "%", sep=" "),
     cex = 0.8, pos = 3, col = "black")
dev.off()

#%%%%%%%%%%%%%%%%%%% Proporciones %%%%%%%%%%%%%%%%%%%%%

mejora <- filter(datosm, mejora=="Si")
propesperada <- 0.60
tamano <- length(datosm$tamano)
observadas <- length(mejora$tamano)

#%%%%% estadistico z

z <- (observadas/tamano  - propesperada) / sqrt((propesperada * (1 - propesperada)) / tamano)

#%%%%% Prueba 

prop.test(x=observadas, n=tamano, p=0.6, alternative='greater',
          conf.level=0.95, correct=FALSE)






