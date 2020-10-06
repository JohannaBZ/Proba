#%%%%%%%%%%%%% con z0 %%%%%%%%%%%%%%%%%%

gaussian1 = function(mu, sigma) {
    u = runif(2);
    z0 = sqrt(-2 * log(u[1])) * cos(2 * pi * u[2]);
    return (z0 * sigma + mu);
}

n=4500
m = 20
desv = 15
pseudos1 = numeric()

while (length(pseudos1) < n) {
	pseudos1 = c(pseudos1, gaussian1(m, desv))
}


#%%%%%%%%%%%%% con z1 %%%%%%%%%%%%%%%%%%
gaussian2 = function(mu, sigma) {
    u = runif(2);
    z1 = sqrt(-2 * log(u[1])) * sin(2 * pi * u[2]);;
    return (z1 * sigma  + mu);
}

n=4500
m = 20
desv = 15
pseudos2 = numeric()

while (length(pseudos2) < n) {
	pseudos2 = c(pseudos2, gaussian2(m, desv))
}

#%%%%%%% cajas y bigotes %%%%%%%%%%%%%%%%

graficos = list(pseudos1, pseudos2, pseudos, original)
names(graficos ) = c("Con z0", "Con z1", "Con z0 y z1", "normal")
colores <- c("red", "yellow", "blue", "green")
labelX  <- c("Generador (transformacion de Box-Muller) vs Distribución normal")
png("comparativos.png", width=5000,height=4000,res=600)
boxplot(graficos , ylab = labelY , xlab= labelX, cex.lab=1, cex.axis=1, col=colores)
dev.off()

