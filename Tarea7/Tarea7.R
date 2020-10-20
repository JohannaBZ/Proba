# Modelos Probabilistas Aplicados
# Tarea 5
# Tema: regresión lineal y transformaciones
# Johanna Bolaños Zuñiga

#%%%%%%%%%%%%%%%% Packages %%%%%%%%%%%%%%%%%%
if (!require("normtest")) {
    install.packages('normtest')
}

if (!require("rcompanion")) {
    install.packages('rcompanion')
}

require(normtest)
require(car)
require(MASS)
library(corrplot)
library(rcompanion)
library(trafo)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#replicas
n=100

#data.frame vacio
f1 <- data.frame(x1=numeric(), y=numeric())
f2 <- data.frame(x1=numeric(), y=numeric())
f3 <- data.frame(x1=numeric(), x2=numeric(), y=numeric())
f4 <- data.frame(x1=numeric(), x2=numeric(), y=numeric())

#%%%%% llenado del data.frame

for(i in 1:n){
x1 <- runif(1)
x2 <- runif(1)
xe <- rexp(1)
xn <- rnorm(1, 10, 2)
xa <- sample(seq(10, 60, 0.5), 1)
ruido <- rnorm(1)
f1 <- rbind(f1,data.frame(x1=xa, y=8*xa+5))
f2 <- rbind(f2,data.frame(x1=xa, y=3*xa^2+ruido))
f3 <- rbind(f3,data.frame(x1=x1, x2=x2, y=20*x1+5*x2+1))
f4 <- rbind(f4,data.frame(x1=x1, x2=xa, y=20*x1^2+5*log(xa)+10))
}

#%%%% Con f1 %%%%%
png("linealsencilla.png", width=2000,height=3000,res=600)
plot(f1$x1, f1$y)
dev.off()

rf1 <- lm(y~x1, data=f2)
summary(rf1)

#%%%% Con f3 %%%%%
png("linealmultiple.png", width=2000,height=3000,res=600)
corrplot(corr = cor(x = f3, method = "pearson"), method = "number")
dev.off()

rf3 <- lm(y~x1+x2, data=f3)
summary(rf3)

#%%%%%%%%%% Con f2 %%%%%%%%%%%%%%%%%%%%%%

png("cuadratica.png", width=2000,height=3000,res=600)
scatter.smooth(f2$x1, f2$y, xlab="Valores de x1", ylab="Valores de y", col="blue")
dev.off()

rf2 <- lm(y~x1, data=f2)
summary(rf2)

#2. como no hay una relacion lineal, uso transformaciones
#log
tlogf1 <- lm(log(y)~x1, data=f2)
tlogf12 <- lm(y~log(x1), data=f2)
tlogf13 <- lm(log(y)~log(x1), data=f2)
summary(tlogf1)
summary(tlogf12)
summary(tlogf13)

#graficas transformadas
png("graficaslog.png", width=6500,height=4000,res=900)
par(mfrow = c(1,3))
scatter.smooth(log(f2$y)~f2$x1, col="red", xlab="Valores de x1", ylab = "Valores de y transformados (log(y))")
scatter.smooth(f2$y~log(f2$x1), col="green", xlab="Valores de x1 transformados ((log(x1))", ylab = "Valores de y")
scatter.smooth(log(f2$y)~log(f2$x1), col="orange", xlab="Valores de x transformados ((log(x1))", ylab = "Valores de y transformados (log(y))")
dev.off()

#%%%%%%%%%%% sqrt
traizf1 <- lm(sqrt(y)~x1, data=f2)
traizf12 <- lm(y~sqrt(x1), data=f2)
traizf13 <- lm(sqrt(y)~sqrt(x1), data=f2)
summary(traizf1)
summary(traizf12)
summary(traizf13)

#graficas transformadas
png("graficasraiz.png", width=6500,height=5000,res=900)
par(mfrow = c(1,3))
plot(sqrt(f2$y)~f2$x1)
plot(y~sqrt(x1), data=f2)
plot(sqrt(f2$y)~sqrt(f2$x1))
dev.off()

png("graficasraiz.png", width=6500,height=4000,res=900)
par(mfrow = c(1,3))
scatter.smooth(sqrt(f2$y)~f2$x1, col="red", xlab="Valores de x1", ylab = "Valores de y transformados (sqrt(y))")
scatter.smooth(f2$y~sqrt(f2$x1), col="green", xlab="Valores de x1 transformados ((sqrt(x1))", ylab = "Valores de y")
scatter.smooth(sqrt(f2$y)~sqrt(f2$x1), col="orange", xlab="Valores de x1 transformados ((sqrt(x1))", ylab = "Valores de y transformados (sqrt(y))")
dev.off()

#%%% box-cox
bm=boxcox(rf2)
#lambdabox <- bm$lambdahat
lambda= bm$x
lik= bm$y
bcm=cbind(lambda,lik)
bcm[order(lik),]
lambdabox=bm$x[which(bm$y==max(bm$y))]

tboxf2=lm((y^lambdabox-1)/lambdabox~x1, data=f2) 
summary(tboxf2) 

#%Grafico transformada box
png("graficabox.png", width=4000,height=5000,res=700)
par(mfrow = c(1,2))
bm=boxcox(rf2)
scatter.smooth(((f2$y^lambdabox)-1)/lambdabox~f2$x1, col="yellow", xlab="Valores de x", ylab = "y transformado (box-cox)")
dev.off()


#%%%%%%%%%%%% Con f4 %%%%%%%%%%%%%

rf4 <- lm(y~x1+x2, data=f4)
summary(rf4)
png("multiplenolineal.png", width=3000,height=3000,res=600)
pairs(f4, lower.panel = NULL, col="blue" )
dev.off()

#%%%% Como no hay una relacion lineal, uso transformaciones
#log
tlogf1 <- lm(log(y)~x1+x2, data=f4)
tlogf12 <- lm(y~log(x1)+x2, data=f4)
tlogf13 <- lm(y~x1+log(x2), data=f4)
tlogf14 <- lm(log(y)~log(x1)+log(x2), data=f4)
summary(tlogf1) # este
summary(tlogf12)
summary(tlogf13)
summary(tlogf14)

#sqrt
traiz <- lm(sqrt(y)~x1+x2, data=f4)
traiz1 <- lm(y~sqrt(x1)+x2, data=f4)
traiz2 <- lm(y~x1+sqrt(x2), data=f4)
traiz3 <- lm(sqrt(y)~sqrt(x1)+sqrt(x2), data=f4)
summary(traiz) # este
summary(traiz1)
summary(traiz2)
summary(traiz3)


#%%%%%%%%%%% Box-cox %%%%%%%%%%

bm=boxcox(rf4)
#lambdabox <- bm$lambdahat
lambda= bm$x
lik= bm$y
bcm=cbind(lambda,lik)
bcm[order(lik),]
lambdabox=bm$x[which(bm$y==max(bm$y))]

tboxf2=lm((y^lambdabox-1)/lambdabox~x1, data=f2) 
summary(tboxf2) 

#%%%%%% Tukey pendiente %%%%%%%%%%%%

#transformTukey(f4$x1, plotit=T, statistic=1, returnLambda=T)


