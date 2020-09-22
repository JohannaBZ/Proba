# Modelos Probabilistas Aplicados
# Tarea 1
# Tema: Analisis de texto - Distribuciones
# Johanna Bolaños Zuñiga


#%%%%%%%%%%%%%%%% Packages %%%%%%%%%%%%%%%%%%
if (!require("gutenbergr")) {
    install.packages('gutenbergr')
}

if (!require("tidyverse")) {
    install.packages('tidyverse')
}


if (!require("tokenizers")) {
    install.packages("tokenizers")
}

require(gutenbergr)
require(tidytext)
require(dplyr)
require(tidyr)
require(tidyverse)
require(stringr)
require(tokenizers)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Descargar el libro
libro = gutenberg_download(c(25344))
tail(libro)
head(libro)

# Letras
letras = libro %>% unnest_tokens(chars, text, "characters")

# Palabras
palabras = libro %>% unnest_tokens(word, text, "words")
palabrasFil = filter(palabras,  nchar(palabras$word) > 1)

# Parrafos
parrafos = libro %>% unnest_tokens(paragraph, text, "paragraphs")

# Largo de palabras
lonPalabra <- nchar(as.list(palabrasFil$word))
# Hist de largo de palabras
labelX <- c("Largo de palabras")
labelY <- c("Frecuencia")
color<-c("blue")
niveles<-seq(0,max(lonPalabra),1)
png('largoPalabras.png', width=5000, height=4000, res=500)
hist(lonPalabra, 
	breaks=niveles,
	main=NULL,
	xlab=labelX,
	ylab=labelY,
	col=color)
dev.off()

# Palabaras por parrafo
palabrasParrafo <- tokenize_words(parrafos$paragraph)
cantPalabrasParrafo<-data.frame(sapply(palabrasParrafo, length))
names(cantPalabrasParrafo)= c("length")
palabrasParrafoFilt <- filter(cantPalabrasParrafo, length > 9)

# Hist palabaras por parrafo
color<-c("red")
labelX <- c("Palabras por parrafo")
niveles<-seq(0,600,100)
png('palParrafos.png', width=5000, height=4000, res=500)
hist(palabrasParrafoFilt$length, 	
	breaks=niveles,
	main=NULL, 
	xlab=labelX,
	ylab=labelY,
	col=color)
dev.off()

# %%%%%%%%%%%%%%%%%%%%%%%  Algoritmos  %%%%%%%%%%%%%%%

#Geometrica - encontrar la letra e en todo el libro

counter=0
resultados <- numeric()
letraBuscada = "e"

for(i in 1:length(letras$chars)){
	counter=counter+1
	if(letras$chars[i] == letraBuscada){
		resultados  = c(resultados,counter)		
		counter = 0
	}
}


# Hist 
color<-c("green")
labelX=c("Cantidad de fracasos para encontra la letra e")
png('letraE.png', width=5000, height=4000, res=500)
hist(resultados, 
	main=NULL,
	xlab=labelX, 
	ylab=labelY, 
	col=color)
dev.off()


#Binomial --- cuantas veces encuentra una palabra con x longitud de letras en parrafos de igual tamaño 
cantPalabras = 9 #longitud del parrafo
longPalabra = 2 #longitud de la palabra
resultados=numeric()
counter=0


for(i in 1:length(palabrasParrafo)){
	if(cantPalabrasParrafo$length[i]==cantPalabras){
		for(j in 1:cantPalabrasParrafo$length[i]){
			if(nchar(palabrasParrafo[[i]][j])==longPalabra){
				counter=counter+1
			}
		};
		resultados=c(resultados,counter)
		counter=0
	}
}


# Hist
color<-c("pink")
labelX=c("Cantidad de éxitos con palabras de longitud 5")
niveles<-seq(0,max(resultados),1)
png('cantPalabras.png', width=5000, height=4000, res=500)
hist(resultados, 
	breaks=niveles,
	main=NULL,
	xlab=labelX, 
	ylab=labelY, 
	col=color)
dev.off()

# No binomial - intentos para encontrar x cantidad de h seguida de e
resultados=numeric()
counter=0
exitos=5
contExito=0

for(i in 1:length(letras$chars)){
	counter=counter+1
	if(letras$chars[[i]]=="h" && letras$chars[[i+1]]=="e"){
		contExito=contExito+1
		if(contExito==exitos){
			contExito=0
			resultados=c(resultados,counter)
			counter=0
		}
	}
}

# Hist
color<-c("orange")
labelX=c("Cantidad de intentos para encontrar h seguida de e")
png('he.png', width=5000, height=4000, res=500)
hist(resultados, 
	main=NULL,
	xlab=labelX, 
	ylab=labelY, 
	col=color)
dev.off()



