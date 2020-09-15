# Modelos Probabilistas Aplicados
# Tarea 1
# Tema: Analisis de texto
# Johanna Bolaños Zuñiga


#%%%%%%%%%%%%%%%% Packages %%%%%%%%%%%%%%%%%%
if (!require("gutenbergr")) {
    install.packages('gutenbergr')
}

if (!require("tidyverse")) {
    install.packages('tidyverse')
}

if (!require("SnowballC")) {
    install.packages("SnowballC")
}

if (!require("wordcloud")) {
    install.packages("wordcloud")
}

if (!require("RColorBrewer")) {
    install.packages("RColorBrewer")
}


require(gutenbergr)
require(tidytext)
require(dplyr)
require(tidyr)
require(stopwords)
require(tm)
require(tidyverse)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#anulacion notacion cientifica
options(scipen=999)

# Descargar el libro
libro = gutenberg_download(c(25344))
tail(libro)
head(libro)

#Renombro las columnas
names(libro)= c("ID", "text")

#Quitando renglones vacios
libro <-
  libro %>% 
  filter(!text %in% c(" ", "")) %>% 
  mutate_all(trimws)


# Para ver por letras
letras = libro %>% unnest_tokens(chars, text, "characters")

#Grafico por letras
colores<-c("green","white","red")
labelX<-("Caracteres alfanuméricos")
labelY<-("Frecuencia de uso")
png("letrasEnElTexto.png", width=9000, height=4000, res=600)
barplot(table(letras$chars), 
  xlab=labelX, 
  ylab=labelY,
  col=colores
)
dev.off()

#Para mostrar solo las letras sin los numeros
frameLetras = as.data.frame(table(letras$chars))
letrasFiltradas = frameLetras [11:36,]


#Grafico por letras (sin incluir numeros)
names(letrasFiltradas)= c("Letra", "Frecuencia")
labelX<-("Letras usadas")
png("letrasFiltradas.png",  width=6000, height=4000, res=500)
barplot(letrasFiltradas$Frecuencia, 
  names.arg = letrasFiltradas$Letra, 
  xlab=labelX, 
  ylab=labelY,
  col=colores
)
dev.off()

#Grafico por letras (sin incluir numeros) de manera decreciente
letrasFiltradasDecr = letrasFiltradas[order(letrasFiltradas$Frecuencia, decreasing=TRUE),]
png("letrasFiltradasDrec.png",  width=6000, height=4000, res=500)
barplot(letrasFiltradasDecr$Frecuencia, 
  names.arg = letrasFiltradasDecr$Letra, 
  xlab=labelX, 
  ylab=labelY,
  col=colores
)
dev.off()

# Para ver por palabras
palabras = libro %>% unnest_tokens(word, text, "words")
labelX<-("Palabras")
labelY<-("Frecuencia de uso (escala logaríttmica)")
png("palabrasEnElTexto.png", width=8000, height=5000, res=600)
barplot(table(palabras$word),
  xlab=labelX,
  ylab=labelY,
  log="y"
)
dev.off()

#Depuracion de palabras (quitando articulos entre otros)
stopwords<-get_stopwords()

palabrasDepurada <- palabras %>%
  anti_join(stopwords)

#Recuento de palabras depuradas
palabrasDepurada %>%
  count(word, sort = T)

#Visualizar datos
#view(palabras)
#view(palabrasDepurada)

#Para mostrar palabras mas usadas
framePalabras = as.data.frame(table(palabrasDepurada$word))
names(framePalabras)= c("Palabra", "Frecuencia")
labelY<-("Frecuencia de las palabras más usadas")
limitX=c(0,400)
palabrasFiltradas = framePalabras[framePalabras$Frecuencia > 120,]
png("palabrasFiltradas.png",  width=6000, height=5000, res=600)
barplot(palabrasFiltradas$Frecuencia, 
  names.arg = palabrasFiltradas$Palabra, 
  xlab=labelY, xlim=limitX,
  las=1,
  horiz=TRUE,
  col=colores
)
dev.off()


#Grafico por palabras de manera decreciente
palabrasFiltradasDecr = palabrasFiltradas[order(palabrasFiltradas$Frecuencia, decreasing=FALSE),]
png("palabrasFiltradasDecr.png",  width=6000, height=5000, res=600)
barplot(palabrasFiltradasDecr$Frecuencia, 
  names.arg = palabrasFiltradasDecr$Palabra, 
  xlab=labelY, xlim=limitX,
  las=1,
  horiz=TRUE,
  col=colores
)
dev.off()

#word cloud
png("wordcloud.png", width=1800, height=2100, res=500)
wordcloud(
  words = palabrasFiltradas$Palabra, 
  freq = palabrasFiltradas$Frecuencia, 
  colors = brewer.pal(8, 'Dark2')
)
dev.off()
