##Script para análisis de Sonidos y análisis estadístico
#Elaborado por: Roberto Vargas Masís
#Modificado por: Danny Alfaro Rojas e Ian Portuguez Brenes 
# Version 2.0
#Ultima actualización 22/08/2019


########################################################################################
#######################      ANALISIS DE LA PRESION  "spl1"         ###################################


#Establecer el directorio de trabajo y revisar directorio de trabajo
setwd("C:/Users/artur/OneDrive/Documentos/Trabajos en R/Análisis de Bioacústica")
getwd()

#Instalar paquetes necesarios (agregar aqui los que vayan necesitando)
#install.packages("devtools")
#install.packages("Rcpp")
#install.packages("seewave")
#install.packages("testthat")
#install.packages("vegan")
#install.packages("rlang")
#install.packages("dplyr")
#install.packages("bioacoustics")
#install.packages("gridExtra")
#install.packages("grid")
#install.packages("ggplot2")
#install.packages("lattice")
#install.packages("tuneR")
#install.packages("tidyr", dependencies = T)    # Datos ordenados. lo agregue para poder separae la fecha de la hora
#reinstall.packages("reshape", dependencies = T)

#Llamar los paquetes necesarios (agregar aqui los que vayan necesitando)
library(devtools)
library(Rcpp)
library(seewave)
library(testthat)
library(vegan)
library(rlang)
library(dplyr)
library(bioacoustics)
library(grid)
library(gridExtra)
library(ggplot2)
library(lattice)
library(tuneR)
library(tidyr)
library(reshape)

#Instalación de SINAX Versión 1.3
#install_github("osoramirez/Sinax") # Forma 1
#install_github("osoramirez/resumeRdesc")

# Llamar Sinax y resumeRdesc
library(Sinax)
library(resumeRdesc)

# Crear un vector de la lista de las rutas de las carpetas en el directorio de manera recursiva
l1 <- list.dirs(path = ".", recursive = F, full.names = T)
l1
setwd(l1[1]) # Establecer un directorio mediante un indice referente a la carpeta en la lista

#Crear un vector de una lista recursiva de las carpetas en el directorio
setwd("C:/Users/artur/OneDrive/Documentos/Trabajos en R/Análisis de Bioacústica/audios1")
p1 <- list.files()
p1

#Carpeta por carpeta con nombres de la ruta en el directorio
x = 1
pre = list()
for (i in p1) {
  print(p1[x])
  setwd(p1[x])
  pre[[x]] = spl1()
  pre[[x]]["Sitio"] <- i 
  x = x+1
  setwd("C:/Users/artur/OneDrive/Documentos/Trabajos en R/Análisis de Bioacústica/audios1")
}  

#Vista de las dataframe realizadas
View(pre[[1]])
View(pre[[2]])

#Unir las dataframe en una sola ** Las dataframe deben tener los mismos nombres en columnas
pres <- rbind(pre[[1]],pre[[2]])

#separar la fecha y la hora en 2 columnas diferentes
names(pres)
pres<-separate(pres, col = "Date-Time",into =c("date","time"),sep=" ",convert=T )

#separar la fecha y ola hora en 2 columnas diferentes
names(pres)
pres<-separate(pres, col = "Sitio",into =c("sitio","semana","zona"),sep=" ",convert=T )

#Establecer el directorio de trabajo y revisar directorio de trabajo
setwd("C:/Users/artur/OneDrive/Documentos/Trabajos en R/Análisis de Bioacústica")
getwd()

# Escribir la base de datos en un .csv en el directorio
write.csv(pres, file = "pres.csv")

# Leer dataframe
pres <- read.csv("pres.csv", sep = ",")

#renombrar una variable 
pres = rename(pres, c(X="audio"))

# Escribir la base de datos en un .csv en el directorio
write.csv(pres, file = "pres.csv")

# Leer dataframe
pres<- read.csv("pres.csv", sep = ",")
pres<-pres[,-c(1)]
View(pres)

#Resumenes de datos para previsualizar y pensar en análisis y posibles resultados. Los datos a analizar deben ser numéricos
resume(pres$SPLmean)
resume(pres$SPLsd)
resume(pres$SPLQ25)
resume(pres$SPL50)
resume(pres$SPLQ75)
resume(pres$SPLMax)
resume(pres$SPLMin)
resume(pres$lwr.ci.95..)
resume(pres$upr.ci.95..)

# Convertir la columna Sitio en un Factor para poder usarla como parte del análisis
pres$sitio <- as.factor(pres$sitio) 

#Graficación compartativa con boxplot normales
par(mfrow=c(1:2)) #Crea un arreglo de imagenes del tamaño definido en filas vrs columnas
#dev.off() #Se utiliza para devolver a una sola fila/columna el arreglo de imagenes. No utilizar si no ha guardado las imagenes
boxplot(SPLmean ~ sitio,
        data=pres,
        ylab="Ruido (dB)",
        xlab="Sitios",
        main="Ruido promedio")

boxplot(SPLsd ~ sitio,
        data=pres,
        ylab="Ruido (dB)",
        xlab="Sitios",
        main="Sd Ruido")

boxplot(SPLMax ~ sitio,
        data=pres,
        ylab="Ruido (dB)",
        xlab="Sitios",
        main="Ruido máximo promedio")

boxplot(SPLMin ~ sitio,
        data=pres,
        ylab="Ruido (dB)",
        xlab="Sitios",
        main="Ruido mínimo promedio")

#dev.off() #Se utiliza para eliminar todos los arreglos de imagenes. No utilizar si no ha guardado las imagenes

# Prueba NO PARAMETRICA para buscar diferencias en el SPL entre los sitios (no diferencia entre uno y otro sino entre todos)
# Graficación personalizada en ggplot2
kruskal.test(pres$SPLmean~pres$sitio)
R1 <- ggplot(pres, aes(x = sitio, y = SPLmean)) +
  geom_boxplot( alpha = 0.5)+xlab("Sitios")+ylab("Ruido promedio (dB)")+
  labs(title="Ruido promedio (dB)", subtitle="KW = 88, df = 12, p-value =2e-16")+
  scale_fill_manual(values=c("grey3","grey69", "blue"))
R1

kruskal.test(pres$SPLmean~pres$sitio)
R2 <- ggplot(pres, aes(x = sitio, y = SPLmean)) +
  geom_boxplot(alpha = 0.5)+xlab("Sitios")+ylab("Ruido promedio (dB)")+
  labs(title="Ruido promedio (dB)", subtitle="Kruskal-Wallis chi-squared = 10, df = 2, p-value = 8e-04")+
  scale_fill_manual(values=c("grey3","grey69", "blue"))
R2

grid.arrange(R1, R2, ncol=2, n)



###################################################################################################
    #######################     ANALISIS DE LOS INDICES  "soundindex1"       ##################################


#Establecer el directorio de trabajo y revisar directorio de trabajo
setwd("C:/Users/artur/OneDrive/Documentos/Trabajos en R/Análisis de Bioacústica")
getwd()

#Instalar paquetes necesarios (agregar aqui los que vayan necesitando)
#install.packages("devtools")
#install.packages("Rcpp")
#install.packages("seewave")
#install.packages("testthat")
#install.packages("vegan")
#install.packages("rlang")
#install.packages("dplyr")
#install.packages("bioacoustics")
#install.packages("gridExtra")
#install.packages("grid")
#install.packages("ggplot2")
#install.packages("lattice")
#install.packages("tuneR")
#install.packages("tidyr", dependencies = T)    # Datos ordenados. lo agregue para poder separae la fecha de la hora
#reinstall.packages("reshape", dependencies = T)

#Llamar los paquetes necesarios (agregar aqui los que vayan necesitando)
library(devtools)
library(Rcpp)
library(seewave)
library(testthat)
library(vegan)
library(rlang)
library(dplyr)
library(bioacoustics)
library(grid)
library(gridExtra)
library(ggplot2)
library(lattice)
library(tuneR)
library(tidyr)
library(reshape)

# Llamar Sinax y resumeRdesc
library(Sinax)
library(resumeRdesc)

# Crear un vector de la lista de las rutas de las carpetas en el directorio de manera recursiva
l1 <- list.dirs(path = ".", recursive = F, full.names = T)
l1
setwd(l1[1]) # Establecer un directorio mediante un indice referente a la carpeta en la lista

#Crear un vector de una lista recursiva de las carpetas en el directorio
setwd("C:/Users/artur/OneDrive/Documentos/Trabajos en R/Análisis de Bioacústica/audios1")
p1 <- list.files()
p1

#Carpeta por carpeta con nombres de la ruta en el directorio
x = 1
ind = list()
for (i in p1) {
  print(p1[x])
  setwd(p1[x])
  ind[[x]] = soundindex1()
  ind[[x]]["Sitio"] <- i 
  x = x+1
  setwd("C:/Users/artur/OneDrive/Documentos/Trabajos en R/Análisis de Bioacústica/audios1")
}  

#Vista de las dataframe realizadas
View(ind[[1]])
View(ind[[2]])

#Unir las dataframe en una sola ** Las dataframe deben tener los mismos nombres en columnas
index <- rbind(ind[[1]],ind[[2]])
View(index)

#separar la fecha y la hora en 2 columnas diferentes
names(index)
index<-separate(index, col = "Date-Time",into =c("date","time"),sep=" ",convert=T )

#separar la fecha y ola hora en 2 columnas diferentes
names(index)
index<-separate(index, col = "Sitio",into =c("sitio","semana"),sep=" ",convert=T )

#Establecer el directorio de trabajo y revisar directorio de trabajo
setwd("C:/Users/artur/OneDrive/Documentos/Trabajos en R/Análisis de Bioacústica")
getwd()

# Escribir la base de datos en un .csv en el directorio
write.csv(index, file = "index.csv")

# Leer dataframe
index <- read.csv("index.csv", sep = ",")

#renombrar una variable 
index = rename(index, c(X="audio"))

# Escribir la base de datos en un .csv en el directorio
write.csv(index, file = "index.csv")

# Leer dataframe
index <- read.csv("index.csv", sep = ",")
index<-index[,-c(1)]
View(index)

#Resumenes de datos para previsualizar y pensar en análisis y posibles resultados. Los datos a analizar deben ser numéricos
names(index)
resume(index$AEI.L)
resume(index$BIOAC)
resume(index$BIO.L)
resume(index$ADI.L)
resume()
resume()
resume()
resume()
resume()

# Convertir la columna Sitio en un Factor para poder usarla como parte del análisis
pres$sitio <- as.factor(index$sitio) 

#Graficación compartativa con boxplot normales
par(mfrow=c(1:2)) #Crea un arreglo de imagenes del tamaño definido en filas vrs columnas
#dev.off() #Se utiliza para devolver a una sola fila/columna el arreglo de imagenes. No utilizar si no ha guardado las imagenes
boxplot(AEI.L ~ sitio,
        data=index,
        ylab="Ruido (dB)",
        xlab="Sitios",
        main="Ruido promedio")

boxplot(BIOAC ~ sitio,
        data=index,
        ylab="Ruido (dB)",
        xlab="Sitios",
        main="Sd Ruido")

boxplot(BIO.L ~ sitio,
        data=index,
        ylab="Ruido (dB)",
        xlab="Sitios",
        main="Ruido máximo promedio")

boxplot(ADI.L ~ sitio,
        data=index,
        ylab="Ruido (dB)",
        xlab="Sitios",
        main="Ruido mínimo promedio")

dev.off() #Se utiliza para eliminar todos los arreglos de imagenes. No utilizar si no ha guardado las imagenes

# Prueba NO PARAMETRICA para buscar diferencias en el SPL entre los sitios (no diferencia entre uno y otro sino entre todos)
# Graficación personalizada en ggplot2
kruskal.test(index$AEI.L~index$sitio)
R1 <- ggplot(index, aes(x = sitio, y = AEI.L)) +
  geom_boxplot( alpha = 0.5)+xlab("Sitios")+ylab("Ruido promedio (dB)")+
  labs(title="Ruido promedio (dB)", subtitle="KW = 88, df = 12, p-value =2e-16")+
  scale_fill_manual(values=c("grey3","grey69", "blue"))
R1

kruskal.test(index$BIOAC~index$sitio)
R2 <- ggplot(index, aes(x = sitio, y = BIOAC)) +
  geom_boxplot(alpha = 0.5)+xlab("Sitios")+ylab("Ruido promedio (dB)")+
  labs(title="Ruido promedio (dB)", subtitle="Kruskal-Wallis chi-squared = 10, df = 2, p-value = 8e-04")+
  scale_fill_manual(values=c("grey3","grey69", "blue"))
R2

grid.arrange(R1, R2, ncol=2, n)



