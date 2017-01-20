# Turdus grayi analysis
# Script para determinar variacion geográfica del llamado de Turdus grayi

# Instalación de paquetes y librerías necesarias 
# (otros paquetes pueden incluirse de camino)
library(warbleR)

# Cambiar el directorio de trabajo
setwd("I:/GeCoS/Tg_call_paper")
getwd()

# Hacer un llamado para visualizar los llamados de Turdus grayi desde Xenocanto
Tur.gra <- querxc(qword = "Turdus grayi", download = FALSE) 
View(Tur.gra)

# Nombrar las columnas del dataframe
names(Tur.gra)

# Desplegar el dataframe
View(Tur.gra)

# Descargar y visualizar las imagenes de los mapas del rango geográfico
# de los datos. Por default la imagen es JPG pero en tiff tienen mejor resolución
xcmaps(X = Tur.gra, img = FALSE) 

# Mostrar el número de grabaciones disponibles
nrow(Tur.gra) 

# Encuentra cuantos typos de descripción de la señal existen en los metadatos de Xenocanto
levels(Tur.gra$Vocalization_type)

# Muestra cuantas grabaciones hay por tipo de señal
table(Tur.gra$Vocalization_type)

# Hay muchos niveles en la variable Vocalization_type. 
# Algunos son señales biologicamente reelevantes, pero muchos solo refrlejan la variación en los datos desplegados 

# Filtrar la señal que queremos 
Tur.gra.call <- droplevels(Tur.gra[grep("call", Tur.gra$Vocalization_type, ignore.case = TRUE), ])

# Check resulting data frame
str(Tur.gra.call) 

# Cuantas grabaciones por localidad?
table(Tur.gra.call$Locality)

# Muestra cuantas grabaciones hay por tipo de señal
table(Tur.gra.call$Vocalization_type)


# Descargar los metadatos de Xenocanto creando un csv en el directorio
write.csv(Tur.gra.call, "Tur.gra.call_metadatos.csv", row.names = FALSE)

# Hacer un llamado para descargar los llamados de Turdus grayi desde Xenocanto
Tur.gra.calls <- querxc(qword = "Turdus grayi", download = TRUE) 
Tur.gra.calls

# Hacer un data frame con todos los llamados que posean la palabra "call"
Tgrayi_calls <- Tur.gra.calls[grepl("call", Tur.gra.calls$Vocalization_type), ]
View(Tgrayi_calls) # NO USAR
View(Tur.gra.call)

# Descargar los metadatos de Xenocanto creando un csv en el directorio
write.csv(Tur.gra.call, "Tgrayi_calls_metadatos.csv", row.names = FALSE)


#  Crear y descargaar los mapas para visualizar los datos descargados de Xenocanto  
xcmaps(X = Tur.gra.call, img = FALSE) 

# Revisa el directorio
getwd()
# Convertir mp3 a wav
mp32wav() 

# You can use checkwavs to see if wav files can be read
checkwavs() 


# Revisar los archivos .wav en el directorio
checkwavs()


# Importar sus propias grabaciones desde el directorio correcto con setwd()

files <- list.files(path = getwd(), pattern = ".wav$", ignore.case = TRUE)
View(files)

# Filtrar los "calls" de toda la base de datos de   
require(pbapply)

Tgrayi_calls <- Tgrayi[grepl("call",Tgrayi$Vocalization_type), ]
require(pbapply)
wavdur <- function() { 
  files <- list.files(path = getwd(), pattern = "wav$", ignore.case = TRUE) 
  if(length(files) == 0) stop("no .wav files in working directory")
  a <- pbsapply(files, function(x) {
    rec<-readWave(as.character( x),header = TRUE)
    return(rec$samples/rec$sample.rate) 
  })
  message("all done!")
  return(data.frame(sound.files=names(a),duration=a,row.names = NULL))
}

wavdur()
fd<-wavdur()
head(fd)

str(fd)
Tg<-data.frame(sound.files=fd$sound.files,selec=1,start=0,end=fd$duration, sel.comment=NA)
Tgcall<-Tg[grep(paste(Tgrayi_calls$Recording_ID,collapse = "|"), Tg$sound.files),]
nrow(Tgcall)


# Crear archivos .tiff de grabaciones enteros, flim se modifica en dependencia de la vocalizacion 
tgX <- Tgcall[sample(1:nrow(Tgcall),),]
lspec(X = tgX, flim=c(1,5),sxrow=12, it = "tiff")


# Crear una lista y selección en base a los archivos tiff elegidos anteriormente
tl <- list.files(pattern = ".tiff$", ignore.case = TRUE) 
ind <- sapply(tl,function (x) strsplit(x,split = "-",fixed = TRUE)[[1]][3], USE.NAMES = FALSE) 
Tgrayi_list <- Tgcall[grep(paste(ind, collapse = "|"), Tgcall$sound.files),]
 

# Detectar "calls" automaticamente en base a la amplitud 

calls_autodetec <- autodetec(Tgrayi_list, threshold=8, envt="abs", msmooth=c(512,90),
               power=1, bp= c(0.5, 4), osci = FALSE, wl = 512, xl = 1, picsize = 1, res = 100,
               flim = c(1,5), ls = TRUE, sxrow = 4, rows = 5, mindur = 0.1, maxdur =
                 0.8, redo = TRUE)

# filtrar los datos que no sean NAs, o sea, quitar NAs
Tgrayi_NAN <- calls_autodetec[!is.na(calls_autodetec$start), ]


# Crear los espectrogramas de los datos que fueron autodetectados 
Tgrayi_Spectrograms <- specreator(Tgrayi_NAN, flim = c(0,5), inner.mar = c(4,4.5,2,1), 
          outer.mar = c(4,2,2,1), line = FALSE, osci = TRUE, propwidth = TRUE)


# Analiza los datos de los datos 
Tgrayi_analysis <- specan(Tgrayi_NAN, bp = c(0.5,4), threshold = 8, wl = 512)
View(Tgrayi_analysis)

# estas columnas tienen diferentes numeros de datos
length(Tgrayi_calls$Country)
length(Tgrayi_analysis[ ,1])

# escribir una funcion para anadir una columna de pais a los datos de specan
# que tienen varias selcciones para cada grabacion

# creando un vector de los nombres unicos de las grabaciones en Tgrayi_analysis
uniq.Tgrayi.nms <- unique(Tgrayi_analysis[ ,1])

# para ver como funciona unique()
# uniq.Tgrayi.nms[1]

# crear un indice de prueba para correr frases pequenas dentro del codigo
# asi uno evite errores adelante
# x <- 1

# crear un vector vacio que vamos a poblar usando el loop de lapply()
# con cada iteracion del loop, los valores de pais se van a pegar 
pais <- c()

# todos los variables dentro de una funcion son temporales 
# meter un unlist() para convertir el list que devuelve lapply() en un vector
paises <- unlist(lapply(c(1:length(uniq.Tgrayi.nms)), function(x){
  
  # filtrando todas las filas de Tgrayi_analysis que tengan el mismo
  # nombre del nombre unico especifcado por el variable de iteracion (en este caso, x)
  tmp <- Tgrayi_analysis[Tgrayi_analysis$sound.files==uniq.Tgrayi.nms[x], ]
  
  # calcular el tamano de una columna de este cuadro de datos temporal
  # para saber cuantas veces ocurre este nombre en Tgrayi_analysis (cuantas selecciones tiene)
  len <- length(tmp[ ,1])
  
  
  # Separar el codigo de identificacion unica del nombre de la grabacion
  # para poder hacer un filtro entre dos cuadros de datos mas adelante
  
  # separar en base al "-"
  split.nm <- strsplit(as.character(uniq.Tgrayi.nms[x]), split="-")[[1]][3]
  
  # separar en base al ".", usando los simbolos de escape para separar el valor especial del punto
  split.nm2 <- strsplit(split.nm, split="\\.")[[1]][1]
  
  # poblar el vector pais con el pais repetido tantas veces como aparece la grabacion en Tgray_analysis
  pais <- rep(Tgrayi_calls$Country[grepl(split.nm2, Tgrayi_calls$Recording_ID)], len)
  
  # probar si el grepl funciona
  # Tgrayi_calls[Tgrayi_calls$Recording_ID==split.nm2, ]
  
  # devolver el vector pais para la proxima iteracion
  return(pais)
  
}))

# crear un cuadro de datos nuevo, insertando al nuevo vector justo antes de la columna de selecciones en Tgrayi_analysis
Tgrayi_analysis_FINAL <- data.frame(Tgrayi_analysis[ , c(1:2)], paises, Tgrayi_analysis[ , c(3:18)])

View(Tgrayi_analysis_FINAL) 


# Descargar los metadatos de Xenocanto creando un csv en el directorio
write.csv(Tgrayi_analysis_FINAL, "Tgrayi_analysis_FINAL.csv", row.names = FALSE)


# Analisis en R de Variacion Geográfica
getwd() # asegurarse que estamos trabajando en el directorio inicial que definimos 

  # leer la tabla de datos y convertir en un vector 
  read.csv("Tgrayi_analysis_FINAL.csv") # no hacer si ya tenemos el vector

  # explorar archivos
  str(Tgrayi_analysis_FINAL)
  
  # crear un vectori con el nombre de los archivos
  sound.files <- paste("Turdus-grayi-", Tgrayi$Recording_ID, ".wav", sep = "")
  
  # Crear un dataframe nuevo de los datos de xenocanto con nombres y wav unidos
  Tgrayi2 <- data.frame(sound.files, Tgrayi)
  
  # Hacer 
  Tgrayi3 <- merge(x =Tgrayi , y= Tgrayi2, by) [, grep(sound.files|Country|Latitude|Longitud,
              colnames(Tgrayi2))], by.x = "sound.files", by.y = "sound.files")
  

colnames(Tgrayi_analysis_FINAL)

# Analisis exploratorios

    # para cambiar el nombre de esta columna hay que saber el indice del nombre
    # names(Tgrayi_analysis_FINAL)
    # y despues solo reemplazarlo y chequear que lo hizo bien
    names(Tgrayi_analysis_FINAL)[3] <- "pais"
    names(Tgrayi_analysis_FINAL)  
    View(Tgrayi_analysis_FINAL)
    colnames(Tgrayi_analysis_FINAL)
    
    # Los niveles de Belize y Guatemala desaparecen cuando uno forza esta columna que sea
    # de caracteres, ahorita es un factor 
    table(as.character(Tgrayi_analysis_FINAL$pais))
    
    #  convertir esta columna a caracter de una vez
    Tgrayi_analysis_FINAL$pais <- as.character(Tgrayi_analysis_FINAL$pais)

    # Exploratorio mediante boxplot
    boxplot(Tgrayi_analysis_FINAL$meandomf ~ Tgrayi_analysis_FINAL$pais)  
    
    # Exploratorio de los números de muestra por país
    datosporpais <- table(Tgrayi_analysis_FINAL$pais)  
    datosporpais
    View(datosporpais)
    
dev.off() # Cuando no está actualizando el plot entonces darle este codigo para poder refresh
  
  
# grafico de dispersion 
# plot(as.numeric(as.character(Tgrayi_analysis_FINAL$pais)) 
#    ,Tgrayi_analysis$meandomf, 
#   xlab="pais", ylab="frec dom") 

install.packages("MASS")
library(MASS)
??MASS

#remover variables colineares 
#hacer un data frame solo con las variables numericas 
#calcular colinearidad 
vars<-as.matrix(Tgrayi_analysis_FINAL[,sapply(Tgrayi_analysis_FINAL,is.numeric)]) 
coli<-cor(vars) 
coli<-as.data.frame(coli) 
coli[coli>0.7]<-1 
coli[coli<0.7]<-0 

#ver el numero de variables colineares para cada variable 
vc<-sapply(coli,sum)-1 
vc

library(warbleR)

#remover las q sean colineares con 3 o mas variables 
vars2<-vars[,vc<3] 
coli2<-cor(vars2) 
coli2[coli2>0.7]<-1 
coli2[coli2<0.7]<-0 
#volver a calcular colinearidad 
vc2<-sapply(as.data.frame(coli2),sum)-1 
vc2

#remover algunas q sean colineares con otra variable 
vars3<-vars2[,grep("sd|skew|range",colnames(vars2),invert = T)] 
coli3<-cor(vars3) 
coli3[coli3>0.7]<-1 
coli3[coli3<0.7]<-0 
vc3<-sapply(as.data.frame(coli3),sum)-1 
vc3

#correr el MANOVA no anidado
#Tgrayi_manova <- manova(vars3~Tgrayi_analysis_FINAL$pais+Tgrayi_analysis_FINAL$sound.files) 
#summary(Tgrayi_manova) #ver el resultado del MANOVA 

#correr el MANOVA anidado
Tgrayi_manova_anidado <- manova(vars3~Tgrayi_analysis_FINAL$pais/Tgrayi_analysis_FINAL$sound.files)
summary(Tgrayi_manova_anidado) #ver el resultado del MANOVA 

vars4<-as.data.frame(vars3) #crear un data frame con las variavbles no colineales

#correr un anova para variables duration, kurt, peakf y ffreq  segun el país
Tgrayi_anova <- lm(vars4$duration~Tgrayi_analysis_FINAL$pais/Tgrayi_analysis_FINAL$sound.files)
anova(Tgrayi_anova)

Tgrayi_anova2 <- lm(vars4$kurt~Tgrayi_analysis_FINAL$pais/Tgrayi_analysis_FINAL$sound.files)
anova(Tgrayi_anova2)

Tgrayi_anova3 <- lm(vars4$peakf~Tgrayi_analysis_FINAL$pais/Tgrayi_analysis_FINAL$sound.files)
anova(Tgrayi_anova3)

Tgrayi_anova4 <- lm(vars4$ffreq~Tgrayi_analysis_FINAL$pais/Tgrayi_analysis_FINAL$sound.files)
anova(Tgrayi_anova4)

# Boxplot por país para variable duration según el país
boxplot(Tgrayi_analysis_FINAL$duration ~ Tgrayi_analysis_FINAL$pais, 
        names=c("Colombia", "Costa Rica", "El Salvador", "Honduras", "Mexico", "Nicaragua", "Panamá", "Estados Unidos"))

boxplot(Tgrayi_analysis_FINAL$kurt ~ Tgrayi_analysis_FINAL$pais, 
        names=c("Colombia", "Costa Rica", "El Salvador", "Honduras", "Mexico", "Nicaragua", "Panamá", "Estados Unidos"))

boxplot(Tgrayi_analysis_FINAL$peakf ~ Tgrayi_analysis_FINAL$pais, 
        names=c("Colombia", "Costa Rica", "El Salvador", "Honduras", "Mexico", "Nicaragua", "Panamá", "Estados Unidos"))

boxplot(Tgrayi_analysis_FINAL$ffreq ~ Tgrayi_analysis_FINAL$pais, 
        names=c("Colombia", "Costa Rica", "El Salvador", "Honduras", "Mexico", "Nicaragua", "Panamá", "Estados Unidos"))


dev.off()
par(mfrow=c(2,2)) # dividir la pantalla de los plots en 4 partes, 2 arriba y 2 abajo
