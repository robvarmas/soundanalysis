# Turdus grayi analysis
# Script para determinar variacion geográfica del llamado de Turdus grayi

# Instalación de paquetes y librerías necesarias 
# (otros paquetes pueden incluirse de camino)
install.packages("warbleR")
library(warbleR)

# Cambiar el directorio de trabajo
setwd("C:/Users/Laboratorio/Desktop/Roberto Vargas/Dendrortyx macroura")
getwd()

# Hacer un llamado para visualizar las vocalizacions de Dendrortyx macroura desde Xenocanto
denmac <- querxc(qword = "Dendrortyx macroura", download = FALSE) 

# Desplegar el dataframe
View(denmac)

# Nombrar las columnas del dataframe
names(denmac)

# Descargar y visualizar las imagenes de los mapas del rango geográfico
# de los datos. Por default la imagen es JPG pero en tiff tienen mejor resolución
xcmaps(X = denmac, img = FALSE) 

# Mostrar el número de grabaciones disponibles
nrow(denmac) 

# Encuentra cuantos typos de descripción de la señal existen en los metadatos de Xenocanto
levels(denmac$Vocalization_type)

# Muestra delas grabaciones cuales tipo de señal existen en xenocanto
table(denmac$Vocalization_type)

# Hay muchos niveles en la variable Vocalization_type. 
# Algunos son señales biologicamente reelevantes, pero muchos solo reflejan la variación en los datos desplegados 

# Filtrar la señal que queremos 
denmac_song <- droplevels(denmac[grep("song", denmac$Vocalization_type, ignore.case = TRUE), ])
View(denmac_song)

# Comprueba el resultado del data frame
str(denmac_song) 

# Cuantas grabaciones por localidad?
table(denmac_song$Locality)

# Muestra cuantas grabaciones hay por tipo de señal
table(denmac_song$Vocalization_type)


# Descargar los metadatos de Xenocanto creando un csv en el directorio
write.csv(denmac_song, "denmac_song_metadatos.csv", row.names = FALSE)

# Hacer un llamado para descargar los llamados de Turdus grayi desde Xenocanto
denmac_song <- querxc(qword = "Dendrortyx macroura", download = TRUE) 
denmac_song

# Hacer un data frame con todos los llamados que posean la palabra "call"
dmac_song <- denmac_song[grepl("song", denmac_song$Vocalization_type), ]
View(dmac_song)

# Descargar los metadatos de Xenocanto creando un csv en el directorio
write.csv(denmac_song, "dmac_song_metadatos.csv", row.names = FALSE)

#  Crear un mapa para visualizar los datos descargados de Xenocanto  
xcmaps(X = denmac_song, img = FALSE) 

# Revisa el directorio
getwd()

# RECORDAR Convertir los archivos mp3 descargados a wav 
# Puede utilizarse Audacity o algun otro software


# Revisar los archivos .wav en el directorio
checkwavs()


# Importar sus propias grabaciones desde el directorio correcto con setwd()

files <- list.files(path = getwd(), pattern = ".wav$", ignore.case = TRUE)
View(files)

# Filtrar los "calls" de toda la base de datos de   
require(pbapply)
dmac_song <- denmac[grepl("song",denmac_song$Vocalization_type), ]
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
DM<-data.frame(sound.files=fd$sound.files,selec=1,start=0,end=fd$duration, sel.comment=NA)
DMsong<-DM[grep(paste(dmac_song$Recording_ID,collapse = "|"), DM$sound.files),]
nrow(DMsong)


# Crear archivos .tiff de grabaciones enteros, flim se modifica en dependencia de la vocalizacion 
DMX <- DMsong[sample(1:nrow(DMsong),),]
lspec(X = DMX, flim=c(1,5),sxrow=12, it = "tiff")


# Crear una lista y selección en base a los archivos tiff elegidos anteriormente
DML <- list.files(pattern = ".tiff$", ignore.case = TRUE) 
DMI <- sapply(DML,function (x) strsplit(x,split = "-",fixed = TRUE)[[1]][3], USE.NAMES = FALSE) 
Denmac_list <- DMsong[grep(paste(DMI, collapse = "|"), DMsong$sound.files),]
 

# Detectar "calls" automaticamente en base a la amplitud 

song_autodetec <- autodetec(Denmac_list, threshold=1, envt="abs", msmooth=c(512,90),
                             power=0.3, bp= c(0, 3), osci = FALSE, wl = 512, xl = 1, picsize = 1, res = 300,
                             flim = c(0, 3), ls = TRUE, sxrow = 4, rows = 5, mindur = 0.1, maxdur =
                               0.3, redo = TRUE)


song_autodetec <- autodetec(Denmac_list, threshold=10, envt="abs", msmooth=c(512,60),
                             power=1, bp= c(0.1, 4), osci = FALSE, wl = 512, xl = 1, picsize = 1, res = 100,
                             flim = c(0,3), ls = TRUE, sxrow = 4, rows = 5, mindur = 0.1, maxdur =
                               0.6, redo = TRUE)


# Ver las marcas del autodetect en un dataframe
View(song_autodetec)


# Filtrar los datos que no sean NAs, o sea, quitar NAs
Denmac_NAN <- song_autodetec[!is.na(song_autodetec$start), ]


# Escribir en un csv los datos autodetectados
write.csv(Denmac_NAN, "denmac_song_autodetec_metadatos.csv", row.names = FALSE)