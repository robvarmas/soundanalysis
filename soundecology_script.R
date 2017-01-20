# Cambiar el directorio de trabajo
setwd("C:/Users/Laboratorio/Desktop/Roberto Vargas/Grabaciones/Alameda de Leon")
getwd()

library(soundecology)
library(seewave)
library(tuneR)

####  Cargar archivos individuales
soundfileA <- readWave("1.wav")
soundfileB <- readWave("2.wav")
soundfileC <- readWave("3.wav")
soundfileD <- readWave("4.wav")

### Acoustic Complexity index (ACI)
## The ACI is based on the "observation that many biotic sounds, such as bird songs, are characterized by 
## an intrinsic variability of intensities, while some types of human generated noise (such as car passing 
## or airplane transit) present very constant intensity values" (Pieretti, et al. 2011).

ACI_A <- acoustic_complexity(soundfileA)
print(ACI_A$AciTotAll_left)

ACI_B <- acoustic_complexity(soundfileB)
print(ACI_B$AciTotAll_left)

ACI_C <- acoustic_complexity(soundfileC)
print(ACI_C$AciTotAll_left)

ACI_D <- acoustic_complexity(soundfileD)
print(ACI_D$AciTotAll_left)



### Normalized Difference Soundscape Index (NDSI)
## The Normalized Difference Soundscape Index (NDSI), from REAL (http://www.real.msu.edu) and Kasten, et al. 2012, 
## seeks to "estimate the level of anthropogenic disturbance on the soundscape by computing the ratio of human-generated 
##(anthrophony) to biological (biophony) acoustic components found in field collected sound samples".

NDSI_A <- ndsi(soundfileA)
print(NDSI_A$ndsi_left)

NDSI_B <- ndsi(soundfileB)
print(NDSI_B$ndsi_left)

NDSI_C <- ndsi(soundfileC)
print(NDSI_C$ndsi_left)

NDSI_D <- ndsi(soundfileB)
print(NDSI_D$ndsi_left)


### Bioacoustic Index
## The Bioacoustic Index, from Boelman et al. 2007, is calculated as the "area under each curve included all frequency bands 
## associated with the dB value that was greater than the minimum dB value for each curve. The area values are thus a function 
## of both the sound level and the number of frequency bands used by the avifauna".

BIND_A <- bioacoustic_index(soundfileA)
print(BIND_A$left_area)

BIND_B <- bioacoustic_index(soundfileB)
print(BIND_B$left_area)

BIND_C <- bioacoustic_index(soundfileC)
print(BIND_C$left_area)

BIND_D <- bioacoustic_index(soundfileD)
print(BIND_D$left_area)


### Acoustic Diversity Index (ADI)
## The Acoustic Diversity Index (ADI), from Villanueva-Rivera et al. 2011, is calculated by dividing the spectrogram into bins 
## (default 10, each one of 1000 Hz) and taking the proportion of the signals in each bin above a threshold (default -50 dBFS).
## The ADI is the result of the Shannon index applied to these bins.

ADI_A <- acoustic_diversity(soundfileA)
print(ADI_A$adi_left)

ADI_B <- acoustic_diversity(soundfileB)
print(ADI_B$adi_left)

ADI_C <- acoustic_diversity(soundfileC)
print(ADI_C$adi_left)

ADI_D <- acoustic_diversity(soundfileD)
print(ADI_D$adi_left)


### Acoustic Evenness Index (AEI)
## The Acoustic Evenness Index (AEI), from Villanueva-Rivera et al. 2011 (band evenness using the Gini index), is calculated 
## by dividing the spectrogram into bins (default 10, each one of 1000 Hz) and taking the proportion of the signals in each 
## bin above a threshold (default -50 dBFS). The AEI is the result of the Gini index applied to these bins.

AEI_A <- acoustic_evenness(soundfileA)
print(AEI_A$adi_left)

AEI_B <- acoustic_evenness(soundfileB)
print(AEI_B$adi_left)

AEI_C <- acoustic_evenness(soundfileC)
print(AEI_C$adi_left)

AEI_D <- acoustic_evenness(soundfileD)
print(AEI_D$adi_left)


#### Cargar varios archivos desde un directorio

multiple_sounds(directory = "Alameda de Leon", resultfile = "ndsi_results.csv", soundindex = "ndsi", no_cores = "max")
multiple_sounds(directory = "Alameda de Leon", resultfile = "acoustic_complexity_results.csv", soundindex = "acoustic_complexity", no_cores = "max")
multiple_sounds(directory = "Alameda de Leon", resultfile = "acoustic_diversity_results.csv", soundindex = "acoustic_diversity", no_cores = "max")
multiple_sounds(directory = "Alameda de Leon", resultfile = "acoustic_evenness_results.csv", soundindex = "acoustic_evenness", no_cores = "max")
multiple_sounds(directory = "Alameda de Leon", resultfile = "bioacoustic_index_results.csv", soundindex = "bioacoustic_index", no_cores = "max")
