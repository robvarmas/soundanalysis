#Codigo para analisis

setwd("Z:/Roberto/Bioacústica/BioSonidos/Pasantia/analisis")
getwd()

install.packages("ggplot2")
library(ggplot2)

install.packages("car")
library(car)

results_avg <- read.csv(file="results_avg.csv", header=TRUE, sep=";")
View(results_avg)

results_4ch <- read.csv(file="results_4ch.csv", header=TRUE, sep=";")
View(results_4ch)

#Normality test Shapiro-wilk by index
df_test <- results_4ch[,c(7,8,9,10,11,12,13,14,15,16,17,18,19,20)]
shap_test <- lapply(df_test, shapiro.test)
shap_test2 <- sapply(shap_test, `[`, c("statistic","p.value"))
t(shap_test2)

#Evenness variance test Levene by index
leveneTest(results_4ch$AcouOccupancy_left~results_4ch$Site)
leveneTest(results_4ch$Bioac_left~results_4ch$Site)
leveneTest(results_4ch$H_left~results_4ch$Site)
leveneTest(results_4ch$ACI_left~results_4ch$Site)
leveneTest(results_4ch$AEI_villa_left~results_4ch$Site)
leveneTest(results_4ch$NDSI_left~results_4ch$Site)
leveneTest(results_4ch$ADI_left~results_4ch$Site)
leveneTest(results_4ch$npic_left~results_4ch$Site)
leveneTest(results_4ch$Noise~results_4ch$Site)
leveneTest(results_4ch$Richness_sp~results_4ch$Site)
leveneTest(results_4ch$Abundance_sp~results_4ch$Site)


#Boxplot by site
boxplot(Noise ~ Site_code,
        data = results_4ch,
        ylab="Number of peaks",
        xlab="Site")

par(mfrow=c(4,2))
boxplot(AcouOccupancy_left ~ Site_code,
        data = results_4ch,
        ylab="Acoustic Occupancy Index",
        xlab="Site")

boxplot(Bioac_left ~ Site_code,
        data = results_4ch,
        ylab="Bioacoustic Index",
        xlab="Site")

boxplot(H_left ~ Site_code,
        data = results_4ch,
        ylab="Entrophy Index",
        xlab="Site")

boxplot(ACI_left ~ Site_code,
        data = results_4ch,
        ylab="Acoustic Complexity Index",
        xlab="Site")

boxplot(AEI_villa_left ~ Site_code,
        data = results_4ch,
        ylab="Acoustic Evenness Index",
        xlab="Site")

boxplot(NDSI_left ~ Site_code,
        data = results_4ch,
        ylab="Normalized Difference Soundscape Index",
        xlab="Site")

boxplot(ADI_left ~ Site_code,
        data = results_4ch,
        ylab="Acoustic Diversity Index",
        xlab="Site")

boxplot(npic_left ~ Site_code,
        data = results_4ch,
        ylab="Number of peaks",
        xlab="Site")

#Lineal model by site
lm_AO <- lm(AcouOccupancy_left ~ Site, data = results_4ch)
summary(lm_AO)


lm_NO <- lm(Noise ~ Site + Error(Mic/Site), data = results_4ch)
summary(lm_NO)

lm_NO <- aov(Richness_sp ~ Site + Error(Mic/Site), data = results_4ch)
summary(lm_NO)

lm_NO <- aov(Abundance_sp ~ Site + Error(Mic/Site), data = results_4ch)
summary(lm_NO)

#Boxplot by index
par(mfrow=c(4,3)) 
boxplot(results_avg$AcouOccupancy_left, ylab = "Index", main = "Acoustic Occupancy")
boxplot(results_avg$Bioac_left, ylab = "Index", main = "Bioacoustic Index")
boxplot(results_avg$H_left,  ylab = "Index", main = "Entropy")
boxplot(results_avg$ACI_left,  ylab = "Index", main = "Acoustic Complexity")
boxplot(results_avg$AEI_villa_left,  ylab = "Index", main = "Acoustic Evenness")
boxplot(results_avg$NDSI_left,  ylab = "Index", main = "Normalized Difference Soundscape")
boxplot(results_avg$ADI_left,  ylab = "Index", main = "Acoustic Diversity Index")
boxplot(results_avg$npic_left,  ylab = "Number", main = "Number of peaks")
boxplot(results_avg$Noise, ylab = "decibels (dB)", main = "Noise")
boxplot(results_avg$N_species, ylab = "Number", main = "Richness species")
boxplot(results_avg$A_species, ylab = "Number", main = "Abundance species")

#Barplots by sites
par(mfrow=c(4,3))
barplot(results_avg$AcouOccupancy_left, xlab = "Sites", ylab = "Index", main = "Acoustic Occupancy", names.arg = sort(results_avg$Site.code))
barplot(results_avg$Bioac_left, xlab = "Sites", ylab = "Index", main = "Bioacoustic Index", names.arg = sort(results_avg$Site.code))
barplot(results_avg$H_left, xlab = "Sites", ylab = "Index", main = "Entropy", names.arg = sort(results_avg$Site.code))
barplot(results_avg$ACI_left, xlab = "Sites", ylab = "Index", main = "Acoustic Complexity", names.arg = sort(results_avg$Site.code))
barplot(results_avg$AEI_villa_left, xlab = "Sites", ylab = "Index", main = "Acoustic Evenness", names.arg = sort(results_avg$Site.code))
barplot(results_avg$NDSI_left, xlab = "Sites", ylab = "Index", main = "Normalized Difference Soundscape", names.arg = sort(results_avg$Site.code))
barplot(results_avg$ADI_left, xlab = "Sites", ylab = "Index", main = "Acoustic Diversity Index", names.arg = sort(results_avg$Site.code))
barplot(results_avg$npic_left, xlab = "Sites", ylab = "Index", main = "Number of peaks", names.arg = sort(results_avg$Site.code))
barplot(results_avg$Noise, xlab = "Sites", ylab = "Decibels (dB)", main = "Environment Noise", names.arg = sort(results_avg$Site.code))
barplot(results_avg$N_species, xlab = "Sites", ylab = "Number", main = "Number of species", names.arg = sort(results_avg$Site.code))
barplot(results_avg$A_species, xlab = "Sites", ylab = "Number", main = "Abundance of species", names.arg = sort(results_avg$Site.code))


#Spearmans correlations coefficient by site
cor.test(x=results_4ch$AcouOccupancy_left, y=as.vector(results_4ch$Site), method = 'spearman')
