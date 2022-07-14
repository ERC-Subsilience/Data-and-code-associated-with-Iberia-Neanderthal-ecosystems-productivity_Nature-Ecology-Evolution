rm(list = ls()) # Clear all

## Load libraries

library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(openxlsx)
library(gridExtra)
library(ggpubr)
library(ie2misc)


### Choose the directory

setwd("C:/...")


##### Function to estimate the population density 


Estimate_Population_Density <- function(x)
{


  ### Herbivore densities
  Fauna_BodyMass.df<- as.data.frame(Fauna_BodyMass)

  Dens.mean <- (x / sum(Fauna_BodyMass.df^0.25)) * Fauna_BodyMass.df ^-0.75

  Mean<- Dens.mean[,1]

  Herbivore_Density<- as.data.frame (Mean)
  Herbivore_Density$Species <- Species_List
  Herbivore_Density$BM <-Fauna_BodyMass
  Herbivore_Density$Observed<- Observed_Dens
  Herbivore_Density$Area<- Locality

  return(Herbivore_Density)
}



###### Dataset

Fauna <- read.xlsx("HB_ValidationDataset.xlsx", rowNames=FALSE,
                               colNames=TRUE, sheet="Sheet1")

head(Fauna)


#__________________

### Estimated herbivore population densities in each reserve/national park:

#__________________

# Amboseli
Fauna_BodyMass <- na.omit (Fauna$Bm_Amboseli)
Species_List <- na.omit (Fauna$Species_Amboseli)
Observed_Dens<- na.omit(Fauna$Dens_Amboseli)
Locality <- "Amboseli"
HB<- 10^(Fauna$Amboseli_HB[1])
HB

Density_Amboseli <- Estimate_Population_Density(HB)
Density_Amboseli


Amboseli <- ggplot(data=Density_Amboseli, aes(x=log10(Observed), y=log10(Mean) ))+
  geom_point() + 
  geom_smooth(method="lm") +
  stat_cor(aes(label = ..r.label..)) +
  xlab("Observed")+
  ylab("Predicted") + ggtitle("Amboseli")+theme(plot.title = element_text(size = 8, face = "bold"))

Amboseli


# Etosha
Fauna_BodyMass <- na.omit (Fauna$Bm_Etosha)
Species_List <- na.omit (Fauna$Species_Etosha)
Observed_Dens<- na.omit(Fauna$Dens_Etosha)
Locality <- "Etosha"
HB<- 10^(Fauna$Etosha_HB[1])
HB

Density_Etosha <- Estimate_Population_Density(HB)
Density_Etosha


Etosha <- ggplot(data=Density_Etosha, aes(x=log10(Observed), y=log10(Mean)     ))+
  geom_point() + 
  geom_smooth(method="lm") +
  stat_cor(aes(label = ..r.label..)) + xlab("Observed")+ ylab("Predicted") + ggtitle("Etosha")+theme(plot.title = element_text(size = 8, face = "bold"))

Etosha


# Gonarezhou
Fauna_BodyMass <- na.omit (Fauna$Bm_Gonarezhou)
Species_List <- na.omit (Fauna$Species_Gonarezhou)
Observed_Dens<- na.omit(Fauna$Dens_Gonarezhou)
Locality <- "Gonarezhou"
HB<- 10^(Fauna$Gonarezhou_HB[1])
HB

Density_Gonarezhou <- Estimate_Population_Density(HB)
Density_Gonarezhou


Gonarezhou <- ggplot(data=Density_Gonarezhou, aes(x=log10(Observed), y=log10(Mean)     ))+
  geom_point() + 
  geom_smooth(method="lm") +
  stat_cor(aes(label = ..r.label..)) + xlab("Observed")+ ylab("Predicted") + ggtitle("Gonarezhou")+theme(plot.title = element_text(size = 8, face = "bold"))

Gonarezhou



# Hluhluwe

Fauna_BodyMass <- na.omit (Fauna$Bm_Hluhluwe)
Species_List <- na.omit (Fauna$Species_Hluhluwe)
Observed_Dens<- na.omit(Fauna$Dens_Hluhluwe)
Locality <- "Hluhluwe"
HB<- 10^(Fauna$Hluhluwe_HB[1])
HB

Density_Hluhluwe <- Estimate_Population_Density(HB)
Density_Hluhluwe

Hluhluwe <- ggplot(data=Density_Hluhluwe, aes(x=log10(Observed), y=log10(Mean)     ))+
  geom_point() + 
  geom_smooth(method="lm") +
  stat_cor(aes(label = ..r.label..)) + xlab("Observed")+ ylab("Predicted") + ggtitle("Hluhluwe")+theme(plot.title = element_text(size = 8, face = "bold"))

Hluhluwe


# Hwange

Fauna_BodyMass <- na.omit (Fauna$Bm_Hwange)
Species_List <- na.omit (Fauna$Species_Hwange)
Observed_Dens<- na.omit(Fauna$Dens_Hwange)
Locality <- "Hwange"
HB<- 10^(Fauna$Hwange_HB[1])
HB

Density_Hwange <- Estimate_Population_Density(HB)
Density_Hwange

Hwange <- ggplot(data=Density_Hwange, aes(x=log10(Observed), y=log10(Mean)     ))+
  geom_point() + 
  geom_smooth(method="lm") +
  stat_cor(aes(label = ..r.label..)) + xlab("Observed")+ ylab("Predicted") + ggtitle("Hwange")+theme(plot.title = element_text(size = 8, face = "bold"))

Hwange


# Kalahari

Fauna_BodyMass <- na.omit (Fauna$Bm_Kalahari)
Species_List <- na.omit (Fauna$Species_Kalahari)
Observed_Dens<- na.omit(Fauna$Dens_Kalahari)
Locality <- "Kalahari"
HB<- 10^(Fauna$Kalahari_HB[1])
HB

Density_Kalahari <- Estimate_Population_Density(HB)
Density_Kalahari

Kalahari <- ggplot(data=Density_Kalahari, aes(x=log10(Observed), y=log10(Mean)     ))+
  geom_point() + 
  geom_smooth(method="lm") +
  stat_cor(aes(label = ..r.label..)) + xlab("Observed")+ ylab("Predicted") + ggtitle("Kalahari")+theme(plot.title = element_text(size = 8, face = "bold"))

Kalahari

# Katavi

Fauna_BodyMass <- na.omit (Fauna$Bm_Katavi)
Species_List <- na.omit (Fauna$Species_Katavi)
Observed_Dens<- na.omit(Fauna$Dens_Katavi)
Locality <- "Katavi"
HB<- 10^(Fauna$Katavi_HB[1])
HB

Density_Katavi <- Estimate_Population_Density(HB)
Density_Katavi

Katavi <- ggplot(data=Density_Katavi, aes(x=log10(Observed), y=log10(Mean)     ))+
  geom_point() + 
  geom_smooth(method="lm") +
  stat_cor(aes(label = ..r.label..)) + xlab("Observed")+ ylab("Predicted") + ggtitle("Katavi")+theme(plot.title = element_text(size = 8, face = "bold"))

Katavi


# Kidepo

Fauna_BodyMass <- na.omit (Fauna$Bm_Kidepo)
Species_List <- na.omit (Fauna$Species_Kidepo)
Observed_Dens<- na.omit(Fauna$Dens_Kidepo)
Locality <- "Kidepo"
HB<- 10^(Fauna$Kidepo_HB[1])
HB

Density_Kidepo <- Estimate_Population_Density(HB)
Density_Kidepo

Kidepo <- ggplot(data=Density_Kidepo, aes(x=log10(Observed), y=log10(Mean)))+
  geom_point() + 
  geom_smooth(method="lm") +
  stat_cor(aes(label = ..r.label..)) + xlab("Observed")+ ylab("Predicted") + ggtitle("Kidepo")+theme(plot.title = element_text(size = 8, face = "bold"))

Kidepo


# Kruger

Fauna_BodyMass <- na.omit (Fauna$Bm_Kruger)
Species_List <- na.omit (Fauna$Species_Kruger)
Observed_Dens<- na.omit(Fauna$Dens_Kruger)
Locality <- "Kruger"
HB<- 10^(Fauna$Kruger_HB[1])
HB

Density_Kruger <- Estimate_Population_Density(HB)
Density_Kruger
Kruger <- ggplot(data=Density_Kruger, aes(x=log10(Observed), y=log10(Mean)))+
  geom_point() + 
  geom_smooth(method="lm") +
  stat_cor(aes(label = ..r.label..)) + xlab("Observed")+ ylab("Predicted") + ggtitle("Kruger")+theme(plot.title = element_text(size = 8, face = "bold"))
Kruger


# MasaiMara

Fauna_BodyMass <- na.omit (Fauna$Bm_MasaiMara)
Species_List <- na.omit (Fauna$Species_MasaiMara)
Observed_Dens<- na.omit(Fauna$Dens_MasaiMara)
Locality <- "MasaiMara"
HB<- 10^(Fauna$MasaiMara_HB[1])
HB

Density_MasaiMara <- Estimate_Population_Density(HB)
Density_MasaiMara
MasaiMara <- ggplot(data=Density_MasaiMara, aes(x=log10(Observed), y=log10(Mean)))+
  geom_point() + 
  geom_smooth(method="lm") +
  stat_cor(aes(label = ..r.label..)) + xlab("Observed")+ ylab("Predicted") + ggtitle("Masa mara")+theme(plot.title = element_text(size = 8, face = "bold"))
MasaiMara


# Manyara

Fauna_BodyMass <- na.omit (Fauna$Bm_Manyara)
Species_List <- na.omit (Fauna$Species_Manyara)
Observed_Dens<- na.omit(Fauna$Dens_Manyara)
Locality <- "Manyara"
HB<- 10^(Fauna$Manyara_HB[1])
HB

Density_Manyara <- Estimate_Population_Density(HB)
Density_Manyara
Manyara <- ggplot(data=Density_Manyara, aes(x=log10(Observed), y=log10(Mean)))+
  geom_point() + 
  geom_smooth(method="lm") +
  stat_cor(aes(label = ..r.label..)) + xlab("Observed")+ ylab("Predicted") + ggtitle("Manyara")+theme(plot.title = element_text(size = 8, face = "bold"))
Manyara


# Mkomazi

Fauna_BodyMass <- na.omit (Fauna$Bm_Mkomazi)
Species_List <- na.omit (Fauna$Species_Mkomazi)
Observed_Dens<- na.omit(Fauna$Dens_Mkomazi)
Locality <- "Mkomazi"
HB<- 10^(Fauna$Mkomazi_HB[1])
HB

Density_Mkomazi <- Estimate_Population_Density(HB)
Density_Mkomazi
Mkomazi <- ggplot(data=Density_Mkomazi, aes(x=log10(Observed), y=log10(Mean)))+
  geom_point() + 
  geom_smooth(method="lm") +
  stat_cor(aes(label = ..r.label..)) + xlab("Observed")+ ylab("Predicted") + ggtitle("Mkomazi")+theme(plot.title = element_text(size = 8, face = "bold"))
Mkomazi


# Nairobi

Fauna_BodyMass <- na.omit (Fauna$Bm_Nairobi)
Species_List <- na.omit (Fauna$Species_Nairobi)
Observed_Dens<- na.omit(Fauna$Dens_Nairobi)
Locality <- "Nairobi"
HB<- 10^(Fauna$Nairobi_HB[1])
HB

Density_Nairobi <- Estimate_Population_Density(HB)
Density_Nairobi
Nairobi <- ggplot(data=Density_Nairobi, aes(x=log10(Observed), y=log10(Mean)))+
  geom_point() + 
  geom_smooth(method="lm") +
  stat_cor(aes(label = ..r.label..)) + xlab("Observed")+ ylab("Predicted") + ggtitle("Nairobi")+theme(plot.title = element_text(size = 8, face = "bold"))
Nairobi

# Ngorongoro

Fauna_BodyMass <- na.omit (Fauna$Bm_Ngorongoro)
Species_List <- na.omit (Fauna$Species_Ngorongoro)
Observed_Dens<- na.omit(Fauna$Dens_Ngorongoro)
Locality <- "Ngorongoro"
HB<- 10^(Fauna$Ngorongoro_HB[1])
HB

Density_Ngorongoro <- Estimate_Population_Density(HB)
Density_Ngorongoro
Ngorongoro <- ggplot(data=Density_Ngorongoro, aes(x=log10(Observed), y=log10(Mean)))+
  geom_point() + 
  geom_smooth(method="lm") +
  stat_cor(aes(label = ..r.label..)) + xlab("Observed")+ ylab("Predicted") + ggtitle("Ngorongoro")+theme(plot.title = element_text(size = 8, face = "bold"))
Ngorongoro


# Nwaswitshaka

Fauna_BodyMass <- na.omit (Fauna$Bm_Nwaswitshaka)
Species_List <- na.omit (Fauna$Species_Nwaswitshaka)
Observed_Dens<- na.omit(Fauna$Dens_Nwaswitshaka)
Locality <- "Nwaswitshaka"
HB<- 10^(Fauna$Nwaswitshaka_HB[1])
HB

Density_Nwaswitshaka <- Estimate_Population_Density(HB)
Density_Nwaswitshaka
Nwaswitshaka <- ggplot(data=Density_Nwaswitshaka, aes(x=log10(Observed), y=log10(Mean)))+
  geom_point() + 
  geom_smooth(method="lm") +
  stat_cor(aes(label = ..r.label..)) + xlab("Observed")+ ylab("Predicted") + ggtitle("Nwaswitshaka")+theme(plot.title = element_text(size = 8, face = "bold"))
Nwaswitshaka


# Okavango

Fauna_BodyMass <- na.omit (Fauna$Bm_Okavango)
Species_List <- na.omit (Fauna$Species_Okavango)
Observed_Dens<- na.omit(Fauna$Dens_Okavango)
Locality <- "Okavango"
HB<- 10^(Fauna$Okavango_HB[1])
HB

Density_Okavango <- Estimate_Population_Density(HB)
Density_Okavango
Okavango <- ggplot(data=Density_Okavango, aes(x=log10(Observed), y=log10(Mean)))+
  geom_point() + 
  geom_smooth(method="lm") +
  stat_cor(aes(label = ..r.label..)) + xlab("Observed")+ ylab("Predicted") + ggtitle("Okavango")+theme(plot.title = element_text(size = 8, face = "bold"))
Okavango


# Pilanesburg

Fauna_BodyMass <- na.omit (Fauna$Bm_Pilanesburg)
Species_List <- na.omit (Fauna$Species_Pilanesburg)
Observed_Dens<- na.omit(Fauna$Dens_Pilanesburg)
Locality <- "Pilanesburg"
HB<- 10^(Fauna$Pilanesburg_HB[1])
HB

Density_Pilanesburg <- Estimate_Population_Density(HB)
Density_Pilanesburg
Pilanesburg <- ggplot(data=Density_Pilanesburg, aes(x=log10(Observed), y=log10(Mean)))+
  geom_point() + 
  geom_smooth(method="lm") +
  stat_cor(aes(label = ..r.label..)) + xlab("Observed")+ ylab("Predicted") + ggtitle("Pilanesburg")+theme(plot.title = element_text(size = 8, face = "bold"))
Pilanesburg


# QueenE

Fauna_BodyMass <- na.omit (Fauna$Bm_QueenE)
Species_List <- na.omit (Fauna$Species_QueenE)
Observed_Dens<- na.omit(Fauna$Dens_QueenE)
Locality <- "QueenE"
HB<- 10^(Fauna$QueenE_HB[1])
HB

Density_QueenE <- Estimate_Population_Density(HB)
Density_QueenE
QueenE <- ggplot(data=Density_QueenE, aes(x=log10(Observed), y=log10(Mean)))+
  geom_point() + 
  geom_smooth(method="lm") +
  stat_cor(aes(label = ..r.label..)) + xlab("Observed")+ ylab("Predicted") + ggtitle("Queen Eliz.")+theme(plot.title = element_text(size = 8, face = "bold"))
QueenE



# Serengeti

Fauna_BodyMass <- na.omit (Fauna$Bm_Serengeti)
Species_List <- na.omit (Fauna$Species_Serengeti)
Observed_Dens<- na.omit(Fauna$Dens_Serengeti)
Locality <- "Serengeti"
HB<- 10^(Fauna$Serengeti_HB[1])
HB

Density_Serengeti <- Estimate_Population_Density(HB)
Density_Serengeti
Serengeti <- ggplot(data=Density_Serengeti, aes(x=log10(Observed), y=log10(Mean)))+
  geom_point() + 
  geom_smooth(method="lm") +
  stat_cor(aes(label = ..r.label..)) + xlab("Observed")+ ylab("Predicted") + ggtitle("Serengeti")+theme(plot.title = element_text(size = 8, face = "bold"))
Serengeti


# Tarangire

Fauna_BodyMass <- na.omit (Fauna$Bm_Tarangire)
Species_List <- na.omit (Fauna$Species_Tarangire)
Observed_Dens<- na.omit(Fauna$Dens_Tarangire)
Locality <- "Tarangire"
HB<- 10^(Fauna$Tarangire_HB[1])
HB

Density_Tarangire <- Estimate_Population_Density(HB)
Density_Tarangire
Tarangire <- ggplot(data=Density_Tarangire, aes(x=log10(Observed), y=log10(Mean)))+
  geom_point() + 
  geom_smooth(method="lm") +
  stat_cor(aes(label = ..r.label..)) + xlab("Observed")+ ylab("Predicted") + ggtitle("Tarangire")+theme(plot.title = element_text(size = 8, face = "bold"))
Tarangire

# Bandhavgarh
Fauna_BodyMass <- na.omit (Fauna$Bm_Bandhavgarh)
Species_List <- na.omit (Fauna$Species_Bandhavgarh)
Observed_Dens<- na.omit(Fauna$Dens_Bandhavgarh)
Locality <- "Bandhavgarh"
HB<- 10^(Fauna$Bandhavgarh_HB[1])
HB

Density_Bandhavgarh <- Estimate_Population_Density(HB)
Density_Bandhavgarh
Bandhavgarh <- ggplot(data=Density_Bandhavgarh, aes(x=log10(Observed), y=log10(Mean)))+
  geom_point() + 
  geom_smooth(method="lm") +
  stat_cor(aes(label = ..r.label..)) + xlab("Observed")+ ylab("Predicted") + ggtitle("Bandhavgarh")+theme(plot.title = element_text(size = 8, face = "bold"))
Bandhavgarh



# Bandipur

Fauna_BodyMass <- na.omit (Fauna$Bm_Bandipur)
Species_List <- na.omit (Fauna$Species_Bandipur)
Observed_Dens<- na.omit(Fauna$Dens_Bandipur)
Locality <- "Bandipur"
HB<- 10^(Fauna$Bandipur_HB[1])
HB

Density_Bandipur <- Estimate_Population_Density(HB)
Density_Bandipur
Bandipur <- ggplot(data=Density_Bandipur, aes(x=log10(Observed), y=log10(Mean)))+
  geom_point() + 
  geom_smooth(method="lm") +
  stat_cor(aes(label = ..r.label..)) + xlab("Observed")+ ylab("Predicted") + ggtitle("Bandipur")+theme(plot.title = element_text(size = 8, face = "bold"))
Bandipur


# Bhadra
Fauna_BodyMass <- na.omit (Fauna$Bm_Bhadra)
Species_List <- na.omit (Fauna$Species_Bhadra)
Observed_Dens<- na.omit(Fauna$Dens_Bhadra)
Locality <- "Bhadra"
HB<- 10^(Fauna$Bhadra_HB[1])
HB

Density_Bhadra <- Estimate_Population_Density(HB)
Density_Bhadra
Bhadra <- ggplot(data=Density_Bhadra, aes(x=log10(Observed), y=log10(Mean) ))+
  geom_point() + 
  geom_smooth(method="lm") +
  stat_cor(aes(label = ..r.label..)) + xlab("Observed")+ ylab("Predicted") + ggtitle("Bhadra")+theme(plot.title = element_text(size = 8, face = "bold"))
Bhadra

# Bori

Fauna_BodyMass <- na.omit (Fauna$Bm_Bori)
Species_List <- na.omit (Fauna$Species_Bori)
Observed_Dens<- na.omit(Fauna$Dens_Bori)
Locality <- "Bori"
HB<- 10^(Fauna$Bori_HB[1])
HB

Density_Bori <- Estimate_Population_Density(HB)
Density_Bori
Bori <- ggplot(data=Density_Bori, aes(x=log10(Observed), y=log10(Mean)))+
  geom_point() + 
  geom_smooth(method="lm") +
  stat_cor(aes(label = ..r.label..)) + xlab("Observed")+ ylab("Predicted") + ggtitle("Bori")+theme(plot.title = element_text(size = 8, face = "bold"))
Bori

# Buxa
Fauna_BodyMass <- na.omit (Fauna$Bm_Buxa)
Species_List <- na.omit (Fauna$Species_Buxa)
Observed_Dens<- na.omit(Fauna$Dens_Buxa)
Locality <- "Buxa"
HB<- 10^(Fauna$Buxa_HB[1])
HB

Density_Buxa <- Estimate_Population_Density(HB)
Density_Buxa
Buxa <- ggplot(data=Density_Buxa, aes(x=log10(Observed), y=log10(Mean) ))+
  geom_point() + 
  geom_smooth(method="lm") +
  stat_cor(aes(label = ..r.label..)) + xlab("Observed")+ ylab("Predicted") + ggtitle("Buxa")+theme(plot.title = element_text(size = 8, face = "bold"))
Buxa

# Corbett

Fauna_BodyMass <- na.omit (Fauna$Bm_Corbett)
Species_List <- na.omit (Fauna$Species_Corbett)
Observed_Dens<- na.omit(Fauna$Dens_Corbett)
Locality <- "Corbett"
HB<- 10^(Fauna$Corbett_HB[1])
HB

Density_Corbett <- Estimate_Population_Density(HB)
Density_Corbett
Corbett <- ggplot(data=Density_Corbett, aes(x=log10(Observed), y=log10(Mean) ))+
  geom_point() + 
  geom_smooth(method="lm") +
  stat_cor(aes(label = ..r.label..)) + xlab("Observed")+ ylab("Predicted") + ggtitle("Corbett")+theme(plot.title = element_text(size = 8, face = "bold"))
Corbett

# Kalakad


Fauna_BodyMass <- na.omit (Fauna$Bm_Kalakad)
Species_List <- na.omit (Fauna$Species_Kalakad)
Observed_Dens<- na.omit(Fauna$Dens_Kalakad)
Locality <- "Kalakad"
HB<- 10^(Fauna$Kalakad_HB[1])
HB

Density_Kalakad <- Estimate_Population_Density(HB)
Density_Kalakad
Kalakad <- ggplot(data=Density_Kalakad, aes(x=log10(Observed), y=log10(Mean)))+
  geom_point() + 
  geom_smooth(method="lm") +
  stat_cor(aes(label = ..r.label..)) + xlab("Observed")+ ylab("Predicted") + ggtitle("Kalakad")+theme(plot.title = element_text(size = 8, face = "bold"))
Kalakad

# Phen

Fauna_BodyMass <- na.omit (Fauna$Bm_Phen)
Species_List <- na.omit (Fauna$Species_Phen)
Observed_Dens<- na.omit(Fauna$Dens_Phen)
Locality <- "Phen"
HB<- 10^(Fauna$Phen_HB[1])
HB

Density_Phen <- Estimate_Population_Density(HB)
Density_Phen
Phen <- ggplot(data=Density_Phen, aes(x=log10(Observed), y=log10(Mean)))+
  geom_point() + 
  geom_smooth(method="lm") +
  stat_cor(aes(label = ..r.label..)) + xlab("Observed")+ ylab("Predicted") + ggtitle("Phen")+theme(plot.title = element_text(size = 8, face = "bold"))
Phen

# Melghat


Fauna_BodyMass <- na.omit (Fauna$Bm_Melghat)
Species_List <- na.omit (Fauna$Species_Melghat)
Observed_Dens<- na.omit(Fauna$Dens_Melghat)
Locality <- "Melghat"
HB<- 10^(Fauna$Melghat_HB[1])
HB

Density_Melghat <- Estimate_Population_Density(HB)
Density_Melghat
Melghat <- ggplot(data=Density_Melghat, aes(x=log10(Observed), y=log10(Mean)))+
  geom_point() + 
  geom_smooth(method="lm") +
  stat_cor(aes(label = ..r.label..)) + xlab("Observed")+ ylab("Predicted") + ggtitle("Melghat")+theme(plot.title = element_text(size = 8, face = "bold"))
Melghat

# Nagarahole



Fauna_BodyMass <- na.omit (Fauna$Bm_Nagarahole)
Species_List <- na.omit (Fauna$Species_Nagarahole)
Observed_Dens<- na.omit(Fauna$Dens_Nagarahole)
Locality <- "Nagarahole"
HB<- 10^(Fauna$Nagarahole_HB[1])
HB

Density_Nagarahole <- Estimate_Population_Density(HB)
Density_Nagarahole
Nagarahole <- ggplot(data=Density_Nagarahole, aes(x=log10(Observed), y=log10(Mean)))+
  geom_point() + 
  geom_smooth(method="lm") +
  stat_cor(aes(label = ..r.label..)) + xlab("Observed")+ ylab("Predicted") + ggtitle("Nagarahole")+theme(plot.title = element_text(size = 8, face = "bold"))
Nagarahole

# Palamau

Fauna_BodyMass <- na.omit (Fauna$Bm_Palamau)
Species_List <- na.omit (Fauna$Species_Palamau)
Observed_Dens<- na.omit(Fauna$Dens_Palamau)
Locality <- "Palamau"
HB<- 10^(Fauna$Palamau_HB[1])
HB

Density_Palamau <- Estimate_Population_Density(HB)
Density_Palamau
Palamau <- ggplot(data=Density_Palamau, aes(x=log10(Observed), y=log10(Mean)))+
  geom_point() + 
  geom_smooth(method="lm") +
  stat_cor(aes(label = ..r.label..)) + xlab("Observed")+ ylab("Predicted") + ggtitle("Palamau")+theme(plot.title = element_text(size = 8, face = "bold"))
Palamau

# Panna


Fauna_BodyMass <- na.omit (Fauna$Bm_Panna)
Species_List <- na.omit (Fauna$Species_Panna)
Observed_Dens<- na.omit(Fauna$Dens_Panna)
Locality <- "Panna"
HB<- 10^(Fauna$Panna_HB[1])
HB

Density_Panna <- Estimate_Population_Density(HB)
Density_Panna
Panna <- ggplot(data=Density_Panna, aes(x=log10(Observed), y=log10(Mean)))+
  geom_point() + 
  geom_smooth(method="lm") +
  stat_cor(aes(label = ..r.label..)) + xlab("Observed")+ ylab("Predicted") + ggtitle("Panna")+theme(plot.title = element_text(size = 8, face = "bold"))
Panna

# Pench

Fauna_BodyMass <- na.omit (Fauna$Bm_Pench)
Species_List <- na.omit (Fauna$Species_Pench)
Observed_Dens<- na.omit(Fauna$Dens_Pench)
Locality <- "Pench"
HB<- 10^(Fauna$Pench_HB[1])
HB

Density_Pench <- Estimate_Population_Density(HB)
Density_Pench
Pench <- ggplot(data=Density_Pench, aes(x=log10(Observed), y=log10(Mean)))+
  geom_point() + 
  geom_smooth(method="lm") +
  stat_cor(aes(label = ..r.label..)) + xlab("Observed")+ ylab("Predicted") + ggtitle("Pench")+theme(plot.title = element_text(size = 8, face = "bold"))
Pench

# Periyar
Fauna_BodyMass <- na.omit (Fauna$Bm_Periyar)
Species_List <- na.omit (Fauna$Species_Periyar)
Observed_Dens<- na.omit(Fauna$Dens_Periyar)
Locality <- "Periyar"
HB<- 10^(Fauna$Periyar_HB[1])
HB

Density_Periyar <- Estimate_Population_Density(HB)
Density_Periyar
Periyar <- ggplot(data=Density_Periyar, aes(x=log10(Observed), y=log10(Mean)))+
  geom_point() + 
  geom_smooth(method="lm") +
  stat_cor(aes(label = ..r.label..)) + xlab("Observed")+ ylab("Predicted") + ggtitle("Periyar")+theme(plot.title = element_text(size = 8, face = "bold"))
Periyar

# Teslin
Fauna_BodyMass <- na.omit (Fauna$Bm_Teslin)
Species_List <- na.omit (Fauna$Species_Teslin)
Observed_Dens<- na.omit(Fauna$Dens_Teslin)
Locality <- "Teslin"
HB<- 10^(Fauna$Teslin_HB[1])
HB

Density_Teslin <- Estimate_Population_Density(HB)
Density_Teslin
Teslin <- ggplot(data=Density_Teslin, aes(x=log10(Observed), y=log10(Mean)))+
  geom_point() + 
  geom_smooth(method="lm") +
  stat_cor(aes(label = ..r.label..)) + xlab("Observed")+ ylab("Predicted") + ggtitle("Teslin")+theme(plot.title = element_text(size = 8, face = "bold"))
Teslin

# Panagua
Fauna_BodyMass <- na.omit (Fauna$Bm_Panagua)
Species_List <- na.omit (Fauna$Species_Panagua)
Observed_Dens<- na.omit(Fauna$Dens_Panagua)
Locality <- "Panagua"
HB<- 10^(Fauna$Panagua_HB[1])
HB

Density_Panagua <- Estimate_Population_Density(HB)
Density_Panagua
Panagua <- ggplot(data=Density_Panagua, aes(x=log10(Observed), y=log10(Mean)))+
  geom_point() + 
  geom_smooth(method="lm") +
  stat_cor(aes(label = ..r.label..)) + xlab("Observed")+ ylab("Predicted") + ggtitle("Panagua")+theme(plot.title = element_text(size = 8, face = "bold"))
Panagua

# Denali
Fauna_BodyMass <- na.omit (Fauna$Bm_Denali)
Species_List <- na.omit (Fauna$Species_Denali)
Observed_Dens<- na.omit(Fauna$Dens_Denali)
Locality <- "Denali"
HB<- 10^(Fauna$Denali_HB[1])
HB

Density_Denali <- Estimate_Population_Density(HB)
Density_Denali
Denali <- ggplot(data=Density_Denali, aes(x=log10(Observed), y=log10(Mean) ))+
  geom_point() + 
  geom_smooth(method="lm") +
  stat_cor(aes(label = ..r.label..)) + xlab("Observed")+ ylab("Predicted") + ggtitle("Denali")+theme(plot.title = element_text(size = 8, face = "bold"))
Denali

# Yukon
Fauna_BodyMass <- na.omit (Fauna$Bm_Yukon)
Species_List <- na.omit (Fauna$Species_Yukon)
Observed_Dens<- na.omit(Fauna$Dens_Yukon)
Locality <- "Yukon"
HB<- 10^(Fauna$Yukon_HB[1])
HB

Density_Yukon <- Estimate_Population_Density(HB)
Density_Yukon
Yukon <- ggplot(data=Density_Yukon, aes(x=log10(Observed), y=log10(Mean)))+
  geom_point() + 
  geom_smooth(method="lm") +
  stat_cor(aes(label = ..r.label..)) + xlab("Observed")+ ylab("Predicted") + ggtitle("Yukon")+theme(plot.title = element_text(size = 8, face = "bold"))
Yukon


# ForesteCas
Fauna_BodyMass <- na.omit (Fauna$Bm_ForesteCas)
Species_List <- na.omit (Fauna$Species_ForesteCas)
Observed_Dens<- na.omit(Fauna$Dens_ForesteCas)
Locality <- "ForesteCas"
HB<- 10^(Fauna$ForesteCas_HB[1])
HB

Density_ForesteCas <- Estimate_Population_Density(HB)
Density_ForesteCas
ForesteCas <- ggplot(data=Density_ForesteCas, aes(x=log10(Observed), y=log10(Mean)))+
  geom_point() + 
  geom_smooth(method="lm") +
  stat_cor(aes(label = ..r.label..)) + xlab("Observed")+ ylab("Predicted") + ggtitle("Foreste Cas.")+theme(plot.title = element_text(size = 8, face = "bold"))
ForesteCas

# IndoSum
Fauna_BodyMass <- na.omit (Fauna$Bm_IndoSum)
Species_List <- na.omit (Fauna$Species_IndoSum)
Observed_Dens<- na.omit(Fauna$Dens_IndoSum)
Locality <- "IndoSum"
HB<- 10^(Fauna$IndoSum_HB[1])
HB

Density_IndoSum <- Estimate_Population_Density(HB)
Density_IndoSum
IndoSum <- ggplot(data=Density_IndoSum, aes(x=log10(Observed), y=log10(Mean)))+
  geom_point() + 
  geom_smooth(method="lm") +
  stat_cor(aes(label = ..r.label..)) + xlab("Observed")+ ylab("Predicted") + ggtitle("Ind. Sum.")+theme(plot.title = element_text(size = 8, face = "bold"))
IndoSum

# Kayapo
Fauna_BodyMass <- na.omit (Fauna$Bm_Kayapo)
Species_List <- na.omit (Fauna$Species_Kayapo)
Observed_Dens<- na.omit(Fauna$Dens_Kayapo)
Locality <- "Kayapo"
HB<- 10^(Fauna$Kayapo_HB[1])
HB

Density_Kayapo <- Estimate_Population_Density(HB)
Density_Kayapo
Kayapo <- ggplot(data=Density_Kayapo, aes(x=log10(Observed), y=log10(Mean) ))+
  geom_point() + 
  geom_smooth(method="lm") +
  stat_cor(aes(label = ..r.label..)) + xlab("Observed")+ ylab("Predicted") + ggtitle("Kayapo")+theme(plot.title = element_text(size = 8, face = "bold"))
Kayapo

# Kunlun
Fauna_BodyMass <- na.omit (Fauna$Bm_Kunlun)
Species_List <- na.omit (Fauna$Species_Kunlun)
Observed_Dens<- na.omit(Fauna$Dens_Kunlun)
Locality <- "Kunlun"
HB<- 10^(Fauna$Kunlun_HB[1])
HB

Density_Kunlun <- Estimate_Population_Density(HB)
Density_Kunlun
Kunlun <- ggplot(data=Density_Kunlun, aes(x=log10(Observed), y=log10(Mean)))+
  geom_point() + 
  geom_smooth(method="lm") +
  stat_cor(aes(label = ..r.label..)) + xlab("Observed")+ ylab("Predicted") + ggtitle("Kunlun")+theme(plot.title = element_text(size = 8, face = "bold"))
Kunlun


# Taman
Fauna_BodyMass <- na.omit (Fauna$Bm_Taman)
Species_List <- na.omit (Fauna$Species_Taman)
Observed_Dens<- na.omit(Fauna$Dens_Taman)
Locality <- "Taman"
HB<- 10^(Fauna$Taman_HB[1])
HB

Density_Taman <- Estimate_Population_Density(HB)
Density_Taman

Taman <- ggplot(data=Density_Taman, aes(x=log10(Observed), y=log10(Mean) ))+
  geom_point() + 
  geom_smooth(method="lm") +
  stat_cor(aes(label = ..r.label..)) + xlab("Observed")+ ylab("Predicted") + ggtitle("Taman")+theme(plot.title = element_text(size = 8, face = "bold"))
Taman


EachPlot<- grid.arrange(Amboseli, Etosha, Gonarezhou, Hluhluwe, Hwange, Kalahari, Katavi, Kidepo, Kruger, Manyara, MasaiMara, Mkomazi,
                        Nairobi, Ngorongoro, Nwaswitshaka, Okavango, Pilanesburg, QueenE, Serengeti, Tarangire, Bandhavgarh, Bandipur, Bhadra,
                        Bori, Buxa, Corbett, Kalakad, Phen, Melghat, Nagarahole, Palamau, Panna, Pench, Periyar, Panagua, Teslin, Denali, Yukon,
                         ForesteCas, IndoSum, Kayapo, Kunlun, Taman)

EachPlot

AllDensities <- rbind(Density_Amboseli, Density_Etosha, Density_Gonarezhou, Density_Hluhluwe, Density_Hwange,
                      Density_Kalahari, Density_Katavi, Density_Kidepo, Density_Kruger,
                      Density_Manyara, Density_MasaiMara, Density_Mkomazi, Density_Nairobi, Density_Ngorongoro,
                      Density_Nwaswitshaka, Density_Okavango, Density_Pilanesburg, Density_QueenE,
                      Density_Serengeti, Density_Tarangire, Density_Bandhavgarh, Density_Bandipur, Density_Bhadra,
                      Density_Bori, Density_Buxa, Density_Corbett, Density_Kalakad, Density_Phen, Density_Melghat,
                      Density_Nagarahole, Density_Palamau, Density_Panna,  Density_Pench,
                      Density_Panagua, Density_Teslin, Density_Denali, Density_Yukon,
                      Density_ForesteCas, Density_IndoSum, Density_Kayapo, Density_Kunlun, Density_Taman)




Allplot <- ggplot(data=AllDensities, aes(x=log10(Observed), y=log10(Mean)))+
  geom_point() + 
  geom_smooth(method="lm", se = FALSE)  +
  stat_cor(method = "pearson")

Allplot



