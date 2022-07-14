
rm(list = ls()) # Clear all

## Load libraries

library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(openxlsx)
library(gridExtra)
library(robustbase)
library(rstatix)
library(TSclust)
library(ggdendro)
library(circlize)
library(dendextend)
library(vegan)

################## The script below was written in  Rstudio, so it is  recommended
############ to be run within RStudio.
### Choose your own directory:

setwd("C:/...")


#_________________________________________________________________________________
#
## LOAD FUNCTIONS
#
#_______________________________________________


####### Functions to estimate the total biomass of primary consumers from the estimated NPP <log10 (g/m^2/y)>:

## Open the dataset with  the NPP and the herbivore biomass empirical observations:
Dataset <- read.xlsx("HB_ValidationDataset.xlsx", rowNames=FALSE,
                     colNames=TRUE, sheet="NPP_HB")
## Compute the predictive model. For details, see the script :'HerbivoreBiomassFromNPP.R'
model <- lmrob(logHB~logNPP, data = Dataset) 
## Apply the predictive model to estimate the herbivore biomass and the 95% Confidence Interval of the prediction:
Bpc.mean <- function(NPP) 
{
  # Create a new data frame 
  logNPP <- NPP
  Value <- data.frame(logNPP)
  # Compute the mean herbivore biomass with the 95% CI
  Biomass_PC<- as.data.frame(predict(model, newdata = Value, interval = 'confidence'))
  # report the biomass in kg/km^2
  Biomass_PC <-10^(Biomass_PC$fit)
  return(Biomass_PC)
}
Bpc.max <- function(NPP) 
{
  # Create a new data frame 
  logNPP <- NPP
  Value <- data.frame(logNPP)
  # Compute the mean herbivore biomass with the 95% CI
  Biomass_PC<- as.data.frame(predict(model, newdata = Value, interval = 'confidence'))
  # report the biomass in kg/km^2
  Biomass_PC <-10^(Biomass_PC$upr)
  return(Biomass_PC)
}
Bpc.min <- function(NPP)
{
  # Create a new data frame 
  logNPP <- NPP
  Value <- data.frame(logNPP)
  # Compute the mean herbivore biomass with the 95% CI
  Biomass_PC<- as.data.frame(predict(model, newdata = Value, interval = 'confidence'))
  # report the biomass in kg/km^2
  Biomass_PC <-10^(Biomass_PC$lwr)
  return(Biomass_PC)
}

######## Function to transform the NPP expressed in kg/m^2/yr to log10 (g/m^2/y):

log_transform_NPP <- function(x) 
{
  log_transform_NPP <- log10 (as.numeric(x) * 1000)
  return(log_transform_NPP)
}

####### Compute the coefficient of variation

co.var<-function(x) 100*(sd(x,na.rm=TRUE)/mean(x,na.rm=TRUE))

##### Function to estimate the abundance of each herbivore population species in each Stadial/Interstadial (x)

Estimate_Population_Density <- function(x) 
{
  NPP <- log_transform_NPP( sprintf("%.4f", Summary_NPP$NPP[,1][Summary_NPP$Phase==x]))
  
  ### Total Herbivore Biomass:
  
  Mean.Biomass <- Bpc.mean (NPP)
  Min.Biomass <- Bpc.min (NPP)
  Max.Biomass <- Bpc.max (NPP)
  
  ### Mean population density (Dens.mean) and the 95% CI (Dens.min and Dens.max) of each herbivore species:

  Fauna_BodyMass.df<- as.data.frame(Fauna_BodyMass)
  
  Dens.mean <- (Mean.Biomass / sum(Fauna_BodyMass.df^0.25)) * Fauna_BodyMass.df ^-0.75
  Dens.min <- (Min.Biomass / sum(Fauna_BodyMass.df^0.25)) * Fauna_BodyMass.df ^-0.75
  Dens.max <- (Max.Biomass / sum(Fauna_BodyMass.df^0.25)) * Fauna_BodyMass.df ^-0.75
  
  ## Create a new data frame with the mean population density of each herbivore species, the 95% C.I. of
  ## the estimations and the body size category:

  Mean<- Dens.mean[,1]
  Min <- Dens.min [,1]
  Max <- Dens.max [,1]
  
  Herbivore_Density<- as.data.frame (t(rbind(Mean, Min, Max)))
  Herbivore_Density$Species <- Species_List
  Herbivore_Density$BM <-Fauna_BodyMass
  Herbivore_Density$SC <- SizeCategory
  Herbivore_Density$Phase <- x
  
  return(Herbivore_Density)
}


#### Function to estimate the Biomass (kg/km^2/yr) of each herbivore population according to the 
#### weight size category and the 95% CI:

  Estimate_Biomass_Per_Size_Category <- function(x) 
  {
  df <- as.data.frame(x)
  Biomass_SmallSize_Herbivores_Mean <- sum(subset(df, SC=="S")$BM * subset(df, SC=="S")$Mean)
  Biomass_SmallSize_Herbivores_Min <- sum(subset(df, SC=="S")$BM * subset(df, SC=="S")$Min)
  Biomass_SmallSize_Herbivores_Max <- sum(subset(df, SC=="S")$BM * subset(df, SC=="S")$Max)
  Biomass_MediumSize_Herbivores_Mean <- sum(subset(df, SC=="M")$BM * subset(df, SC=="M")$Mean)
  Biomass_MediumSize_Herbivores_Min <- sum(subset(df, SC=="M")$BM * subset(df, SC=="M")$Min)
  Biomass_MediumSize_Herbivores_Max <- sum(subset(df, SC=="M")$BM * subset(df, SC=="M")$Max)
  Biomass_MediumLarge_Herbivores_Mean <- sum(subset(df, SC=="ML")$BM * subset(df, SC=="ML")$Mean)
  Biomass_MediumLarge_Herbivores_Min <- sum(subset(df, SC=="ML")$BM * subset(df, SC=="ML")$Min)
  Biomass_MediumLarge_Herbivores_Max <- sum(subset(df, SC=="ML")$BM * subset(df, SC=="ML")$Max)
  Biomass_Large_Herbivores_Mean <- sum(subset(df, SC=="L")$BM * subset(df, SC=="L")$Mean)
  Biomass_Large_Herbivores_Min <- sum(subset(df, SC=="L")$BM * subset(df, SC=="L")$Min)
  Biomass_Large_Herbivores_Max <- sum(subset(df, SC=="L")$BM * subset(df, SC=="L")$Max)
  
  Herbivore_Biomass<- as.data.frame (t(rbind(Biomass_SmallSize_Herbivores_Mean, Biomass_SmallSize_Herbivores_Min, Biomass_SmallSize_Herbivores_Max,
                                             Biomass_MediumSize_Herbivores_Mean, Biomass_MediumSize_Herbivores_Min, Biomass_MediumSize_Herbivores_Max,
                                             Biomass_MediumLarge_Herbivores_Mean, Biomass_MediumLarge_Herbivores_Min, Biomass_MediumLarge_Herbivores_Max,
                                             Biomass_Large_Herbivores_Mean, Biomass_Large_Herbivores_Min, Biomass_Large_Herbivores_Max )))
  Herbivore_Biomass$Phase <- Phase_Stadial_Interstadial
  
  return(Herbivore_Biomass)
  }
  

  #_________________________________________________________________________________
  #
  ## CLUSTER CLASSIFICATION OF THE ESTIMATED NPP AND THE COMPOSITION OF HERBIVORE PALEOCOMMUNITIES (PCOM)  
  #
  #_______________________________________________
  
  ### NPP comparison according to the DCORT method 
  
  NPP <- read.xlsx("Dataset-NPP.xlsx", rowNames=FALSE,
                   colNames=TRUE, sheet="ALL.NPP")
  
  
  ### Compute the DCORT dissimilarity index to compare the similarity of the 
  ### evolutionary trends in the estimated NPP during the MIS 3 
  
  DCORT <- TSclust::diss(SERIES = t(NPP), METHOD = "CORT") 
  hc <- stats::hclust(DCORT, method="complete") # Clustering from the DCORT outcomes
  
  ## Plot 

  hc <- hc %>%
    color_branches(k = 3) %>%
    set("branches_lwd", 3) %>%
    color_labels (k = 3)
  
  circlize_dendrogram(hc,facing = "outside", labels = TRUE, 
                      dend_track_height = 0.7, labels_track_height = 0.2)  
  
  ### PCOMs comparison according to Jaccard Similarity Index (JSI)
  
  Fauna <- read.xlsx("Herbivore.Species.xlsx", rowNames=FALSE,
                     colNames=TRUE, sheet="Jaccard.Index")
  
 ### Compute the JSI
  JSI<-vegdist(Fauna[,2:25] ,method="jaccard", na.rm = TRUE)
  hc <- hclust(JSI)  
  
  # Plot
  plot(hc, labels=Fauna[,1], hang=-1)
  
  
  
  
  #_________________________________________________________________________________________________________
  #
  #
  #
  ####### COMPUTE THE HERBIVORE ABUNDANCE IN EACH REGION DURING EACH STADIAL/ITNERSTADIAL PERIOD OF THE MIS 3
  #
  #
  #
  #_________________________________________________________________________________________________________
  
  
 
#_________________________________________________________________________________
#
############################################ IBERIA: Eurosiberian zone  ############################################
#
#_________________________________________________________________________________
  

### NET PRIMARY PRODUCTIVITY (NPP):

NPP.Eurosiberian <- read.xlsx("Dataset-NPP.xlsx", rowNames=FALSE,
                              colNames=TRUE, sheet="NPP.Eurosiberian")


### Mean NPP in all stadial/interstadial periods of the late MIS 3:

Stadials <- subset(NPP.Eurosiberian, Phase=="GS-13"| Phase=="GS-12"| Phase=="GS-11"| Phase=="GS-10"| Phase=="GS-9"
                   | Phase=="GS-8"| Phase=="GS-7"| Phase=="GS-6"| Phase=="GS-5")
Stadials$Stadial_Interstadial="Stadial"

Interstadials <- subset(NPP.Eurosiberian, Phase=="GI-13"| Phase=="GI-12"| Phase=="GI-11"| Phase=="GI-10"| Phase=="GI-9"
                   | Phase=="GI-8"| Phase=="GI-7"| Phase=="GI-6"| Phase=="GI-5")
Interstadials$Stadial_Interstadial="Interstadial"

Dataset <- rbind(Stadials, Interstadials)

Summary_NPP <- aggregate(NPP~Stadial_Interstadial,data=Dataset, function(x) c(mean=mean(x), sd = sd(x), cv=co.var(x)))
Summary_NPP

# Wilcox rank sum test
wilcox.test(NPP ~ Stadial_Interstadial, alternative="two.sided", data=Dataset)


#### NPP in each  stadial-interstadial

Summary_NPP <- aggregate(NPP~Phase,data=NPP.Eurosiberian, function(x) c(mean=mean(x), sd = sd(x)))
Summary_NPP

# Save outputs:
# write.csv(Summary_NPP, "NPP_Mesotemperate.csv")

### Plot the temporal evolution of the NPP in the Eurosiberian region: 

Eurosiberian_Iberia_NPP_Plot <- ggplot(NPP.Eurosiberian, aes(x = Age, y = NPP, ymin= NPP - (2*SD),
                                                                   ymax= NPP + (2 * SD))) +
  geom_line( color="black", size = 1) +
  scale_x_reverse(breaks = seq(30000, 55000, 5000)) +
  geom_ribbon(alpha= 0.45) +
  ggtitle ("Eurosiberian region") +
  xlab ("age BP")+
  ylab ("NPP (kg/m^2/yr)") +
  ylim(0, 0.6) +
  theme_classic(base_size = 14, base_family = "serif")+ 
  theme(legend.position = "none")

Eurosiberian_Iberia_NPP_Plot


#__________________

## HERBIVORE ABUNDANCE PER STAIDAL/INTERSTADIAL:

#__________________


###### Primary consumer species and their body masses (BM)

Fauna <- read.xlsx("Herbivore.Species.xlsx", rowNames=FALSE,
                               colNames=TRUE, sheet="Fauna.EuroSiberian")


### Abundance of herbivore species recovered from the archeo-paleontological assemblages covering
### the period comprised between the GI13 and the GI9 in the Eurosiberian region:

Fauna_BodyMass <- na.omit (Fauna$BM_GI13_GI9)
Species_List <- na.omit (Fauna$Species_GI13_GI9)
SizeCategory <- na.omit(Fauna$SC_GI13_GI9)

Phase_Stadial_Interstadial <- "GI-13"
Density_GI13<- Estimate_Population_Density("GI-13")
Biomass_GI13<- Estimate_Biomass_Per_Size_Category(Density_GI13)
Biomass_GI13$SI <- "Interstadial"

Phase_Stadial_Interstadial <- "GS-13"
Density_GS13<- Estimate_Population_Density("GS-13")
Biomass_GS13<- Estimate_Biomass_Per_Size_Category(Density_GS13)
Biomass_GS13$SI <- "Stadial"

Phase_Stadial_Interstadial <- "GI-12"
Density_GI12<- Estimate_Population_Density("GI-12")
Biomass_GI12<- Estimate_Biomass_Per_Size_Category(Density_GI12)
Biomass_GI12$SI <- "Interstadial"

Phase_Stadial_Interstadial <- "GS-12"
Density_GS12<- Estimate_Population_Density("GS-12")
Biomass_GS12<- Estimate_Biomass_Per_Size_Category(Density_GS12)
Biomass_GS12$SI <- "Stadial"

Phase_Stadial_Interstadial <- "GI-11"
Density_GI11<- Estimate_Population_Density("GI-11")
Biomass_GI11<- Estimate_Biomass_Per_Size_Category(Density_GI11)
Biomass_GI11$SI <- "Interstadial"

Phase_Stadial_Interstadial <- "GS-11"
Density_GS11<- Estimate_Population_Density("GS-11")
Biomass_GS11<- Estimate_Biomass_Per_Size_Category(Density_GS11)
Biomass_GS11$SI <- "Stadial"

Phase_Stadial_Interstadial <- "GI-10"
Density_GI10<- Estimate_Population_Density("GI-10")
Biomass_GI10<- Estimate_Biomass_Per_Size_Category(Density_GI10)
Biomass_GI10$SI <- "Interstadial"

Phase_Stadial_Interstadial <- "GS-10"
Density_GS10<- Estimate_Population_Density("GS-10")
Biomass_GS10<- Estimate_Biomass_Per_Size_Category(Density_GS10)
Biomass_GS10$SI <- "Stadial"

Phase_Stadial_Interstadial <- "GI-9"
Density_GI9<- Estimate_Population_Density("GI-9")
Biomass_GI9<- Estimate_Biomass_Per_Size_Category(Density_GI9)
Biomass_GI9$SI <- "Interstadial"

### Abundance of herbivore species recovered from the archaeo-paleontological assemblages covering
### the period comprised between the GS9 and the GI7 in the Eurosiberian region:

Fauna_BodyMass <- na.omit (Fauna$BM_GS9_GI7)
Species_List <- na.omit (Fauna$Species_GS9_GI7)
SizeCategory <- na.omit(Fauna$SC_GS9_GI7)

Phase_Stadial_Interstadial <- "GS-9"
Density_GS9<- Estimate_Population_Density("GS-9")
Biomass_GS9<- Estimate_Biomass_Per_Size_Category(Density_GS9)
Biomass_GS9$SI <- "Stadial"

Phase_Stadial_Interstadial <- "GI-8"
Density_GI8<- Estimate_Population_Density("GI-8")
Biomass_GI8<- Estimate_Biomass_Per_Size_Category(Density_GI8)
Biomass_GI8$SI <- "Interstadial"

Phase_Stadial_Interstadial <- "GS-8"
Density_GS8<- Estimate_Population_Density("GS-8")
Biomass_GS8<- Estimate_Biomass_Per_Size_Category(Density_GS8)
Biomass_GS8$SI <- "Stadial"

Phase_Stadial_Interstadial <- "GI-7"
Density_GI7<- Estimate_Population_Density("GI-7")
Biomass_GI7<- Estimate_Biomass_Per_Size_Category(Density_GI7)
Biomass_GI7$SI <- "Interstadial"

### Abundance of herbivore species recovered from the archaeo-paleontological assemblages covering
### the period comprised between the GS7 and the GS5 in the Eurosiberian region:

Fauna_BodyMass <- na.omit (Fauna$BM_GS7_GS5)
Species_List <- na.omit (Fauna$Species_GS7_GS5)
SizeCategory <- na.omit(Fauna$SC_GS7_GS5)

Phase_Stadial_Interstadial <- "GS-7"
Density_GS7<- Estimate_Population_Density("GS-7")
Biomass_GS7<- Estimate_Biomass_Per_Size_Category(Density_GS7)
Biomass_GS7$SI <- "Stadial"

Phase_Stadial_Interstadial <- "GI-6"
Density_GI6<- Estimate_Population_Density("GI-6")
Biomass_GI6<- Estimate_Biomass_Per_Size_Category(Density_GI6)
Biomass_GI6$SI <- "Interstadial"

Phase_Stadial_Interstadial <- "GS-6"
Density_GS6<- Estimate_Population_Density("GS-6")
Biomass_GS6<- Estimate_Biomass_Per_Size_Category(Density_GS6)
Biomass_GS6$SI <- "Stadial"

Phase_Stadial_Interstadial <- "GI-5"
Density_GI5<- Estimate_Population_Density("GI-5")
Biomass_GI5<- Estimate_Biomass_Per_Size_Category(Density_GI5)
Biomass_GI5$SI <- "Interstadial"

Phase_Stadial_Interstadial <- "GS-5"
Density_GS5<- Estimate_Population_Density("GS-5")
Biomass_GS5<- Estimate_Biomass_Per_Size_Category(Density_GS5)
Biomass_GS5$SI <- "Stadial"


##################### MERGE THE POPULATION DENSITY ESTIMATIONS OF ALL HERBIVORE SPECIES
#####################             RECOVERED FROM THE EUROSIBERIAN REGION:

DensEurosiberian<- rbind(Density_GI13, Density_GS13, Density_GI12, Density_GS12, Density_GI11, 
                         Density_GS11, Density_GI10, Density_GS10, Density_GI9, Density_GS9,
                         Density_GI8, Density_GS8, Density_GI7, Density_GS7, Density_GI6, Density_GS6,
                         Density_GI5, Density_GS5)
DensEurosiberian$Region <- "Eurosiberian"
DensEurosiberian$MacroRegion <- "EuroSiberian"
head(DensEurosiberian)

##################### MERGE THE ESTIMATED BIOMASS OF HERBIVORES PER SIZE CATEGORY:

BiomassEurosiberian<- rbind(Biomass_GI13, Biomass_GS13, Biomass_GI12, Biomass_GS12, Biomass_GI11, 
                            Biomass_GS11, Biomass_GI10, Biomass_GS10, Biomass_GI9, Biomass_GS9,
                            Biomass_GI8, Biomass_GS8, Biomass_GI7, Biomass_GS7, Biomass_GI6, Biomass_GS6,
                            Biomass_GI5, Biomass_GS5)
BiomassEurosiberian$Region <- "Eurosiberian"
BiomassEurosiberian$MacroRegion <- "EuroSiberian"
head(BiomassEurosiberian)


#_________________________________________________________________________________
#
############################################ IBERIA: Mediterranean macro-region  ############################################
#
#_________________________________________________________________________________

#
############################################ IBERIA: Submediterranean zone  ############################################
#
#_________________________________________________________________________________

NPP.SubMediterranean <- read.xlsx("Dataset-NPP.xlsx", rowNames=FALSE,
                                    colNames=TRUE, sheet="NPP.Submediterranean")



### Mean NPP in all stadial/interstadial periods of the late MIS 3:

Stadials <- subset(NPP.SubMediterranean, Phase=="GS-13"| Phase=="GS-12"| Phase=="GS-11"| Phase=="GS-10"| Phase=="GS-9"
                   | Phase=="GS-8"| Phase=="GS-7"| Phase=="GS-6"| Phase=="GS-5")
Stadials$Stadial_Interstadial="Stadial"

Interstadials <- subset(NPP.SubMediterranean, Phase=="GI-13"| Phase=="GI-12"| Phase=="GI-11"| Phase=="GI-10"| Phase=="GI-9"
                        | Phase=="GI-8"| Phase=="GI-7"| Phase=="GI-6"| Phase=="GI-5")
Interstadials$Stadial_Interstadial="Interstadial"

Dataset <- rbind(Stadials, Interstadials)

Summary_NPP <- aggregate(NPP~Stadial_Interstadial,data=Dataset, function(x) c(mean=mean(x), sd = sd(x), cv=co.var(x)))
Summary_NPP

# Wilcox rank sum test
wilcox.test(NPP ~ Stadial_Interstadial, alternative="two.sided", data=Dataset)


#### NPP in each  stadial-interstadial
Summary_NPP <- aggregate(NPP~Phase,data=NPP.SubMediterranean, function(x) c(mean=mean(x), sd = sd(x)))
Summary_NPP

#Save outputs:
#write.csv(Summary_NPP, "NPP_SubMediterranean.csv")
# Plot the temporal evolution of the NPP in the SubMediterranean region:

NPP.SubMediterranean_Plot <- ggplot(NPP.SubMediterranean, aes(x = Age, y = NPP, ymin= NPP - (2*SD),
                                                                    ymax= NPP + (2 * SD))) +
  geom_line( color="black", size = 1) +
  scale_x_reverse(breaks = seq(30000, 55000, 5000)) +
  geom_ribbon(alpha= 0.45) +
  ggtitle ("SubMediterranean") +
  xlab ("age BP")+
  ylab ("NPP (kg/m^2/yr)") +
  ylim(0, 0.6) +
  theme_classic(base_size = 14, base_family = "serif")+ 
  theme(legend.position = "none")

NPP.SubMediterranean_Plot


#__________________

## HERBIVORE ABUNDANCE PER STAIDAL/INTERSTADIAL:

#__________________

###### Primary consumer species and their body masses (BM)

Fauna <- read.xlsx("Herbivore.Species.xlsx", rowNames=FALSE,
                   colNames=TRUE, sheet="Fauna.SubMediterranean")


### Abundance of herbivore species recovered from the archeo-paleontological assemblages covering
### the period comprised between the GI13 and the GS7 in the Thermomediterranean region:

Fauna_BodyMass <- na.omit (Fauna$BM_GI13_GS7)
Species_List <- na.omit (Fauna$Species_GI13_GS7)
SizeCategory <- na.omit(Fauna$SC_GI13_GS7)

Phase_Stadial_Interstadial <- "GI-13"
Density_GI13<- Estimate_Population_Density("GI-13")
Biomass_GI13<- Estimate_Biomass_Per_Size_Category(Density_GI13)
Biomass_GI13$SI <- "Interstadial"

Phase_Stadial_Interstadial <- "GS-13"
Density_GS13<- Estimate_Population_Density("GS-13")
Biomass_GS13<- Estimate_Biomass_Per_Size_Category(Density_GS13)
Biomass_GS13$SI <- "Stadial"

Phase_Stadial_Interstadial <- "GI-12"
Density_GI12<- Estimate_Population_Density("GI-12")
Biomass_GI12<- Estimate_Biomass_Per_Size_Category(Density_GI12)
Biomass_GI12$SI <- "Interstadial"

Phase_Stadial_Interstadial <- "GS-12"
Density_GS12<- Estimate_Population_Density("GS-12")
Biomass_GS12<- Estimate_Biomass_Per_Size_Category(Density_GS12)
Biomass_GS12$SI <- "Stadial"

Phase_Stadial_Interstadial <- "GI-11"
Density_GI11<- Estimate_Population_Density("GI-11")
Biomass_GI11<- Estimate_Biomass_Per_Size_Category(Density_GI11)
Biomass_GI11$SI <- "Interstadial"

Phase_Stadial_Interstadial <- "GS-11"
Density_GS11<- Estimate_Population_Density("GS-11")
Biomass_GS11<- Estimate_Biomass_Per_Size_Category(Density_GS11)
Biomass_GS11$SI <- "Stadial"

Phase_Stadial_Interstadial <- "GI-10"
Density_GI10<- Estimate_Population_Density("GI-10")
Biomass_GI10<- Estimate_Biomass_Per_Size_Category(Density_GI10)
Biomass_GI10$SI <- "Interstadial"

Phase_Stadial_Interstadial <- "GS-10"
Density_GS10<- Estimate_Population_Density("GS-10")
Biomass_GS10<- Estimate_Biomass_Per_Size_Category(Density_GS10)
Biomass_GS10$SI <- "Stadial"

Phase_Stadial_Interstadial <- "GI-9"
Density_GI9<- Estimate_Population_Density("GI-9")
Biomass_GI9<- Estimate_Biomass_Per_Size_Category(Density_GI9)
Biomass_GI9$SI <- "Interstadial"

Phase_Stadial_Interstadial <- "GS-9"
Density_GS9<- Estimate_Population_Density("GS-9")
Biomass_GS9<- Estimate_Biomass_Per_Size_Category(Density_GS9)
Biomass_GS9$SI <- "Stadial"

Phase_Stadial_Interstadial <- "GI-8"
Density_GI8<- Estimate_Population_Density("GI-8")
Biomass_GI8<- Estimate_Biomass_Per_Size_Category(Density_GI8)
Biomass_GI8$SI <- "Interstadial"

Phase_Stadial_Interstadial <- "GS-8"
Density_GS8<- Estimate_Population_Density("GS-8")
Biomass_GS8<- Estimate_Biomass_Per_Size_Category(Density_GS8)
Biomass_GS8$SI <- "Stadial"

Phase_Stadial_Interstadial <- "GI-7"
Density_GI7<- Estimate_Population_Density("GI-7")
Biomass_GI7<- Estimate_Biomass_Per_Size_Category(Density_GI7)
Biomass_GI7$SI <- "Interstadial"

Phase_Stadial_Interstadial <- "GS-7"
Density_GS7<- Estimate_Population_Density("GS-7")
Biomass_GS7<- Estimate_Biomass_Per_Size_Category(Density_GS7)
Biomass_GS7$SI <- "Stadial"

### Abundance of herbivore species recovered from the archeo-paleontological assemblages covering
### the period comprised between the GI6 and the GS5 in the Thermomediterranean region:

Fauna_BodyMass <- na.omit (Fauna$BM_GI6_GS5)
Species_List <- na.omit (Fauna$Species_GI6_GS5)
SizeCategory <- na.omit(Fauna$SC_GI6_GS5)

Phase_Stadial_Interstadial <- "GI-6"
Density_GI6<- Estimate_Population_Density("GI-6")
Biomass_GI6<- Estimate_Biomass_Per_Size_Category(Density_GI6)
Biomass_GI6$SI <- "Interstadial"

Phase_Stadial_Interstadial <- "GS-6"
Density_GS6<- Estimate_Population_Density("GS-6")
Biomass_GS6<- Estimate_Biomass_Per_Size_Category(Density_GS6)
Biomass_GS6$SI <- "Stadial"

Phase_Stadial_Interstadial <- "GI-5"
Density_GI5<- Estimate_Population_Density("GI-5")
Biomass_GI5<- Estimate_Biomass_Per_Size_Category(Density_GI5)
Biomass_GI5$SI <- "Interstadial"

Phase_Stadial_Interstadial <- "GS-5"
Density_GS5<- Estimate_Population_Density("GS-5")
Biomass_GS5<- Estimate_Biomass_Per_Size_Category(Density_GS5)
Biomass_GS5$SI <- "Stadial"


##################### MERGE THE POPULATION DENSITY OF ALL HERBIVORE SPECIES
#####################    RECOVERED FROM THE SubMediterranean REGION:

DensSubMediterranean<- rbind(Density_GI13, Density_GS13, Density_GI12, Density_GS12, Density_GI11, 
                               Density_GS11, Density_GI10, Density_GS10, Density_GI9, Density_GS9,
                               Density_GI8, Density_GS8, Density_GI7, Density_GS7, Density_GI6, Density_GS6,
                               Density_GI5, Density_GS5)
DensSubMediterranean$Region <- "SubMediterranean"
DensSubMediterranean$MacroRegion <- "Mediterranean"
head(DensSubMediterranean)


##################### MERGE THE ESTIMATED BIOMASS ACCORDING TO EACH SIZE CATEGORY

BiomassSubMediterranean<- rbind(Biomass_GI13, Biomass_GS13, Biomass_GI12, Biomass_GS12, Biomass_GI11, 
                                  Biomass_GS11, Biomass_GI10, Biomass_GS10, Biomass_GI9, Biomass_GS9,
                                  Biomass_GI8, Biomass_GS8, Biomass_GI7, Biomass_GS7, Biomass_GI6, Biomass_GS6,
                                  Biomass_GI5, Biomass_GS5)
BiomassSubMediterranean$Region <- "SubMediterranean"
BiomassSubMediterranean$MacroRegion <- "Mediterranean"
head(BiomassSubMediterranean)



#_________________________________________________________________________________
#
############################################ IBERIA: Mesomediterranean zone  ############################################
#
#_________________________________________________________________________________

NPP.MesoMediterranean <- read.xlsx("Dataset-NPP.xlsx", rowNames=FALSE,
                                   colNames=TRUE, sheet="NPP.Mesomediterranean")



### Mean NPP in all stadial/interstadial periods of the late MIS 3:

Stadials <- subset(NPP.MesoMediterranean, Phase=="GS-13"| Phase=="GS-12"| Phase=="GS-11"| Phase=="GS-10"| Phase=="GS-9"
                   | Phase=="GS-8"| Phase=="GS-7"| Phase=="GS-6"| Phase=="GS-5")
Stadials$Stadial_Interstadial="Stadial"

Interstadials <- subset(NPP.MesoMediterranean, Phase=="GI-13"| Phase=="GI-12"| Phase=="GI-11"| Phase=="GI-10"| Phase=="GI-9"
                        | Phase=="GI-8"| Phase=="GI-7"| Phase=="GI-6"| Phase=="GI-5")
Interstadials$Stadial_Interstadial="Interstadial"

Dataset <- rbind(Stadials, Interstadials)

Summary_NPP <- aggregate(NPP~Stadial_Interstadial,data=Dataset, function(x) c(mean=mean(x), sd = sd(x), cv=co.var(x)))
Summary_NPP

# Wilcox rank sum test
wilcox.test(NPP ~ Stadial_Interstadial, alternative="two.sided", data=Dataset)

#### NPP in each specific stadial-interstadial
Summary_NPP <- aggregate(NPP~Phase,data=NPP.MesoMediterranean, function(x) c(mean=mean(x), sd = sd(x)))
Summary_NPP

#Save outputs:
#write.csv(Summary_NPP, "NPP.MesoMediterranean.csv")
# Plot the temporal evolution of the NPP in the Mesomediterranean region:

NPP.MesoMediterranean_Plot <- ggplot(NPP.MesoMediterranean, aes(x = Age, y = NPP, ymin= NPP - (2*SD),
                                                                    ymax= NPP + (2 * SD))) +
  geom_line( color="black", size = 1) +
  scale_x_reverse(breaks = seq(30000, 55000, 5000)) +
  geom_ribbon(alpha= 0.45) +
  ggtitle ("Mesomediterranean") +
  xlab ("age BP")+
  ylab ("NPP (kg/m^2/yr)") +
  ylim(0, 0.6) +
  theme_classic(base_size = 14, base_family = "serif")+ 
  theme(legend.position = "none")

NPP.MesoMediterranean_Plot



#__________________

## HERBIVORE ABUNDANCE PER STAIDAL/INTERSTADIAL:

#__________________

###### Primary consumer species and their body masses (BM)

Fauna <- read.xlsx("Herbivore.Species.xlsx", rowNames=FALSE,
                   colNames=TRUE, sheet="Fauna.MesoMediterranean")


### Abundance of herbivore species recovered from the archeo-paleontological assemblages covering
### the period comprised between the GI13 and the GS9 in the Mesomediterranean region:

Fauna_BodyMass <- na.omit (Fauna$BM_GI13_GS9)
Species_List <- na.omit (Fauna$Species_GI13_GS9)
SizeCategory <- na.omit(Fauna$SC_GI13_GS9)

Phase_Stadial_Interstadial <- "GI-13"
Density_GI13<- Estimate_Population_Density("GI-13")
Biomass_GI13<- Estimate_Biomass_Per_Size_Category(Density_GI13)
Biomass_GI13$SI <- "Interstadial"

Phase_Stadial_Interstadial <- "GS-13"
Density_GS13<- Estimate_Population_Density("GS-13")
Biomass_GS13<- Estimate_Biomass_Per_Size_Category(Density_GS13)
Biomass_GS13$SI <- "Stadial"

Phase_Stadial_Interstadial <- "GI-12"
Density_GI12<- Estimate_Population_Density("GI-12")
Biomass_GI12<- Estimate_Biomass_Per_Size_Category(Density_GI12)
Biomass_GI12$SI <- "Interstadial"

Phase_Stadial_Interstadial <- "GS-12"
Density_GS12<- Estimate_Population_Density("GS-12")
Biomass_GS12<- Estimate_Biomass_Per_Size_Category(Density_GS12)
Biomass_GS12$SI <- "Stadial"

Phase_Stadial_Interstadial <- "GI-11"
Density_GI11<- Estimate_Population_Density("GI-11")
Biomass_GI11<- Estimate_Biomass_Per_Size_Category(Density_GI11)
Biomass_GI11$SI <- "Interstadial"

Phase_Stadial_Interstadial <- "GS-11"
Density_GS11<- Estimate_Population_Density("GS-11")
Biomass_GS11<- Estimate_Biomass_Per_Size_Category(Density_GS11)
Biomass_GS11$SI <- "Stadial"

Phase_Stadial_Interstadial <- "GI-10"
Density_GI10<- Estimate_Population_Density("GI-10")
Biomass_GI10<- Estimate_Biomass_Per_Size_Category(Density_GI10)
Biomass_GI10$SI <- "Interstadial"

Phase_Stadial_Interstadial <- "GS-10"
Density_GS10<- Estimate_Population_Density("GS-10")
Biomass_GS10<- Estimate_Biomass_Per_Size_Category(Density_GS10)
Biomass_GS10$SI <- "Stadial"

Phase_Stadial_Interstadial <- "GI-9"
Density_GI9<- Estimate_Population_Density("GI-9")
Biomass_GI9<- Estimate_Biomass_Per_Size_Category(Density_GI9)
Biomass_GI9$SI <- "Interstadial"

Phase_Stadial_Interstadial <- "GS-9"
Density_GS9<- Estimate_Population_Density("GS-9")
Biomass_GS9<- Estimate_Biomass_Per_Size_Category(Density_GS9)
Biomass_GS9$SI <- "Stadial"

### Abundance of herbivore species recovered from the archeo-paleontological assemblages covering
### the period comprised between the GI8 and the GI7 in the Mesomediterranean region:

Fauna_BodyMass <- na.omit (Fauna$BM_GI8_GI7)
Species_List <- na.omit (Fauna$Species_GI8_GI7)
SizeCategory <- na.omit(Fauna$SC_GI8_GI7)

Phase_Stadial_Interstadial <- "GI-8"
Density_GI8<- Estimate_Population_Density("GI-8")
Biomass_GI8<- Estimate_Biomass_Per_Size_Category(Density_GI8)
Biomass_GI8$SI <- "Interstadial"

Phase_Stadial_Interstadial <- "GS-8"
Density_GS8<- Estimate_Population_Density("GS-8")
Biomass_GS8<- Estimate_Biomass_Per_Size_Category(Density_GS8)
Biomass_GS8$SI <- "Stadial"

Phase_Stadial_Interstadial <- "GI-7"
Density_GI7<- Estimate_Population_Density("GI-7")
Biomass_GI7<- Estimate_Biomass_Per_Size_Category(Density_GI7)
Biomass_GI7$SI <- "Interstadial"

### Abundance of herbivore species recovered from the archeo-paleontological assemblages covering
### the period comprised between the GS7 and the GS5 in the Mesomediterranean region:

Fauna_BodyMass <- na.omit (Fauna$BM_GS7_GS5)
Species_List <- na.omit (Fauna$Species_GS7_GS5)
SizeCategory <- na.omit(Fauna$SC_GS7_GS5)

Phase_Stadial_Interstadial <- "GS-7"
Density_GS7<- Estimate_Population_Density("GS-7")
Biomass_GS7<- Estimate_Biomass_Per_Size_Category(Density_GS7)
Biomass_GS7$SI <- "Stadial"

Phase_Stadial_Interstadial <- "GI-6"
Density_GI6<- Estimate_Population_Density("GI-6")
Biomass_GI6<- Estimate_Biomass_Per_Size_Category(Density_GI6)
Biomass_GI6$SI <- "Interstadial"

Phase_Stadial_Interstadial <- "GS-6"
Density_GS6<- Estimate_Population_Density("GS-6")
Biomass_GS6<- Estimate_Biomass_Per_Size_Category(Density_GS6)
Biomass_GS6$SI <- "Stadial"

Phase_Stadial_Interstadial <- "GI-5"
Density_GI5<- Estimate_Population_Density("GI-5")
Biomass_GI5<- Estimate_Biomass_Per_Size_Category(Density_GI5)
Biomass_GI5$SI <- "Interstadial"

Phase_Stadial_Interstadial <- "GS-5"
Density_GS5<- Estimate_Population_Density("GS-5")
Biomass_GS5<- Estimate_Biomass_Per_Size_Category(Density_GS5)
Biomass_GS5$SI <- "Stadial"


##################### MERGE THE ESTIMATED POPULATION DENSITY OF ALL HERBIVORE SPECIES
#####################          RECOVERED FROM THE MESOMEDITERRANEAN REGION:

DensMesoMediterranean<- rbind(Density_GI13, Density_GS13, Density_GI12, Density_GS12, Density_GI11, 
                              Density_GS11, Density_GI10, Density_GS10, Density_GI9, Density_GS9,
                              Density_GI8, Density_GS8, Density_GI7, Density_GS7, Density_GI6, Density_GS6,
                              Density_GI5, Density_GS5)
DensMesoMediterranean$Region <- "MesoMediterranean"
DensMesoMediterranean$MacroRegion <- "Mediterranean"
head(DensMesoMediterranean)


##################### MERGE THE ESTIMATED BIOMASS ACCORDING TO EACH SIZE CATEGORY

BiomassMesoMediterranean<- rbind(Biomass_GI13, Biomass_GS13, Biomass_GI12, Biomass_GS12, Biomass_GI11, 
                                 Biomass_GS11, Biomass_GI10, Biomass_GS10, Biomass_GI9, Biomass_GS9,
                                 Biomass_GI8, Biomass_GS8, Biomass_GI7, Biomass_GS7, Biomass_GI6, Biomass_GS6,
                                 Biomass_GI5, Biomass_GS5)
BiomassMesoMediterranean$Region <- "MesoMediterranean"
BiomassMesoMediterranean$MacroRegion <- "Mediterranean"
head(BiomassMesoMediterranean)


#_________________________________________________________________________________
#
########################################## Thermomediterranean zone - COASTAL AREAS
#
#_____________________________________________________________________________

NPP.ThermoMediterranean <- read.xlsx("Dataset-NPP.xlsx", rowNames=FALSE,
                                     colNames=TRUE, sheet="NPP.Thermomediterranean")


### Mean NPP in all stadial/interstadial periods of the late MIS 3:

Stadials <- subset(NPP.ThermoMediterranean, Phase=="GS-13"| Phase=="GS-12"| Phase=="GS-11"| Phase=="GS-10"| Phase=="GS-9"
                   | Phase=="GS-8"| Phase=="GS-7"| Phase=="GS-6"| Phase=="GS-5")
Stadials$Stadial_Interstadial="Stadial"

Interstadials <- subset(NPP.ThermoMediterranean, Phase=="GI-13"| Phase=="GI-12"| Phase=="GI-11"| Phase=="GI-10"| Phase=="GI-9"
                        | Phase=="GI-8"| Phase=="GI-7"| Phase=="GI-6"| Phase=="GI-5")
Interstadials$Stadial_Interstadial="Interstadial"

Dataset <- rbind(Stadials, Interstadials)

Summary_NPP <- aggregate(NPP~Stadial_Interstadial,data=Dataset, function(x) c(mean=mean(x), sd = sd(x), cv=co.var(x)))
Summary_NPP


#### NPP in each specific stadial-interstadial
Summary_NPP <- aggregate(NPP~Phase,data=NPP.ThermoMediterranean, function(x) c(mean=mean(x), sd = sd(x)))
Summary_NPP

#Save outputs
#write.csv(Summary_NPP, "NPP_Thermomediterranean.csv")

# Wilcox rank sum test
wilcox.test(NPP ~ Stadial_Interstadial, alternative="two.sided", data=Dataset)



## Plot temporal evolution of the NPP in the Thermomediterranean region:

NPP.ThermoMediterranean_Plot <- ggplot(NPP.ThermoMediterranean, aes(x = Age, y = NPP, ymin= NPP - (2*SD),
                                                                    ymax= NPP + (2 * SD))) +
  geom_line( color="black", size = 1) +
  scale_x_reverse(breaks = seq(30000, 55000, 5000)) +
  geom_ribbon(alpha= 0.45) +
  ggtitle ("Thermomediterranean") +
  xlab ("age BP")+
  ylab ("NPP (kg/m^2/yr)") +
  ylim(0, 0.6) +
  theme_classic(base_size = 14, base_family = "serif")+ 
  theme(legend.position = "none")

NPP.ThermoMediterranean_Plot




#__________________

## HERBIVORE ABUNDANCE PER STAIDAL/INTERSTADIAL:

#__________________



Fauna <- read.xlsx("Herbivore.Species.xlsx", rowNames=FALSE,
                   colNames=TRUE, sheet="Fauna.ThermoMediterranean")


### Abundance of herbivore species recovered from the archeo-paleontological assemblages covering
### the period comprised between the GI13 and the GS5 in the Thermomediterranean region:

Fauna_BodyMass <- na.omit (Fauna$BM_GI13_GS5)
Species_List <- na.omit (Fauna$Species_GI13_GS5)
SizeCategory <- na.omit(Fauna$SC_GI13_GS5)

Phase_Stadial_Interstadial <- "GI-13"
Density_GI13<- Estimate_Population_Density("GI-13")
Biomass_GI13<- Estimate_Biomass_Per_Size_Category(Density_GI13)
Biomass_GI13$SI <- "Interstadial"

Phase_Stadial_Interstadial <- "GS-13"
Density_GS13<- Estimate_Population_Density("GS-13")
Biomass_GS13<- Estimate_Biomass_Per_Size_Category(Density_GS13)
Biomass_GS13$SI <- "Stadial"

Phase_Stadial_Interstadial <- "GI-12"
Density_GI12<- Estimate_Population_Density("GI-12")
Biomass_GI12<- Estimate_Biomass_Per_Size_Category(Density_GI12)
Biomass_GI12$SI <- "Interstadial"

Phase_Stadial_Interstadial <- "GS-12"
Density_GS12<- Estimate_Population_Density("GS-12")
Biomass_GS12<- Estimate_Biomass_Per_Size_Category(Density_GS12)
Biomass_GS12$SI <- "Stadial"

Phase_Stadial_Interstadial <- "GI-11"
Density_GI11<- Estimate_Population_Density("GI-11")
Biomass_GI11<- Estimate_Biomass_Per_Size_Category(Density_GI11)
Biomass_GI11$SI <- "Interstadial"

Phase_Stadial_Interstadial <- "GS-11"
Density_GS11<- Estimate_Population_Density("GS-11")
Biomass_GS11<- Estimate_Biomass_Per_Size_Category(Density_GS11)
Biomass_GS11$SI <- "Stadial"

Phase_Stadial_Interstadial <- "GI-10"
Density_GI10<- Estimate_Population_Density("GI-10")
Biomass_GI10<- Estimate_Biomass_Per_Size_Category(Density_GI10)
Biomass_GI10$SI <- "Interstadial"

Phase_Stadial_Interstadial <- "GS-10"
Density_GS10<- Estimate_Population_Density("GS-10")
Biomass_GS10<- Estimate_Biomass_Per_Size_Category(Density_GS10)
Biomass_GS10$SI <- "Stadial"

Phase_Stadial_Interstadial <- "GI-9"
Density_GI9<- Estimate_Population_Density("GI-9")
Biomass_GI9<- Estimate_Biomass_Per_Size_Category(Density_GI9)
Biomass_GI9$SI <- "Interstadial"

Phase_Stadial_Interstadial <- "GS-9"
Density_GS9<- Estimate_Population_Density("GS-9")
Biomass_GS9<- Estimate_Biomass_Per_Size_Category(Density_GS9)
Biomass_GS9$SI <- "Stadial"

Phase_Stadial_Interstadial <- "GI-8"
Density_GI8<- Estimate_Population_Density("GI-8")
Biomass_GI8<- Estimate_Biomass_Per_Size_Category(Density_GI8)
Biomass_GI8$SI <- "Interstadial"

Phase_Stadial_Interstadial <- "GS-8"
Density_GS8<- Estimate_Population_Density("GS-8")
Biomass_GS8<- Estimate_Biomass_Per_Size_Category(Density_GS8)
Biomass_GS8$SI <- "Stadial"

Phase_Stadial_Interstadial <- "GI-7"
Density_GI7<- Estimate_Population_Density("GI-7")
Biomass_GI7<- Estimate_Biomass_Per_Size_Category(Density_GI7)
Biomass_GI7$SI <- "Interstadial"

Phase_Stadial_Interstadial <- "GS-7"
Density_GS7<- Estimate_Population_Density("GS-7")
Biomass_GS7<- Estimate_Biomass_Per_Size_Category(Density_GS7)
Biomass_GS7$SI <- "Stadial"

Phase_Stadial_Interstadial <- "GI-6"
Density_GI6<- Estimate_Population_Density("GI-6")
Biomass_GI6<- Estimate_Biomass_Per_Size_Category(Density_GI6)
Biomass_GI6$SI <- "Interstadial"

Phase_Stadial_Interstadial <- "GS-6"
Density_GS6<- Estimate_Population_Density("GS-6")
Biomass_GS6<- Estimate_Biomass_Per_Size_Category(Density_GS6)
Biomass_GS6$SI <- "Stadial"

Phase_Stadial_Interstadial <- "GI-5"
Density_GI5<- Estimate_Population_Density("GI-5")
Biomass_GI5<- Estimate_Biomass_Per_Size_Category(Density_GI5)
Biomass_GI5$SI <- "Interstadial"

Phase_Stadial_Interstadial <- "GS-5"
Density_GS5<- Estimate_Population_Density("GS-5")
Biomass_GS5<- Estimate_Biomass_Per_Size_Category(Density_GS5)
Biomass_GS5$SI <- "Stadial"


##################### MERGE THE POPULATION DENSITY OF ALL HERBIVORE SPECIES
#####################   RECOVERED FROM THE THERMOMEDITERRANEAN REGION:

DensThermoMediterranean<- rbind(Density_GI13, Density_GS13, Density_GI12, Density_GS12, Density_GI11, 
                                Density_GS11, Density_GI10, Density_GS10, Density_GI9, Density_GS9,
                                Density_GI8, Density_GS8, Density_GI7, Density_GS7, Density_GI6, Density_GS6,
                                Density_GI5, Density_GS5)
DensThermoMediterranean$Region <- "ThermoMediterranean"
DensThermoMediterranean$MacroRegion <- "Mediterranean"
head(DensThermoMediterranean)


##################### MERGE THE HERBIVORE BIOMASS ACCORDING TO EACH BODY SIZE CATEGORY

BiomassThermoMediterranean<- rbind(Biomass_GI13, Biomass_GS13, Biomass_GI12, Biomass_GS12, Biomass_GI11, 
                                   Biomass_GS11, Biomass_GI10, Biomass_GS10, Biomass_GI9, Biomass_GS9,
                                   Biomass_GI8, Biomass_GS8, Biomass_GI7, Biomass_GS7, Biomass_GI6, Biomass_GS6,
                                   Biomass_GI5, Biomass_GS5)
BiomassThermoMediterranean$Region <- "ThermoMediterranean"
BiomassThermoMediterranean$MacroRegion <- "Mediterranean"
head(BiomassThermoMediterranean)





#___________________________________________________________________________________________________
#
####### SAVE OUTPUTS
#
#___________________________________________________________________________________________________

##_____________________________________
#
### HERBIVORE POPULATION DENSITIES AND BIOMASS OF ALL BIOGEOGRAPHIC REGIONS OF THE IBERIAN PENINSULA
#
##________________________________________

Herbivore_Population_Densities_IberianPeninsula <- rbind(DensEurosiberian,DensSubMediterranean, DensMesoMediterranean, DensThermoMediterranean)
  
write.xlsx(Herbivore_Population_Densities_IberianPeninsula, "Herbivore_Population_Densities_IberianPeninsula.xlsx")
 

 
Herbivore_Biomass_IberianPeninsula <- rbind(BiomassEurosiberian, BiomassThermoMediterranean, BiomassMesoMediterranean, BiomassSubMediterranean)
 
 write.xlsx(Herbivore_Biomass_IberianPeninsula, "Herbivore_Biomass_IberianPeninsula.xlsx")
  


#_________________________________________________________________________________________________________
#
#
#
####### RESULTS
#
#
#
#_________________________________________________________________________________________________________

 
 
###### COMPARISON OF HERBIVORE BIOMASS PER REGIONS DURING THE GREENLAND INTERSTADIAL PERIODS 
  
###   Mann-Whitney-Wilcoxon test, with a post-hoc Bonferroni correction:
  
  
Biomass_Interstadials <- subset(Herbivore_Biomass_IberianPeninsula, SI=="Interstadial")
 
  
Small <- Biomass_Interstadials  %>% wilcox_test (Biomass_SmallSize_Herbivores_Mean ~ Region, paired = TRUE)  %>%
  adjust_pvalue(method = "bonferroni") 

Medium <- Biomass_Interstadials  %>% wilcox_test (Biomass_MediumSize_Herbivores_Mean ~ Region, paired = TRUE)%>%
  adjust_pvalue(method = "bonferroni") 

MediumLarge <- Biomass_Interstadials  %>% wilcox_test (Biomass_MediumLarge_Herbivores_Mean ~ Region, paired = TRUE)%>%
  adjust_pvalue(method = "bonferroni") 

Large <- Biomass_Interstadials  %>% wilcox_test (Biomass_Large_Herbivores_Mean ~ Region, paired = TRUE)%>%
  adjust_pvalue(method = "bonferroni") 

Comparison_Biomass_Interstadials <- rbind(Small, Medium, MediumLarge, Large)
Comparison_Biomass_Interstadials


# save outputs: 
#write.xlsx(Comparison_Biomass_Interstadials, "Comparison_Biomass_Interstadials.xlsx")


############ COMPARISON OF HERBIVORE BIOMASS BETWEEN REGIONS DURING THE COLD MOMENTS (STADIALS):
##   Mann-Whitney-Wilcoxon test


head(Herbivore_Biomass_IberianPeninsula)

Biomass_Stadials <- subset(Herbivore_Biomass_IberianPeninsula, SI=="Stadial")


Small <- Biomass_Stadials  %>% wilcox_test (Biomass_SmallSize_Herbivores_Mean ~ Region, paired = TRUE)%>%
  adjust_pvalue(method = "bonferroni") 

Medium <- Biomass_Stadials  %>% wilcox_test (Biomass_MediumSize_Herbivores_Mean ~ Region, paired = TRUE)%>%
  adjust_pvalue(method = "bonferroni") 

MediumLarge <- Biomass_Stadials  %>% wilcox_test (Biomass_MediumLarge_Herbivores_Mean ~ Region, paired = TRUE)%>%
  adjust_pvalue(method = "bonferroni") 

Large <- Biomass_Stadials  %>% wilcox_test (Biomass_Large_Herbivores_Mean ~ Region, paired = TRUE)%>%
  adjust_pvalue(method = "bonferroni") 


Comparison_Biomass_Stadials <- rbind(Small, Medium, MediumLarge, Large)


# save outputs: 
# write.xlsx(Comparison_Biomass_Stadials, "Comparison_Biomass_Stadials.xlsx")



#### BOXPLOT OF BIOMASS PER SIZE CATEGORY AND STADIAL/INTERSTADIAL

head(Herbivore_Biomass_IberianPeninsula)


### Small herbivores (Body weight < 20 kg)
Biomass_Small_Size <- ggplot(Herbivore_Biomass_IberianPeninsula, aes(x = factor(Region), y = Biomass_SmallSize_Herbivores_Mean, fill = SI)) + 
  stat_boxplot(geom = "errorbar", position = position_dodge(width = 0.9), width = 0.5) + 
  geom_boxplot(position = position_dodge(width = 0.9)) + 
  scale_fill_brewer(palette = "BrBG") + 
  xlab("") + 
  ylab("Biomass of small herbivores") + 
  labs(fill = "") + 
  theme_classic(base_size = 12, base_family = "sans")

Biomass_Small_Size<- Biomass_Small_Size + scale_x_discrete(limits=c("Eurosiberian", "SubMediterranean", "MesoMediterranean", "ThermoMediterranean"))
Biomass_Small_Size



### Medium herbivores (20< Body weight > 100)

Biomass_Medium_Size <- ggplot(Herbivore_Biomass_IberianPeninsula, aes(x = factor(Region), y = Biomass_MediumSize_Herbivores_Mean, fill = SI)) + 
  stat_boxplot(geom = "errorbar", position = position_dodge(width = 0.9), width = 0.5) + 
  geom_boxplot(position = position_dodge(width = 0.9)) +
  scale_fill_brewer(palette = "BrBG") + 
  xlab("") + 
  ylab("Biomass of medium herbivores") + 
  labs(fill = "") + 
  theme_classic(base_size = 12, base_family = "sans")

Biomass_Medium_Size<-Biomass_Medium_Size+ scale_x_discrete(limits=c("Eurosiberian", "SubMediterranean", "MesoMediterranean", "ThermoMediterranean"))
Biomass_Medium_Size
### Medium-Large herbivores (100 < Body weigth > 300)

Biomass_MediumLarge_Size <- ggplot(Herbivore_Biomass_IberianPeninsula, aes(x = factor(Region), y = Biomass_MediumLarge_Herbivores_Mean, fill = SI)) + 
  stat_boxplot(geom = "errorbar", position = position_dodge(width = 0.9), width = 0.5) + 
  geom_boxplot(position = position_dodge(width = 0.9)) + 
  scale_fill_brewer(palette = "BrBG") + 
  xlab("") + 
  ylab("Biomass of medium-large herbivores") + 
  labs(fill = "") + 
  theme_classic(base_size = 12, base_family = "sans")

Biomass_MediumLarge_Size<-Biomass_MediumLarge_Size + scale_x_discrete(limits=c("Eurosiberian", "SubMediterranean", "MesoMediterranean", "ThermoMediterranean"))
Biomass_MediumLarge_Size


### Large herbivores (Body weight > 300)
Biomass_Large_Size <- ggplot(Herbivore_Biomass_IberianPeninsula, aes(x = factor(Region), y = Biomass_Large_Herbivores_Mean, fill = SI)) + 
  stat_boxplot(geom = "errorbar", position = position_dodge(width = 0.9), width = 0.5) + 
  geom_boxplot(position = position_dodge(width = 0.9)) + 
  scale_fill_brewer(palette = "BrBG") + 
  xlab("") + 
  ylab("Biomass of large herbivores") + 
  labs(fill = "") + 
  theme_classic(base_size = 12, base_family = "sans")

Biomass_Large_Size<-Biomass_Large_Size+ scale_x_discrete(limits=c("Eurosiberian",  "SubMediterranean", "MesoMediterranean", "ThermoMediterranean"))
Biomass_Large_Size



grid.arrange(Biomass_Large_Size, Biomass_MediumLarge_Size, Biomass_Medium_Size, Biomass_Small_Size, nrow = 4)



 #### BARPLOT OF THE ESTIMATED HERBIVORE BIOMASS IN EACH SPECIFIC STADIAL/INTERSTADIAL MOMENT

head(Herbivore_Biomass_IberianPeninsula)

#Create a new dataframe:

Small <- data.frame(Biomass = c(Herbivore_Biomass_IberianPeninsula[,"Biomass_SmallSize_Herbivores_Mean"]),
                    Phase = (Herbivore_Biomass_IberianPeninsula[,"Phase"]), 
                    Region = (Herbivore_Biomass_IberianPeninsula[,"Region"]),
                    Size= "Small")
Medium <- data.frame(Biomass = c(Herbivore_Biomass_IberianPeninsula[,"Biomass_MediumSize_Herbivores_Mean"]),
                    Phase = (Herbivore_Biomass_IberianPeninsula[,"Phase"]), 
                    Region = (Herbivore_Biomass_IberianPeninsula[,"Region"]),
                    Size= "Medium")
MediumLarge <- data.frame(Biomass = c(Herbivore_Biomass_IberianPeninsula[,"Biomass_MediumLarge_Herbivores_Mean"]),
                     Phase = (Herbivore_Biomass_IberianPeninsula[,"Phase"]), 
                     Region = (Herbivore_Biomass_IberianPeninsula[,"Region"]),
                     Size= "Medium-Large")
Large <- data.frame(Biomass = c(Herbivore_Biomass_IberianPeninsula[,"Biomass_Large_Herbivores_Mean"]),
                          Phase = (Herbivore_Biomass_IberianPeninsula[,"Phase"]), 
                          Region = (Herbivore_Biomass_IberianPeninsula[,"Region"]),
                          Size= "Large")  

Dataset_Biomass <- rbind(Small, Medium, MediumLarge, Large)

### Eurosiberian

Eurosiberian <- subset(Dataset_Biomass, Region =="Eurosiberian")

Eurosiberian_Boimass <- ggplot() +
  labs(title="Eurosiberian",
       x ="Stadial/Interstadial", y = "Biomass (kg/km^2)") +
  geom_bar(data=Eurosiberian, aes(x = Phase, y = Biomass, fill=factor(Size, levels=c("Large", "Medium-Large", "Medium", "Small"))) , stat ="identity", position="stack") +
  scale_x_discrete(limits=c("GI-13", "GS-13", "GI-12", "GS-12",
                            "GI-11", "GS-11", "GI-10", "GS-10", "GI-9", "GS-9", "GI-8", "GS-8", "GI-7", "GS-7", "GI-6", "GS-6", "GI-5", "GS-5")) + 
  scale_y_continuous(sec.axis = sec_axis(trans = ~ . / 1.01), limits = c(0, 950)) +
  theme_classic(base_size = 12, base_family = "sans")+
  scale_fill_brewer(name="Size", breaks = c("Large", "Medium-Large", "Medium", "Small"), palette = "BrBG")

  

Eurosiberian_Boimass


## Mediterranean: SubMediterranean

SubMediterranean <- subset(Dataset_Biomass, Region =="SubMediterranean")
head(SubMediterranean)

SubMediterranean_Boimass <- ggplot() +
  labs(title="SubMediterranean",
       x ="Stadial/Interstadial", y = "Biomass (kg/km^2)") +
  geom_bar(data=SubMediterranean, aes(x = Phase, y = Biomass, fill=factor(Size, levels=c("Large", "Medium-Large", "Medium", "Small"))) , stat ="identity", position="stack") +
  scale_x_discrete(limits=c("GI-13", "GS-13", "GI-12", "GS-12",
                            "GI-11", "GS-11", "GI-10", "GS-10", "GI-9", "GS-9", "GI-8", "GS-8", "GI-7", "GS-7", "GI-6", "GS-6", "GI-5", "GS-5")) + 
  scale_y_continuous(sec.axis = sec_axis(trans = ~ . / 1.01), limits = c(0, 950)) +
  theme_classic(base_size = 12, base_family = "sans")+
  scale_fill_brewer(name="Size", breaks = c("Large", "Medium-Large", "Medium", "Small"), palette = "BrBG")

SubMediterranean_Boimass

## Mediterranean: Mesomediterranean

Mesomediterranean <- subset(Dataset_Biomass, Region =="MesoMediterranean")
head(Mesomediterranean)

Mesomediterranean_Boimass <- ggplot() +
  labs(title="Mesomediterranean",
       x ="Stadial/Interstadial", y = "Biomass (kg/km^2)") +
  geom_bar(data=Mesomediterranean, aes(x = Phase, y = Biomass, fill=factor(Size, levels=c("Large", "Medium-Large", "Medium", "Small"))) , stat ="identity", position="stack") +
  scale_x_discrete(limits=c("GI-13", "GS-13", "GI-12", "GS-12",
                            "GI-11", "GS-11", "GI-10", "GS-10", "GI-9", "GS-9", "GI-8", "GS-8", "GI-7", "GS-7", "GI-6", "GS-6", "GI-5", "GS-5")) + 
  scale_y_continuous(sec.axis = sec_axis(trans = ~ . / 1.01), limits = c(0, 950)) +
  theme_classic(base_size = 12, base_family = "sans")+
  scale_fill_brewer(name="Size",breaks = c("Large", "Medium-Large", "Medium", "Small"), palette = "BrBG")

Mesomediterranean_Boimass

## Mediterranean: Thermomediterranean

Thermomediterranean <- subset(Dataset_Biomass, Region =="ThermoMediterranean")
head(Thermomediterranean)

Thermomediterranean_Boimass <- ggplot() +
  labs(title="Thermomediterranean",
       x ="Stadial/Interstadial", y = "Biomass (kg/km^2)") +
  geom_bar(data=Thermomediterranean, aes(x = Phase, y = Biomass, fill=factor(Size, levels=c("Large", "Medium-Large", "Medium", "Small"))) , stat ="identity", position="stack") +
  scale_x_discrete(limits=c("GI-13", "GS-13", "GI-12", "GS-12",
                            "GI-11", "GS-11", "GI-10", "GS-10", "GI-9", "GS-9", "GI-8", "GS-8", "GI-7", "GS-7", "GI-6", "GS-6", "GI-5", "GS-5")) + 
  scale_y_continuous(sec.axis = sec_axis(trans = ~ . / 1.01), limits = c(0, 950)) +
  theme_classic(base_size = 12, base_family = "sans")+
  scale_fill_brewer(name="Size",breaks = c("Large", "Medium-Large", "Medium", "Small"), palette = "BrBG")

Thermomediterranean_Boimass

grid.arrange(Eurosiberian_Boimass, SubMediterranean_Boimass,
             Mesomediterranean_Boimass, Thermomediterranean_Boimass, nrow = 4)




###__________________________________________________________
#
### ADDITIONAL PLOT (NOT INCLUDED IN THE MANUSCRIPT):
#
###__________________________________________________________


##### PLOT OF THE ESTIMATED HERBIVORE BIOMASS (WITH THE 95% CI) IN EACH BIOGEOGRAPHIC REGION PER STADIAL/INTERSTADIAL 
### ACCORDING THE BODY SIZE CATEGORY:


### SMALL HERBIVORES

Biomass_Small_Size<- ggplot(Herbivore_Biomass_IberianPeninsula, 
                            aes(x = factor(Phase), y = Biomass_SmallSize_Herbivores_Mean, 
                                color = factor(Region, levels=c("Eurosiberian","SubMediterranean", "MesoMediterranean", 
                                                                "ThermoMediterranean"))))+
  geom_point(position=position_dodge(0.75)) + 
  geom_errorbar(aes(ymin=Biomass_SmallSize_Herbivores_Min, ymax=Biomass_SmallSize_Herbivores_Max), width=.2,
                position=position_dodge(0.75)) +
  xlab("Stadial/Interstadial") + 
  scale_x_discrete(limits=c("GI-13", "GS-13", "GI-12", "GS-12",
                            "GI-11", "GS-11", "GI-10", "GS-10", "GI-9", "GS-9", "GI-8", "GS-8", "GI-7", "GS-7", "GI-6", "GS-6", "GI-5", "GS-5")) +
  ylab("Biomass of small herbivores") + 
  labs(colour= "Region")+ 
  theme_classic(base_size = 12, base_family = "sans")

Biomass_Small_Size



### MEDIUM HERBIVORES

Biomass_Medium_Size<-  ggplot(Herbivore_Biomass_IberianPeninsula, aes(x = factor(Phase), y = Biomass_MediumSize_Herbivores_Mean, 
                                                                      color = factor(Region, levels=c("Eurosiberian","SubMediterranean", "MesoMediterranean", 
                                                                                                      "ThermoMediterranean"))))+
  geom_point(position=position_dodge(0.75)) + 
  geom_errorbar(aes(ymin=Biomass_MediumSize_Herbivores_Min, ymax=Biomass_MediumSize_Herbivores_Max), width=.2,
                position=position_dodge(0.75)) +
  xlab("Stadial/Interstadial") + 
  scale_x_discrete(limits=c("GI-13", "GS-13", "GI-12", "GS-12",
                            "GI-11", "GS-11", "GI-10", "GS-10", "GI-9", "GS-9", "GI-8", "GS-8", "GI-7", "GS-7", "GI-6", "GS-6", "GI-5", "GS-5")) +
  ylab("Biomass of small herbivores") + 
  labs(color = "Region") + 
  theme_classic(base_size = 12, base_family = "sans")



Biomass_Medium_Size


### MEDIUM-LARGE HERBIVORES

Biomass_MediumLarge_Size<-  ggplot(Herbivore_Biomass_IberianPeninsula, aes(x = factor(Phase), y = Biomass_MediumLarge_Herbivores_Mean, 
                                                                           color = factor(Region, levels=c("Eurosiberian","SubMediterranean", "MesoMediterranean", 
                                                                                                           "ThermoMediterranean"))))+
  geom_point(position=position_dodge(0.75)) + 
  geom_errorbar(aes(ymin=Biomass_MediumLarge_Herbivores_Min, ymax=Biomass_MediumLarge_Herbivores_Max), width=.2,
                position=position_dodge(0.75)) +
  xlab("Stadial/Interstadial") + 
  scale_x_discrete(limits=c("GI-13", "GS-13", "GI-12", "GS-12",
                            "GI-11", "GS-11", "GI-10", "GS-10", "GI-9", "GS-9", "GI-8", "GS-8", "GI-7", "GS-7", "GI-6", "GS-6", "GI-5", "GS-5")) +
  ylab("Biomass of small herbivores") + 
  labs(color = "Region") + 
  theme_classic(base_size = 12, base_family = "sans")


Biomass_MediumLarge_Size

### LARGE HERBIVORES

Biomass_Large_Size<-  ggplot(Herbivore_Biomass_IberianPeninsula, aes(x = factor(Phase), y = Biomass_Large_Herbivores_Mean, 
                                                                     color = factor(Region, levels=c("Eurosiberian","SubMediterranean", "MesoMediterranean", 
                                                                                                     "ThermoMediterranean"))))+
  geom_point(position=position_dodge(0.75)) + 
  geom_errorbar(aes(ymin=Biomass_Large_Herbivores_Min, ymax=Biomass_Large_Herbivores_Max), width=.2,
                position=position_dodge(0.75)) +
  xlab("Stadial/Interstadial") + 
  scale_x_discrete(limits=c("GI-13", "GS-13", "GI-12", "GS-12",
                            "GI-11", "GS-11", "GI-10", "GS-10", "GI-9", "GS-9", "GI-8", "GS-8", "GI-7", "GS-7", "GI-6", "GS-6", "GI-5", "GS-5")) +
  ylab("Biomass of small herbivores") + 
  labs(color = "Region") + 
  theme_classic(base_size = 12, base_family = "sans")

Biomass_Large_Size


grid.arrange (Biomass_Large_Size, Biomass_MediumLarge_Size, Biomass_Medium_Size, Biomass_Small_Size, ncol=1)






