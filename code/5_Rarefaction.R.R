# Clear all
rm(list = ls()) 

## Load libraries

library(openxlsx)
library(ggplot2)
library(iNEXT)

################## The script below was written within the Rstudio framework.
### Choose your own directory:


setwd("C:/...")



#### RAREFACTION TEST


EuroSiberian <- read.xlsx("Herbivore.Species.xlsx", rowNames=FALSE,
                           colNames=TRUE, sheet="Eurosiberian")
SubMediterranean <- read.xlsx("Herbivore.Species.xlsx", rowNames=FALSE,
                                colNames=TRUE, sheet="Submediterranean")
MesoMediterranean <- read.xlsx("Herbivore.Species.xlsx", rowNames=FALSE,
                               colNames=TRUE, sheet="Mesomediterranean")
ThermoMediterranean <- read.xlsx("Herbivore.Species.xlsx", rowNames=FALSE,
                                 colNames=TRUE, sheet="Thermomediterranean")

# make a matrix from Mesotemperate as type "integer"

EuroSib <- as.matrix(apply(EuroSiberian[,-1],2,as.integer))
row.names(EuroSib) <- EuroSiberian$Species # row names = species

# Do the same with the other regions

SubMed <- as.matrix(apply(SubMediterranean[,-1],2,as.integer))
row.names(SubMed) <- SubMediterranean$Species 

MesoMed <- as.matrix(apply(MesoMediterranean[,-1],2,as.integer))
row.names(MesoMed) <- MesoMediterranean$Species 

ThermoMed <- as.matrix(apply(ThermoMediterranean[,-1],2,as.integer))
row.names(ThermoMed) <- ThermoMediterranean$Species 


SpeciesDataset = list(Eurosiberian= EuroSib,
                      Submediterranean=SubMed, Mesomediterranean=MesoMed,
                      Thermomediterranean=ThermoMed)

## Species richness and extrapolation.

out.raw <- iNEXT(SpeciesDataset, datatype="incidence_raw", endpoint=100, nboot=500)

Outputs_a<- as.data.frame(out.raw$AsyEst)

Outputs_b <- as.data.frame (out.raw$iNextEs)

ggiNEXT(out.raw, facet.var="site") #PLOT

# save outputs in xlsx format

write.xlsx(Outputs_a, "Rarefaction_Outputs_Richness.xlsx")
write.xlsx(Outputs_b, "Rarefaction_Outputs_Extrapolation.xlsx")
