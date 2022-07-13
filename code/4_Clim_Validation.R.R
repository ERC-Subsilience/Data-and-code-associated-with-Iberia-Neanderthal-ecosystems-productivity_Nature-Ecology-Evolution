# Clear all
rm(list = ls()) 

## Load libraries

library(openxlsx)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(analogue)

## Directory

setwd("C:/...")


######################### GENERATE PREDICTIVE FUNCTIONS

###################################### MAT model
PercentageSpecies <- read.xlsx("Clim_ValidationDatast.xlsx", rowNames=TRUE, 
                       colNames =TRUE, sheet="Dataset")


MAT.Dataset <- read.xlsx("Clim_ValidationDatast.xlsx", rowNames=FALSE, 
               colNames=TRUE, sheet="T_ann")

pred.temp <-wa(PercentageSpecies, MAT.Dataset$T_ann, deshrink= "monotonic")
pred.temp

##Bootstrap
boot <- bootstrap(pred.temp, n.boot = 500)
performance(boot)


##################################### MAP model


MAP.Dataset <- read.xlsx("Clim_ValidationDatast.xlsx", 
                 rowNames=FALSE, colNames=TRUE, sheet="P_ann")


pred.prep<-wa(PercentageSpecies, MAP.Dataset$P_ann, deshrink= "monotonic")
pred.prep

##Bootstrap
boot <- bootstrap(pred.prep, n.boot = 500)
performance(boot)



###################### APPLY PREDICTIVE FUNCTIONS

##IberianPeninsula

Dataset <- read.xlsx("Clim_ValidationDatast.xlsx", 
                  rowNames=FALSE, colNames=TRUE, sheet="Pred.Pollen.IberianPeninsula")

head(Dataset)
IberianPeninsula <-Dataset[,3:51]



pred.prep.IberianPeninsula <- predict(pred.prep, IberianPeninsula, tol.dw=TRUE)
pred.prep.IberianPeninsula

precip <- as.data.frame(pred.prep.IberianPeninsula$pred$pred)
precip

write.xlsx(precip, "PrecipEstimations.xlsx")

### Temperature estimated

pred.temp.IberianPeninsula <- predict(pred.temp, IberianPeninsula, tol.dw=TRUE)
pred.temp.IberianPeninsula
temp <- as.data.frame(pred.temp.IberianPeninsula$pred$pred)
temp


write.xlsx(temp, "TempEstimations.xlsx")

######## Open the file with the mean MAT and MAP estimated in each archaeological level (from previous 'TempEstimations.xlsx' and 'PrecipEstimations.xlsx')

## Evaluation of the Delta Correction method

Dataset <- read.xlsx("Clim_ValidationDatast.xlsx", rowNames=FALSE,
                     colNames=TRUE, sheet="DeltaCorrection")
head(Dataset)

### Mean difference between observed and predicted values of MAT before ('Not bias corrected') and
# after ('Bias-corrected') using the Delta correction method:
aggregate(D_MAT~Bias, data=Dataset, mean)

### Mean difference between observed and predicted values of MAP before ('Not bias corrected') and
# after ('Bias-corrected') using the Delta correction method:
aggregate(D_MAP~Bias, data=Dataset, mean)

### Plot difference between observed and predicted values.

MaT_B <-ggplot(data=Dataset, aes(x=D_MAT,y=SiteLevel, group=Bias)) +
  geom_point(aes(shape=Bias, color=Bias))+
  geom_vline(xintercept = 0, colour= "red")+
  xlab("Difference between observed and predicted MAT (ºC)") +
  ylab("Site and Level")

MaP_B <-ggplot(data=Dataset, aes(x=D_MAP,y=SiteLevel, group=Bias)) +
  geom_point(aes(shape=Bias, color=Bias))+
  geom_vline(xintercept = 0, colour= "red")+
  xlab("Difference between observed and predicted MAP (mm/month)") +
  ylab("Site and Level")


grid.arrange(MaT_B, MaP_B, ncol=2)

### Correlation Observed vs Predicted

Dataset <- read.xlsx("Clim_ValidationDatast.xlsx", rowNames=FALSE,
                     colNames=TRUE, sheet="Outcomes")
head(Dataset)

MatPlot <- ggplot(data = Dataset, aes(x = MAT_Pollen, y = MAT_Armstrong_BiasCorrected)) +
  geom_point() +
  stat_smooth(method = "lm") +
  scale_y_continuous(expand = c(0.01, 0)) +
  xlab("Observed MAT (ºC)") +
  ylab("Predicted MAT (ºC)") +
  stat_cor(method = "pearson") +
  RcmdrPlugin.KMggplot2::theme_simple(base_size = 14, base_family = "sans")


MaPPlot <- ggplot(data = Dataset, aes(x = MAP_Pollen, y = MAP_Armstrong_BiasCorrected)) +
  geom_point() +
  stat_smooth(method = "lm") +
  scale_y_continuous(expand = c(0.01, 0)) +
  xlab("Observed MAP (mm/month)") +
  ylab("Predicted MAP (mm/month)") +
  stat_cor(method = "pearson") +
  RcmdrPlugin.KMggplot2::theme_simple(base_size = 14, base_family = "sans")

grid.arrange(MatPlot, MaPPlot, ncol=2)

