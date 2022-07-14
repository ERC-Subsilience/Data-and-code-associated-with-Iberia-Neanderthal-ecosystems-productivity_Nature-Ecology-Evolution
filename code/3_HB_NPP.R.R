rm(list = ls()) # Clear all

## Load libraries

library(openxlsx)
library(tidyverse)
library(caret)
library(olsrr)
library(robustbase)
library(ggpubr)

#### Directory:

setwd("C:/...")


#Load dataset

Dataset <- read.xlsx("HB_ValidationDataset.xlsx", rowNames=FALSE,
                                colNames=TRUE, sheet="NPP_HB")
head(Dataset)

## Compute pearson correlation coefficients and plot:

#### PLOTS

ggplot(data = Dataset, aes(x = logNPP, y = logHB, colour = community.type, shape = community.type)) +
  geom_point() +
  stat_smooth(aes(fill = community.type), method = "lm") +
  xlab("log10(NPP)") +
  ylab("log10(HB)") +
  labs(colour = "community.type", shape = "community.type", fill = "community.type") +
  RcmdrPlugin.KMggplot2::theme_simple(base_size = 14, base_family = "sans") +
  theme(legend.position = "right")+
  stat_cor(method = "pearson")



ggplot(data = Dataset, aes(x = logNPP, y = logHB)) +
  geom_point() +
  stat_smooth(method = "lm") +
  xlab("log10(NPP)") +
  ylab("log10(HB)") +
  labs(colour = "community.type", shape = "community.type", fill = "community.type") +
  RcmdrPlugin.KMggplot2::theme_simple(base_size = 14, base_family = "sans") +
  theme(legend.position = "right")+
  stat_cor(method = "pearson")




### Obtain the model with "Robust Estimates for Linear Models': with this technique is less likely
# that a small fraction of outliers have a large effect on the results obtained:

model <- lmrob(logHB~logNPP, data = Dataset) 

model

summary(model)


### Load dataset with the NPP of extant exosystems (for model validation)

PredictionDataset <- read.xlsx("HB_ValidationDataset.xlsx", rowNames=FALSE,
                               colNames=TRUE, sheet="Sites")

head(PredictionDataset)


predictions<- as.data.frame(predict(model, newdata = PredictionDataset, interval = 'prediction'))

predictions

# save outputs
write.xlsx(predictions, "predValues.xlsx")


