# Clear all
rm(list = ls()) 

## Load packages

library(openxlsx)
library(sExtinct)
library(rcarbon)
library(ggplot2)
library(gridExtra)
require(Bchron)
library(gridExtra)

################## The script below was written in  Rstudio, so it is  recommended
############ to be run within RStudio.
### Choose your own directory:

setwd("C:/...")

### FUNCTIONS TO PERFORM OLE MODELS WITH RESAMPLING PROCEDURE

# Function to estimate the extinction time from resampling, see the Methods section for details

T_extinction <- function(x) 
{
  Output_TE <- data.frame("Estimate" = c(NA),
                          "lowerCI" = c(NA),
                          "upperCI" = c(NA))
  set.seed(123)
  for(i in 1:10000) {                                          
    n <- 1
    func1 <- function(x) rnorm(n, mean = x[1], sd = x[2])
    df<-apply(Dataset[,2:3], 1, FUN = func1)
    df<-as.data.frame(df)
    df <- df[with(df, order(-df)), ]
    df<-as.data.frame(df * -1)
    names(df)[names(df) == "df"] <- "years"
    df$sightings<-1
    OLE<-OLE.fun(df, alpha=0.1) #### alpha is 0.1 because it is divided by 2, so the real alpha is 0.05
    Output_TE[nrow(Output_TE) + 1,] <- c(OLE$Estimate, OLE$lowerCI, OLE$upperCI)
    
  }
  print(mean(Output_TE$Estimate, na.rm=TRUE))
  print(mean(Output_TE$lowerCI, na.rm=TRUE))
  print(mean(Output_TE$upperCI, na.rm=TRUE))
  
  Output_TE <-na.omit(Output_TE)
  Output_TE$Cul <- "x"
  
  Output_TE$Estimate<- Output_TE$Estimate * -1
  Plot<- ggplot(data=Output_TE, aes(x=Cul, y=Estimate, fill=Cul)) +
    geom_violin() +
    coord_flip()+
    theme_classic(
    ) +
    ggtitle("x") +
    scale_y_reverse(breaks = seq(30000, 55000, 5000)) +
    xlab("") + stat_summary(fun.data=mean_sdl, 
                            geom="pointrange", color="black")
  Plot

}

# Function to estimate the origin time from resampling

T_origin <- function(x) 
{
  Output_O <- data.frame("Estimate" = c(NA),
                       "lowerCI" = c(NA),
                       "upperCI" = c(NA))
  
  set.seed(123)
  for(i in 1:10000) {                                          
    n <- 1
    func1 <- function(x) rnorm(n, mean = x[1], sd = x[2])
    df<-apply(Dataset[,2:3], 1, FUN = func1)
    df<-as.data.frame(df)
    df <- df[with(df, order(-df)), ]
    df<-as.data.frame(df)
    names(df)[names(df) == "df"] <- "years"
    df$sightings<-1
    OLE<-OLE.fun(df, alpha=0.1)
    Output_O[nrow(Output_O) + 1,] <- c(OLE$Estimate, OLE$lowerCI, OLE$upperCI)
    
  }
  
  print(mean(Output_O$Estimate, na.rm=TRUE))
  print(mean(Output_O$lowerCI, na.rm=TRUE))
  print(mean(Output_O$upperCI, na.rm=TRUE))
  
  Output_O <-na.omit(Output_O)
  Output_O$Cul <- "x"
 
  Plot<- ggplot(data= Output_O, aes(x=Cul, y=Estimate, fill=Cul)) +
    geom_violin() +
    coord_flip()+
    theme_classic() +
    ggtitle("x") +
    scale_y_reverse(breaks = seq(30000, 55000, 5000)) +
    xlab("") + stat_summary(fun.data=mean_sdl, 
                            geom="pointrange", color="black")
  Plot
}



#OPTIMAL LINEAR ESTIMATION


## EUROSIBERIAN REGION, END MOUSTERIAN: Central Range Estimate (CRR)

DF<- read.xlsx("OLE&SPD.xlsx", rowNames= FALSE, colNames= TRUE, sheet="OLE")
head(DF)


Dataset<- subset(DF, Culture=="Mousterian" & Region=="Eurosiberian")
Dataset


CRE_Dataset<- as.data.frame(Dataset$calAgeBP * -1)
CRE_Dataset <- CRE_Dataset[with(CRE_Dataset, order(-CRE_Dataset)), ]
CRE_Dataset<- as.data.frame(CRE_Dataset)
CRE_Dataset$sightings<-1
CRE_Dataset

OLE.fun(CRE_Dataset, alpha=0.1) # alpha is 0.1 because the sExtinct::OLE function divides it by 2, so the real alpha is 0.05

# Resampling:

T_extinction()


## EUROSIBERIAN REGION, START CHATERLPERRONIAN


Dataset<- subset(DF, Culture=="Chatelperronian" & Region=="Eurosiberian")
Dataset

#Central Range Estimate:
CRE_Dataset<- as.data.frame(Dataset$calAgeBP)
CRE_Dataset <- CRE_Dataset[with(CRE_Dataset, order(-CRE_Dataset)), ]
CRE_Dataset<- as.data.frame(CRE_Dataset)
CRE_Dataset$sightings<-1
CRE_Dataset
OLE.fun(CRE_Dataset, alpha=0.01)
# Resampling:
T_origin()


## EUROSIBERIAN REGION, END CHATELPERRONIAN

#Central Range Estimate:

CRE_Dataset<- as.data.frame(Dataset$calAgeBP * -1)
CRE_Dataset <- CRE_Dataset[with(CRE_Dataset, order(-CRE_Dataset)), ]
CRE_Dataset<- as.data.frame(CRE_Dataset)
CRE_Dataset$sightings<-1
CRE_Dataset
OLE.fun(CRE_Dataset, alpha=0.1)

# Resampling:

T_extinction()


## EUROSIBERIAN REGION, START AURIGNACIAN


Dataset<- subset(DF, Culture=="Aurignacian" & Region=="Eurosiberian" & Phase=="Start")
Dataset

#Central Range Estimate:
CRE_Dataset<- as.data.frame(Dataset$calAgeBP)
CRE_Dataset <- CRE_Dataset[with(CRE_Dataset, order(-CRE_Dataset)), ]
CRE_Dataset<- as.data.frame(CRE_Dataset)
CRE_Dataset$sightings<-1
CRE_Dataset
OLE.fun(CRE_Dataset, alpha=0.1)

# Resampling:
T_origin()



## EUROSIBERIAN REGION, END AURIGNACIAN

Dataset<- subset(DF, Culture=="Aurignacian" & Region=="Eurosiberian" & Phase=="End")
Dataset


#Central Range Estimate:
CRE_Dataset<- as.data.frame(Dataset$calAgeBP * -1)
CRE_Dataset <- CRE_Dataset[with(CRE_Dataset, order(-CRE_Dataset)), ]
CRE_Dataset<- as.data.frame(CRE_Dataset)
CRE_Dataset$sightings<-1
CRE_Dataset
OLE.fun(CRE_Dataset, alpha=0.1)

# Resampling:

T_extinction()




## SUPRAMEDITERRANEAN REGION


## SUPRAMEDITERRANEAN REGION, END MOUSTERIAN: Central Range Estimate (CRR)

Dataset<- subset(DF, Culture=="Mousterian" & Region=="Supramediterranean" & Phase=="End")
Dataset

CRE_Dataset<- as.data.frame(Dataset$calAgeBP * -1)
CRE_Dataset <- CRE_Dataset[with(CRE_Dataset, order(-CRE_Dataset)), ]
CRE_Dataset<- as.data.frame(CRE_Dataset)
CRE_Dataset$sightings<-1
CRE_Dataset

OLE.fun(CRE_Dataset, alpha=0.1)

# Resampling:

T_extinction()



##################################################################################
## MESOMEDITERRANEAN REGION


## MESOMEDITERRANEAN REGION, END MOUSTERIAN: Central Range Estimate (MesoMedR)

Dataset<- subset(DF, Culture=="Mousterian" & Region=="Mesomediterranean" & Phase=="End")
Dataset

MesoMedE_Dataset<- as.data.frame(Dataset$calAgeBP * -1)
MesoMedE_Dataset <- MesoMedE_Dataset[with(MesoMedE_Dataset, order(-MesoMedE_Dataset)), ]
MesoMedE_Dataset<- as.data.frame(MesoMedE_Dataset)
MesoMedE_Dataset$sightings<-1
MesoMedE_Dataset

OLE.fun(MesoMedE_Dataset, alpha=0.1)

# Resampling:

T_extinction()


## MESOMEDITERRANEAN REGION, START AURIGNACIAN


Dataset<- subset(DF, Culture=="Aurignacian" & Region=="Mesomediterranean" & Phase=="Start")
Dataset

#Central Range Estimate:
MesoMedE_Dataset<- as.data.frame(Dataset$calAgeBP)
MesoMedE_Dataset <- MesoMedE_Dataset[with(MesoMedE_Dataset, order(-MesoMedE_Dataset)), ]
MesoMedE_Dataset<- as.data.frame(MesoMedE_Dataset)
MesoMedE_Dataset$sightings<-1
MesoMedE_Dataset
OLE.fun(MesoMedE_Dataset, alpha=0.1)

# Resampling:
T_origin()



## MESOMEDITERRANEAN REGION, END AURIGNACIAN

Dataset<- subset(DF, Culture=="Aurignacian" & Region=="Mesomediterranean" & Phase=="End")
Dataset

#Central Range Estimate:
MesoMedE_Dataset<- as.data.frame(Dataset$calAgeBP * -1)
MesoMedE_Dataset <- MesoMedE_Dataset[with(MesoMedE_Dataset, order(-MesoMedE_Dataset)), ]
MesoMedE_Dataset<- as.data.frame(MesoMedE_Dataset)
MesoMedE_Dataset$sightings<-1
MesoMedE_Dataset
OLE.fun(MesoMedE_Dataset, alpha=0.1)

# Resampling:

T_extinction()


## THERMOMEDITERRANEAN REGION


## THERMOMEDITERRANEAN REGION, END MOUSTERIAN: Central Range Estimate (CRR)

Dataset<- subset(DF, Culture=="Mousterian" & Region=="Thermomediterranean" & Phase=="End")
Dataset

CRE_Dataset<- as.data.frame(Dataset$calAgeBP * -1)
CRE_Dataset <- CRE_Dataset[with(CRE_Dataset, order(-CRE_Dataset)), ]
CRE_Dataset<- as.data.frame(CRE_Dataset)
CRE_Dataset$sightings<-1
CRE_Dataset

OLE.fun(CRE_Dataset, alpha=0.1)

# Resampling:

T_extinction()




## THERMOMEDITERRANEAN REGION, START AURIGNACIAN


Dataset<- subset(DF, Culture=="Aurignacian" & Region=="Thermomediterranean" & Phase=="Start")
Dataset

#Central Range Estimate:
CRE_Dataset<- as.data.frame(Dataset$calAgeBP)
CRE_Dataset <- CRE_Dataset[with(CRE_Dataset, order(-CRE_Dataset)), ]
CRE_Dataset<- as.data.frame(CRE_Dataset)
CRE_Dataset$sightings<-1
CRE_Dataset
OLE.fun(CRE_Dataset, alpha=0.1)

# Resampling:
T_origin()



## THERMOMEDITERRANEAN REGION, END AURIGNACIAN

Dataset<- subset(DF, Culture=="Aurignacian" & Region=="Thermomediterranean" & Phase=="End")
Dataset

#Central Range Estimate:
CRE_Dataset<- as.data.frame(Dataset$calAgeBP * -1)
CRE_Dataset <- CRE_Dataset[with(CRE_Dataset, order(-CRE_Dataset)), ]
CRE_Dataset<- as.data.frame(CRE_Dataset)
CRE_Dataset$sightings<-1
CRE_Dataset
OLE.fun(CRE_Dataset, alpha=0.1)

# Resampling:

T_extinction()


####################################################################################
####################
##################      SUMMED PROBABILITY DISTRIBUTION
##################
####################################################################################




## EUROSIBERIAN SPD

DF<- read.xlsx("OLE&SPD.xlsx", rowNames= FALSE, colNames= TRUE, sheet="SPD")
head(DF)


Eurosiberian<- subset(DF, Culture=="Mousterian" & Region=="Eurosiberian")
head(Eurosiberian)



#Mousterian
Mousterian <- subset(Eurosiberian, Culture=="Mousterian")
caldatesMousterian=calibrate(x=Mousterian$Age, errors=Mousterian$s.dev., calCurves='intcal20')

summary(caldatesMousterian)

multiplot(caldatesMousterian, type="d", decreasing=FALSE,rescale=TRUE, gapFactor=1, xlim = c(55000, 30000), cex.id=FALSE)


## binning sensitivity analysis:
bins = binPrep(sites=Mousterian$Site,ages=Mousterian$Age,h=100)
spd.bins = spd(caldatesMousterian,bins=bins,timeRange=c(55000,30000))

binsense(x=caldatesMousterian,y=Mousterian$Site,h=seq(0,500,100),timeRange=c(55000,30000)) 

####

Mousterian.spd <- spd(caldatesMousterian,bins=500, timeRange=c(55000,30000))

plot(Mousterian.spd)
plot(Mousterian.spd,runm=200,add=TRUE,type="simple",col="darkorange",lwd=1.5,lty=2)
Mousterian.spd


MousterianPrDens <- as.data.frame (Mousterian.spd$grid$PrDens)
names(MousterianPrDens)[1] <- 'PrDens'
MousterianPrDens$Age<-Mousterian.spd$grid$calBP
MousterianPrDens$Culture="Mousterian"

head(MousterianPrDens)
#

ggplot(data=MousterianPrDens, aes(x=Age, y=PrDens))+
  geom_line()+
  scale_x_reverse(breaks = seq(30000, 50000, 5500))+
  theme_classic()

## write.xlsx(MousterianPrDens, "SDP.Mousterian_EuroSiberian.xlsx")


### Aurignacian

Aurignacian<- subset(DF, Culture=="Aurignacian" & Region=="Eurosiberian")
head(Aurignacian)


caldatesAurignacian=calibrate(x=Aurignacian$Age, errors=Aurignacian$s.dev., calCurves='intcal20')

summary(caldatesAurignacian)
multiplot(caldatesAurignacian, type="d", decreasing=FALSE,rescale=TRUE, gapFactor=1, xlim = c(55000, 30000), cex.id=FALSE)


## binning sensitivity analysis:
bins = binPrep(sites=Aurignacian$Site,ages=Aurignacian$Age,h=100)
spd.bins = spd(caldatesAurignacian,bins=bins,timeRange=c(55000,30000))

binsense(x=caldatesAurignacian,y=Aurignacian$Site,h=seq(0,500,100),timeRange=c(55000,30000)) 

####
Aurignacian.spd <- spd(caldatesAurignacian, bins=500,timeRange=c(55000,30000))
plot(Aurignacian.spd)

AurignacianPrDens <- as.data.frame (Aurignacian.spd$grid$PrDens)
names(AurignacianPrDens)[1] <- 'PrDens'
AurignacianPrDens$Age<-Aurignacian.spd$grid$calBP
AurignacianPrDens$Culture="Aurignacian"

head(AurignacianPrDens)


#### Chatelperronian

head(DF)
Chatelperronian<- subset(DF, Culture=="Chatelperronian" & Region=="Eurosiberian")

caldatesChatelperronian=calibrate(x=Chatelperronian$Age, errors=Chatelperronian$s.dev., calCurves='intcal20')

summary(caldatesChatelperronian)

multiplot(caldatesChatelperronian, type="d", decreasing=FALSE,rescale=TRUE, gapFactor=1, xlim = c(55000, 30000), cex.id=FALSE)


## binning sensitivity analysis:
bins = binPrep(sites=Chatelperronian$Site,ages=Chatelperronian$Age,h=100)
spd.bins = spd(caldatesChatelperronian,bins=bins,timeRange=c(55000,30000))

binsense(x=caldatesChatelperronian,y=Chatelperronian$Site,h=seq(0,500,100),timeRange=c(55000,30000)) 

####
Chatelperronian.spd <- spd(caldatesChatelperronian, bins=500,timeRange=c(55000,30000))
plot(Chatelperronian.spd)

ChatelperronianPrDens <- as.data.frame (Chatelperronian.spd$grid$PrDens)
names(ChatelperronianPrDens)[1] <- 'PrDens'
ChatelperronianPrDens$Age<-Chatelperronian.spd$grid$calBP
ChatelperronianPrDens$Culture="Chatelperronian"

head(ChatelperronianPrDens)



##### MERGE THE SDP OF ALL TECHNOCOMPLEXES FROM THE EUROSIBERIAN REGION

SPD.Eurosiberian <- rbind(MousterianPrDens, AurignacianPrDens, ChatelperronianPrDens)
head(SPD.Eurosiberian)




#write.xlsx(SPD.Eurosiberian, "SPD.Eurosiberian.xlsx")


#### PLOT

EruoSib_SDP <- ggplot(data = SPD.Eurosiberian, aes(x = Age, y = PrDens, color = Culture)) + 
  geom_line()+
  scale_x_reverse() +
  xlab("cal age BP") + 
  ylab("PrDens") + 
  labs(colour = "Culture") +
  scale_colour_manual (breaks = c("Mousterian", "Aurignacian", "Chatelperronian"),
                       values=c("black","bisque3", "darkolivegreen4" )) +
  scale_y_continuous(expand = c(0.01, 0)) + 
  theme_classic(base_size = 14, base_family = "serif")  + 
  theme(legend.position = "none")

EruoSib_SDP


# NPP EUROSIBERIAN

NPP.Eurosiberian <- read.xlsx("Dataset-NPP.xlsx", rowNames=FALSE,
                              colNames=TRUE, sheet="NPP.Eurosiberian")


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

grid.arrange(Eurosiberian_Iberia_NPP_Plot, EruoSib_SDP)


##_______________###_______________________________________________________
##_____________________________________________-
#### SUPRAMEDITERRANEAN REGION
##_____________________________________________-
##_______________###_______________________________________________________


#Mousterian
Mousterian<- subset(DF, Culture=="Mousterian" & Region=="Supramediterranean")
caldatesMousterian=calibrate(x=Mousterian$Age, errors=Mousterian$s.dev., calCurves='intcal20')

multiplot(caldatesMousterian, type="d", decreasing=FALSE,rescale=TRUE, gapFactor=1, xlim = c(55000, 30000), cex.id=FALSE)

## binning sensitivity analysis:
bins = binPrep(sites=Mousterian$Site,ages=Mousterian$Age,h=100)
spd.bins = spd(caldatesMousterian,bins=bins,timeRange=c(55000,30000))

binsense(x=caldatesMousterian,y=Mousterian$Site,h=seq(0,500,100),timeRange=c(55000,30000)) 

####

Mousterian.spd <- spd(caldatesMousterian,bins=500, timeRange=c(55000,30000))

plot(Mousterian.spd)
plot(Mousterian.spd,runm=200,add=TRUE,type="simple",col="darkorange",lwd=1.5,lty=2)
Mousterian.spd


SupraMed_PrDens <- as.data.frame (Mousterian.spd$grid$PrDens)
names(SupraMed_PrDens)[1] <- 'PrDens'
SupraMed_PrDens$Age<-Mousterian.spd$grid$calBP
SupraMed_PrDens$Culture="Mousterian"

head(SupraMed_PrDens)

#### PLOT



SupraMed_SDP <- ggplot(data = SupraMed_PrDens, aes(x = Age, y = PrDens, color=Culture)) + 
  geom_line()+
  scale_x_reverse() +
  xlab("cal age BP") + 
  ylab("PrDens") + 
  labs(colour = "Culture") +
  scale_color_manual (breaks = c("Mousterian", "Aurignacian", "Chatelperronian"),
                      values=c("black","bisque3", "darkolivegreen4" )) +
  scale_y_continuous(expand = c(0.01, 0)) + 
  theme_classic(base_size = 14, base_family = "serif")  + 
  theme(legend.position = "none")

SupraMed_SDP

# NPP

NPP.Supramediterranean <- read.xlsx("Dataset-NPP.xlsx", rowNames=FALSE,
                                    colNames=TRUE, sheet="NPP.Submediterranean")


Supramediterranean_Iberia_NPP_Plot <- ggplot(NPP.Supramediterranean, aes(x = Age, y = NPP, ymin= NPP - (2*SD),
                                                                         ymax= NPP + (2 * SD))) +
  geom_line( color="black", size = 1) +
  scale_x_reverse(breaks = seq(30000, 55000, 5000)) +
  geom_ribbon(alpha= 0.45) +
  ggtitle ("Supramediterranean region") +
  xlab ("age BP")+
  ylab ("NPP (kg/m^2/yr)") +
  ylim(0, 0.6) +
  theme_classic(base_size = 14, base_family = "serif")+ 
  theme(legend.position = "none")

Supramediterranean_Iberia_NPP_Plot

grid.arrange(Supramediterranean_Iberia_NPP_Plot, SupraMed_SDP)



###______________________________________________-
##______________________-----
## Mesomediterranean
##______________________----
#_______________________________________________

## MESOMEDITERRANEAN SPD

Mousterian<- subset(DF, Culture=="Mousterian" & Region=="Mesomediterranean")

caldatesMousterian=calibrate(x=Mousterian$Age, errors=Mousterian$s.dev., calCurves='intcal20')


multiplot(caldatesMousterian, type="d", decreasing=FALSE,rescale=TRUE, gapFactor=1, xlim = c(55000, 30000), cex.id=FALSE)

## binning sensitivity analysis:
bins = binPrep(sites=Mousterian$Site,ages=Mousterian$Age,h=100)
spd.bins = spd(caldatesMousterian,bins=bins,timeRange=c(55000,30000))

binsense(x=caldatesMousterian,y=Mousterian$Site,h=seq(0,500,100),timeRange=c(55000,30000)) 

####

Mousterian.spd <- spd(caldatesMousterian,bins=500, timeRange=c(55000,30000))

plot(Mousterian.spd)
plot(Mousterian.spd,runm=200,add=TRUE,type="simple",col="darkorange",lwd=1.5,lty=2)
Mousterian.spd


MousterianPrDens <- as.data.frame (Mousterian.spd$grid$PrDens)
names(MousterianPrDens)[1] <- 'PrDens'
MousterianPrDens$Age<-Mousterian.spd$grid$calBP
MousterianPrDens$Culture="Mousterian"

head(MousterianPrDens)
#

ggplot(data=MousterianPrDens, aes(x=Age, y=PrDens))+
  geom_line()+
  scale_x_reverse(breaks = seq(30000, 50000, 5500))+
  theme_classic()

#write.xlsx(MousterianPrDens, "SDP.Mousterian_EuroSiberian.xlsx")



### Aurignacian

Aurignacian<- subset(DF, Culture=="Aurignacian" & Region=="Mesomediterranean")

Aurignacian <- subset(Mesomediterranean, Culture=="Aurignacian")
caldatesAurignacian=calibrate(x=Aurignacian$Age, errors=Aurignacian$s.dev., calCurves='intcal20')


multiplot(caldatesAurignacian, type="d", decreasing=FALSE,rescale=TRUE, gapFactor=1, xlim = c(55000, 30000), cex.id=FALSE)


## binning sensitivity analysis:
bins = binPrep(sites=Aurignacian$Site,ages=Aurignacian$Age,h=100)
spd.bins = spd(caldatesAurignacian,bins=bins,timeRange=c(55000,30000))

binsense(x=caldatesAurignacian,y=Aurignacian$Site,h=seq(0,500,100),timeRange=c(55000,30000)) 

####
Aurignacian.spd <- spd(caldatesAurignacian, bins=500,timeRange=c(55000,30000))
plot(Aurignacian.spd)

AurignacianPrDens <- as.data.frame (Aurignacian.spd$grid$PrDens)
names(AurignacianPrDens)[1] <- 'PrDens'
AurignacianPrDens$Age<-Aurignacian.spd$grid$calBP
AurignacianPrDens$Culture="Aurignacian"

head(AurignacianPrDens)

ggplot(data=AurignacianPrDens, aes(x=Age, y=PrDens))+
  geom_area()+
  scale_x_reverse(breaks = seq(30000, 50000, 5000))+
  theme_classic()


##### MERGE THE SDP OF ALL TECHNOCOMPLEXES FROM THE MESOMEDITERRANEAN REGION

SPD.Mesomediterranean <- rbind(MousterianPrDens, AurignacianPrDens)
head(SPD.Mesomediterranean)


#write.xlsx(SPD.Mesomediterranean, "SPD.Mesomediterranean.xlsx")



#### PLOT



MesoMed_SDP <- ggplot(data = SPD.Mesomediterranean, aes(x = Age, y = PrDens, color = Culture)) + 
  geom_line()+
  scale_x_reverse() +
  xlab("cal age BP") + 
  ylab("PrDens") + 
  labs(colour = "Culture") +
  scale_color_manual (breaks = c("Mousterian", "Aurignacian"),
                      values=c("black","bisque3")) +
  scale_y_continuous(expand = c(0.01, 0)) + 
  theme_classic(base_size = 14, base_family = "serif")  + 
  theme(legend.position = "none")

MesoMed_SDP


# NPP MESOMEDITERRANEAN

NPP.Mesomediterranean <- read.xlsx("Dataset-NPP.xlsx", rowNames=FALSE,
                                   colNames=TRUE, sheet="NPP.Mesomediterranean")


Mesomediterranean_Iberia_NPP_Plot <- ggplot(NPP.Mesomediterranean, aes(x = Age, y = NPP, ymin= NPP - (2*SD),
                                                                       ymax= NPP + (2 * SD))) +
  geom_line( color="black", size = 1) +
  scale_x_reverse(breaks = seq(30000, 55000, 5000)) +
  geom_ribbon(alpha= 0.45) +
  ggtitle ("Mesomediterranean region") +
  xlab ("age BP")+
  ylab ("NPP (kg/m^2/yr)") +
  ylim(0, 0.6) +
  theme_classic(base_size = 14, base_family = "serif")+ 
  theme(legend.position = "none")

Mesomediterranean_Iberia_NPP_Plot

grid.arrange(Mesomediterranean_Iberia_NPP_Plot, MesoMed_SDP)






###______________________________________________-
##______________________-----
## Thermomediterranean
##______________________----
#_______________________________________________

## THERMOMEDITERRANEAN SPD

Mousterian<- subset(DF, Culture=="Mousterian" & Region=="Thermomediterranean")

caldatesMousterian=calibrate(x=Mousterian$Age, errors=Mousterian$s.dev., calCurves='intcal20')

summary(caldatesMousterian)

multiplot(caldatesMousterian, type="d", decreasing=FALSE,rescale=TRUE, gapFactor=1, xlim = c(55000, 30000), cex.id=FALSE)


## binning sensitivity analysis:
bins = binPrep(sites=Mousterian$Site,ages=Mousterian$Age,h=100)
spd.bins = spd(caldatesMousterian,bins=bins,timeRange=c(55000,30000))

binsense(x=caldatesMousterian,y=Mousterian$Site,h=seq(0,500,100),timeRange=c(55000,30000)) 

####

Mousterian.spd <- spd(caldatesMousterian,bins=500, timeRange=c(55000,30000))

plot(Mousterian.spd)


MousterianPrDens <- as.data.frame (Mousterian.spd$grid$PrDens)
names(MousterianPrDens)[1] <- 'PrDens'
MousterianPrDens$Age<-Mousterian.spd$grid$calBP
MousterianPrDens$Culture="Mousterian"

head(MousterianPrDens)
#

ggplot(data=MousterianPrDens, aes(x=Age, y=PrDens))+
  geom_line()+
  scale_x_reverse(breaks = seq(30000, 50000, 5500))+
  theme_classic()

#write.xlsx(MousterianPrDens, "SDP.Mousterian_EuroSiberian.xlsx")



### Aurignacian

Aurignacian<- subset(DF, Culture=="Aurignacian" & Region=="Thermomediterranean")

caldatesAurignacian=calibrate(x=Aurignacian$Age, errors=Aurignacian$s.dev., calCurves='intcal20')
multiplot(caldatesAurignacian, type="d", decreasing=FALSE,rescale=TRUE, gapFactor=1, xlim = c(55000, 30000), cex.id=FALSE)

## binning sensitivity analysis:
bins = binPrep(sites=Aurignacian$Site,ages=Aurignacian$Age,h=100)
spd.bins = spd(caldatesAurignacian,bins=bins,timeRange=c(55000,30000))

binsense(x=caldatesAurignacian,y=Aurignacian$Site,h=seq(0,500,100),timeRange=c(55000,30000)) 

####
Aurignacian.spd <- spd(caldatesAurignacian, bins=500,timeRange=c(55000,30000))
plot(Aurignacian.spd)

AurignacianPrDens <- as.data.frame (Aurignacian.spd$grid$PrDens)
names(AurignacianPrDens)[1] <- 'PrDens'
AurignacianPrDens$Age<-Aurignacian.spd$grid$calBP
AurignacianPrDens$Culture="Aurignacian"

head(AurignacianPrDens)


#### Chatelperronian


Chatelperronian<- subset(DF, Culture=="Chatelperronian" & Region=="Thermomediterranean")
caldatesChatelperronian=calibrate(x=Chatelperronian$Age, errors=Chatelperronian$s.dev., calCurves='intcal20')
summary(caldatesChatelperronian)

plot(caldatesChatelperronian)


## binning sensitivity analysis:
bins = binPrep(sites=Chatelperronian$Site,ages=Chatelperronian$Age,h=100)
spd.bins = spd(caldatesChatelperronian,bins=bins,timeRange=c(55000,30000))

binsense(x=caldatesChatelperronian,y=Chatelperronian$Site,h=seq(0,500,100),timeRange=c(55000,30000)) 

####
Chatelperronian.spd <- spd(caldatesChatelperronian, bins=500,timeRange=c(55000,30000))
plot(Chatelperronian.spd)

ChatelperronianPrDens <- as.data.frame (Chatelperronian.spd$grid$PrDens)
names(ChatelperronianPrDens)[1] <- 'PrDens'
ChatelperronianPrDens$Age<-Chatelperronian.spd$grid$calBP
ChatelperronianPrDens$Culture="Chatelperronian"

head(ChatelperronianPrDens)



##### MERGE THE SDP OF ALL TECHNOCOMPLEXES FROM THE THERMOMEDITERRANEAN REGION

SPD.Thermomediterranean <- rbind(MousterianPrDens, AurignacianPrDens, ChatelperronianPrDens)
head(SPD.Thermomediterranean)


#write.xlsx(SPD.Thermomediterranean, "SPD.Thermomediterranean.xlsx")



#### PLOT



ThermoMed_SDP <- ggplot(data = SPD.Thermomediterranean, aes(x = Age, y = PrDens, color = Culture)) + 
  geom_line()+
  scale_x_reverse() +
  xlab("cal age BP") + 
  ylab("PrDens") + 
  labs(colour = "Culture") +
  scale_color_manual (breaks = c("Mousterian", "Aurignacian", "Chatelperronian"),
                      values=c("black","bisque3", "darkolivegreen4" )) +
  scale_y_continuous(expand = c(0.01, 0)) + 
  theme_classic(base_size = 14, base_family = "serif")  + 
  theme(legend.position = "none")

ThermoMed_SDP


# NPP THERMOMEDITERRANEAN

NPP.Thermomediterranean <- read.xlsx("Dataset-NPP.xlsx", rowNames=FALSE,
                                     colNames=TRUE, sheet="NPP.Thermomediterranean")


Thermomediterranean_Iberia_NPP_Plot <- ggplot(NPP.Thermomediterranean, aes(x = Age, y = NPP, ymin= NPP - (2*SD),
                                                                           ymax= NPP + (2 * SD))) +
  geom_line( color="black", size = 1) +
  scale_x_reverse(breaks = seq(30000, 55000, 5000)) +
  geom_ribbon(alpha= 0.45) +
  ggtitle ("Thermomediterranean region") +
  xlab ("age BP")+
  ylab ("NPP (kg/m^2/yr)") +
  ylim(0, 0.6) +
  theme_classic(base_size = 14, base_family = "serif")+ 
  theme(legend.position = "none")

Thermomediterranean_Iberia_NPP_Plot
###

grid.arrange(Thermomediterranean_Iberia_NPP_Plot, ThermoMed_SDP)




grid.arrange(Eurosiberian_Iberia_NPP_Plot, EruoSib_SDP, 
             Supramediterranean_Iberia_NPP_Plot, SupraMed_SDP,Mesomediterranean_Iberia_NPP_Plot, 
             MesoMed_SDP, Thermomediterranean_Iberia_NPP_Plot, ThermoMed_SDP, ncol=1)



