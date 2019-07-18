# Git has been integrated
rm(list = ls())
library(tidyverse) 
library(lubridate)
library(BIOMASS)
library(ape)
library(ggplot2)
summarise = dplyr::summarise

Field_Data <- read.csv("Raw_Data/File_for_OR.csv")

# Omitting the NA Values----
Field_Data <- na.omit(Field_Data)
str(Field_Data)
hist(Field_Data$DBH_cm)
View(Field_Data)


# Checking typos in taxonomy----
Taxo <- correctTaxo(genus=Field_Data$Genus, species = Field_Data$Species)
Field_Data$genusCorr <- Taxo$genusCorrected
Field_Data$speciesCorr <- Taxo$speciesCorrected


# Retrieving APG III Families and Orders from Genus names----
APG <- getTaxonomy(Field_Data$genusCorr, findOrder = T)
Field_Data$familyAPG <- APG$family
Field_Data$orderAPG <- APG$order


# 2-RETRIEVE WOOD DENSITY----
dataWD <- getWoodDensity(genus=as.character(Field_Data$genusCorr),
                         species=as.character(Field_Data$speciesCorr),
                         stand=Field_Data$Plot)


sum(dataWD$levelWD == "species")

sum(dataWD$levelWD == "genus")

sum(!dataWD$levelWD%in%c("genus","species"))

Field_Data$WD <- dataWD$meanWD
str(Field_Data)

sum(Field_Data$Height_m)

AGBtree<-computeAGB(D=Field_Data$DBH_cm,
                    WD=Field_Data$WD,
                    H = Field_Data$Height_m)

# Compute AGB(Mg) per tree----
Field_Data$AGB_Mg <- AGBtree

# Compute Carbon (Mg) per tree --
Field_Data <- Field_Data %>%
  mutate(Carbon_Mg= AGB_Mg*0.47)


Field_Carbon <- Field_Data %>% 
  group_by(Plot) %>%
  dplyr::summarise(Total_C = sum(Carbon_Mg)) %>% 
  mutate(C_ha=Total_C/0.04)

hist(Field_Carbon$C_ha)
str(Field_Carbon)


# Carbon distribution in the plots---------------------------------------------------------------------------------
barplot(Field_Carbon$C_ha, Field_Carbon$Plot)

# Diameter distribution in three areas---------------------------------------------------------------------------

# Field_Data$Plot <- as.factor(Field_Data$Plot)
# cbPalette=c("forestgreen","blue", "darkred")
# ggplot(Field_Data, aes(x = Plot, y = Dbh_cm)) +
#   geom_boxplot(aes(fill = Location), alpha = 0.5)+
#   geom_jitter(aes(color = Location, width = 0.01))+
#   scale_fill_manual(values=cbPalette)+ # Boxplot fill color
#   scale_color_manual(values = cbPalette)+# Jitter color palette
#   ylab("DBH (cm)") +
#   xlab("Location")+
#   scale_y_continuous(breaks = c(0,20,40,60,80,100))+
#   ylim(1,100)+
#   theme_bw()+
#   theme(text = element_text(size=15), axis.text.x = element_blank(),
#         axis.title.x = element_text(margin = margin(t = 18, r = 0, b = 0, l = 0)),
#         axis.title.y = element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)))

# # Height distribution in three areas---------------------------------------------------------------------------------
# ggplot(Field_Data, aes(x = Location, y = H_m)) +
#   geom_boxplot(aes(fill = Location), alpha = 0.5)+
#   geom_jitter(aes(color = Location))+
#   scale_fill_manual(values=cbPalette)+ # Boxplot fill color
#   scale_color_manual(values = cbPalette)+# Jitter color palette
#   ylab("Height(m)") +
#   xlab("Location")+
#   scale_y_continuous(breaks = c(0,10,20,30,40,50))+ 
#   ylim(1,50)+
#   theme_bw()+
#   theme(text = element_text(size=15), axis.text.x = element_blank(),
#         axis.title.x = element_text(margin = margin(t = 18, r = 0, b = 0, l = 0)),
#         axis.title.y = element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)))
# 
# 
# # Height and diameter relationship------------------------------------------------------------------------------------
# Field_Data %>%
#   na.omit %>%
#   ggplot(aes(y = H_m, x = Dbh_cm)) +
#   geom_point(position = "jitter", color = "green", size = 3)+
#   xlab("DBH (cm)")+
#   ylab("Height-H (m)")+
#   scale_x_continuous(breaks = c(0,20,40,60,80,100, 120))+
#   geom_smooth(method = "lm")+
#   theme_bw() +
#   theme(text = element_text(size=15),axis.text.x  = element_text(angle=0, hjust=2),
#         axis.title.x = element_text(margin = margin(t = 18, r = 0, b = 0, l = 0)),
#         axis.title.y = element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)))
# 
# Field_Data %>% lm(H_m~Dbh_cm)
# 
# LmHD <- lm(Field_Data$H_m~Field_Data$Dbh_cm)
# summary(LmHD)
# 
# 
# 
# Draft-------
#   Data1 <- na.omit(Field_Data)
# > View(Data1)
# > View(Field_Data)
# > View(Data1)
# > Data2 <- na.omit(Field_Data$Genus)
# > Data2 <- na.omit(Field_Data[,-Genus])
# Error in `[.data.frame`(Field_Data, , -Genus) : object 'Genus' not found
# > Data2 <- na.omit(Field_Data[,-Field_Data$Genus])
# Error in `[.data.frame`(Field_Data, , -Field_Data$Genus) : 
#   undefined columns selected
# In addition: Warning message:
#   In Ops.factor(Field_Data$Genus) : ‘-’ not meaningful for factors
# > Data2 <- na.omit(Field_Data[-Genus])
# Error in `[.data.frame`(Field_Data, -Genus) : object 'Genus' not found
# > Data2 <- na.omit(Field_Data[-7])
# > View(Data2)
