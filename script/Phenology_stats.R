#Code associated with Endris and Rehm 2025
#DOI
#Link
#statistical analysis of phenology varied by growing degree days (GDD), year, and species



library(tidyverse)
library(MuMIn)
library(multcomp)

# # # # # # # # # # # #
#data preparation ----
# # # # # # # # # # # #

DOY<-read.csv("data/DOY_phenology.csv")
GDD<-read.csv("data/GDD_phenology.csv")

DOY$species<-as.factor(DOY$species)
GDD$species<-as.factor(GDD$species)

#did phenology vary with GDD, year, and species
GDD2<-GDD%>%
  pivot_longer(4:7)
colnames(GDD2)[4]<-"phenology"

#model selection and model for phenology stage 2 for GDD/DOY, year, and species
mod<-glm(ph2~year*species,data=GDD,na.action="na.fail")
summary(mod)
dredge(mod)

#best model
mod_GDDph2<-glm(ph2~year+species,data=GDD)
summary(mod_GDDph2)
summary(glht(mod_GDDph2, mcp(species="Tukey")))
#for DOY
mod<-glm(ph2~year*species,data=DOY,na.action="na.fail")
summary(mod)
dredge(mod)
mod_DOYph2<-glm(ph2~year+species,data=DOY)
summary(mod_DOYph2)
summary(glht(mod_DOYph2, mcp(species="Tukey")))

#model selection and model for phenology stage 3 for GDD/DOY, year, and species
mod<-glm(ph3~year*species,data=GDD,na.action="na.fail")
summary(mod)
dredge(mod)
mod_GDDph3<-glm(ph3~year*species,data=GDD)
summary(mod_GDDph3)
summary(glht(mod_GDDph3, mcp(species="Tukey")))

#for DOY
mod<-glm(ph3~year*species,data=DOY,na.action="na.fail")
summary(mod)
dredge(mod)
mod_DOYph3<-glm(ph3~year+species,data=DOY)
summary(mod_DOYph3)
summary(glht(mod_DOYph3, mcp(species="Tukey")))



