#Code associated with Endris and Rehm 2025
#DOI
#Link
#statistical analysis of phenology varied by growing degree days (GDD), year, and species

library(tidyverse)
library(MuMIn)
library(multcomp)
library(emmeans)
library(nlme)

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
#random effect of individual
GDD$individual_ID<-paste(GDD$species,GDD$individual,sep="")
DOY$individual_ID<-paste(DOY$species,DOY$individual,sep="")

#model selection and model for phenology stage 2 for GDD/DOY, year, and species
# mod<-glm(ph2~year*species,data=GDD,na.action="na.fail")
# summary(mod)
# dredge(mod)
#mixed model
mod<-lme(ph2~year*as.factor(species),random=~1|individual_ID,data=GDD,na.action="na.fail")
dredge(mod)
#best model
mod<-lme(ph2~year*as.factor(species),random=~1|individual_ID,data=GDD,na.action="na.fail")
summary(aov(mod))
#pairwise comparison on species, while accounting for the year interaction
emm.species<-pairs(emmeans(mod, pairwise~ species|year))
emm.species#we can see the L. tulipifera is different than f. grandifolia but only in 2022
#in 2023 all species more or less converge onto the same date.

#for DOY
mod<-lme(ph2~year*as.factor(species),random=~1|individual_ID,data=DOY,na.action="na.fail")
dredge(mod)
#best model
mod<-lme(ph2~year*as.factor(species),random=~1|individual_ID,data=DOY,na.action="na.fail")
summary(aov(mod))
#pairwise comparison on species, while accounting for the year interaction
emm.species<-pairs(emmeans(mod, pairwise~ species|year))
emm.species#we can see the L. tulipifera is different than f. grandifolia and a. saccharum but only in 2022
#in 2023 all species more or less converge onto the same date.

#model selection and model for phenology stage 3 for GDD/DOY, year, and species
mod<-lme(ph3~year*as.factor(species),random=~1|individual_ID,data=GDD,na.action="na.fail")
dredge(mod)
#best model
mod<-lme(ph3~year*as.factor(species),random=~1|individual_ID,data=GDD,na.action="na.fail")
summary(aov(mod))
#pairwise comparison on species, while accounting for the year interaction
emm.species<-pairs(emmeans(mod, pairwise~ species|year))
emm.species#we can see the L. tulipifera is different than f. grandifolia but only in 2022
#in 2023 all species more or less converge onto the same date.

#for DOY
mod<-lme(ph3~year*as.factor(species),random=~1|individual_ID,data=DOY,na.action="na.fail")
dredge(mod)
#best model
mod<-lme(ph3~year*as.factor(species),random=~1|individual_ID,data=DOY,na.action="na.fail")
summary(aov(mod))
#pairwise comparison on species, while accounting for the year interaction
emm.species<-pairs(emmeans(mod, pairwise~ species|year))
emm.species#we can see the L. tulipifera is different than f. grandifolia and a. saccharum but only in 2022
#in 2023 all species more or less converge onto the same date.



