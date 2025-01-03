#code specifically comparing LT50 differences among phenology stages
#focus on just stage 3 phenology since we have data for that for all species and years

library(readxl)
library(ggplot2)
library(emmeans)
library(nlme)
library(MuMIn)

#read in raw data for LT50 values
outputs<-read_excel("data/LT50 master.xlsx")

#######################################
# Does LT50 vary with phenology status ----
#######################################
outputs$phen<-as.factor(outputs$phen)
outputs$year<-as.factor(outputs$year)
outputs$Species<-as.factor(outputs$Species)

#random effect of individual
outputs$individual_ID<-paste(outputs$Species,outputs$Individual,sep="")

#Model development and model selection of LT50 by species, phenology stage, and year with all two-way interactions
mod<-lme(LT50~phen+year*Species,random=~1|individual_ID,data=outputs,na.action="na.fail")
summary(mod)
dredge(mod)

#best model
mod<-lme(LT50~phen+year+Species,random=~1|individual_ID,data=outputs,na.action="na.fail")
summary(aov(mod))
summary(mod)
aov(mod)
#comparing specific factor levels
EMM1<-emmeans(mod,~phen)
pairs(EMM1)
#pairwise comparison on species
EMM2<-emmeans(mod,~Species)
pairs(EMM2)

#pairwise comparison on year,
EMM3<-emmeans(mod,~year)
pairs(EMM3)





