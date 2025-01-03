#calculating thermal safety margins based on LT50 and minimum temperatures
#experienced +/- 7 days around each sampling date

library(readxl)
library(tidyverse)
library(multcomp)
library(MuMIn)
library(nlme)
library(emmeans)
library(caTools)#moving window calculation

tenn1980<-read_xlsx("data/tenn1980.xlsx")
#import LT50 values
LT50<-read_excel("data/LT50 master.xlsx")

#Calculate the minimum temperature for each day and year on a rolling +/- 7 day average
temp<-tenn1980%>%
  group_by(year)%>%
  mutate(what=runmin(TMIN,14)) #calculating rolling average

#adds the minimum temp for that specific year
test<-left_join(LT50,temp[,c(7, 9, 10)],by=c("year","julian_date"),relationship = "many-to-many")
colnames(test)[14]<-"current_year_min"      

#get long-term average minimum temp
tempmeanMIN<-temp%>%
  filter(year<2022)%>%
  group_by(julian_date)%>%
  summarize(meanMIN=mean(TMIN,na.rm=TRUE),
            minMIN=min(TMIN,na.rm=TRUE))

#attach that long term min to LT50
test<-left_join(test,tempmeanMIN,by=c("julian_date"))

#calculate safety margin from both the current year and long-term min temp
#positive values indicate LT50 value is more negative than minimum temperatures
#positive values indicate no freezing risk

#compare thermal safety margins calculated as LT50-minimum temperature of the sample year (2022 or 2023)
test$smcurrent<-test$current_year_min-test$LT50

#compare thermal safety margins calculated as LT50-minimum temperature since 1980
test$smlong<-test$minMIN-test$LT50

#create random effects
test$individual_ID<-paste(test$Species,test$Individual,sep="")

#model development and model selection of thermal safety margins against fixed effects Julian date, species, and sample year
mod<-lme(smcurrent~julian_date+year*Species,random=~1|individual_ID,data=test,na.action="na.fail")
summary(mod)
dredge(mod)

#best selected model
mod<-lme(smcurrent~year+Species,random=~1|individual_ID,data=test,na.action="na.fail")
summary(aov(mod))
##pairwise comparison on species - no year interaction
EMM1<-emmeans(mod,~Species)
pairs(EMM1)
##pairwise comparison on year - no species interaction
EMM2<-emmeans(mod,~year)
pairs(EMM2)

#model development and model selection of thermal safety margins against fixed effects Julian date, species and long term climate since 1980 
mod<-lme(smlong~julian_date+year*Species,random=~1|individual_ID,data=test,na.action="na.fail")
summary(mod)
dredge(mod)

#best selected model
mod<-lme(smlong~julian_date+year+Species,random=~1|individual_ID,data=test,na.action="na.fail")
summary(aov(mod))
##pairwise comparison on species - no year interaction
EMM1<-emmeans(mod,~Species)
pairs(EMM1)
##pairwise comparison on year - no species interaction
EMM2<-emmeans(mod,~year)
pairs(EMM2)
