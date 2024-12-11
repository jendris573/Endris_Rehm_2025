#code specifically comparing LT50 differences among phenology stages
#we have no LT50 for stage 4 phenology
#potential focus on just stage 3 phenology since we have data for that for all species and years

#read in raw data for LT50 values
outputs<-read_excel("data/LT50 master.xlsx")

#######################################
# Does LT50 vary with phenology status ----
#######################################
outputs$phen<-as.factor(outputs$phen)
outputs$year<-as.factor(outputs$year)
outputs$Species<-as.factor(outputs$Species)

#Model development and model selection of LT50 by species, phenology stage, and year with all two-way interactions
mod<-glm(LT50~(phen+year+Species)^2,data=outputs,na.action="na.fail")
summary(mod)
dredge(mod)

#best model
mod1<-glm(LT50~phen+Species+year,data=outputs)
summary(mod1)

#comparing specific factor levels
summary(glht(mod1, mcp(phen="Tukey")))
summary(glht(mod1,mcp(Species="Tukey")))




