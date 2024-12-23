### Statistical analysis for Clarksville, TN, USA climate
### Written by Joe Endris
### With input from Evan Rehm

# # # # # # # # #
## Libraries ----
# # # # # # # # #

library(readxl)
library(writexl)
library(fitdistrplus)
library(lubridate)
library(MuMIn)
library(dplyr)
library(pracma)
library(multcomp)
library(ggplot2)
library(gridExtra)

# # # # # # # # # # # # #
## Data Preparation ----
# # # # # # # # # # # # #

#Load NOAA Climate Data Online data
tenn1980<-read_excel("data/tenn1980.xlsx")

# # # # # # # # # # # # # #
## TMIN by Julian date ----
# # # # # # # # # # # # # #

#model to evaluate changes in TMIN by Julian date every years since 1980
TMIN_model <- lm(TMIN ~ julian_date * year , data=tenn1980)

summary(TMIN_model)


# # # # # # # # # #
## Last freeze ----
# # # # # # # # # #

#calculate last day below 0 for each year since 1980
last_freeze <- tenn1980%>%
  filter(TMIN< 0)%>%
  filter(julian_date<180)%>%
  group_by(year)%>%
  filter(row_number()==n())

#calculate mean last freeze for TN since 1980
mean(as.numeric(last_freeze$julian_date))

#statistical model for changes in last freeze date
last_freeze_mod <- lm(julian_date~year, data=last_freeze)
summary(last_freeze_mod)

# # # # # # # # # # # # # # # # # # # # # # #
## The number of days below 0 since 1980 ----
# # # # # # # # # # # # # # # # # # # # # # #

#determine number of spring days below 0
freeze_days <- tenn1980 %>%
  group_by(year) %>%
  filter(month <5) %>%
  summarise(total_days=sum(TMIN < 0))

mod_neg2 <- lm(total_days~year, data=freeze_days)
summary(mod_neg2)

# # # # # # # # # # # # # # # # # # # # #
## Absolute Low by year since 1980 ----
# # # # # # # # # # # # # # # # # # # # #

#Determine absolute coldest day by year
tenn1980$DATE <- as.Date(tenn1980$DATE)

yearly_TMIN <- tenn1980 %>%
  group_by(year) %>%
  summarise(temp = min(TMIN, na.rm = TRUE))

absolute_TMIN <- lm(temp~year, data=yearly_TMIN)
summary(absolute_TMIN)

# # # # # # # # # # # # # # # # # # # # # # # # # # #
## Mean low temps grouped and then for February, March and April ----
# # # # # # # # # # # # # # # # # # # # # # # # # # #

#broad model for January 1 - April 30
overall_model <- lm(TMIN ~ julian_date + year, data = tenn1980, na.action="na.fail")
summary(overall_model)
aov(overall_model)

February_mean_tmin <- tenn1980 %>%
  group_by(julian_date, year) %>%
  filter(julian_date>31) %>%
  filter(julian_date<60) 

february_model <- lm(TMIN ~ julian_date + year, data = February_mean_tmin, na.action="na.fail")
summary(february_model)

March_mean_tmin <- tenn1980 %>%
  group_by(julian_date, year) %>%
  filter(julian_date>59) %>%
  filter(julian_date<91)

march_model <- lm(TMIN ~ julian_date + year, data = March_mean_tmin, na.action="na.fail")
summary(march_model)

April_mean_tmin <- tenn1980 %>%
   group_by(julian_date, year) %>%
  filter(julian_date>90) %>%
  filter(julian_date<121) 

april_model <- lm(TMIN ~ julian_date + year, data = April_mean_tmin, na.action="na.fail")
summary(april_model)
