#reading in necessary packages
library(tidyverse)
library(lubridate)
library(dplyr)
library(zoo)
library(lme4)
library(glme)
library(DescTools)
library(nlme)
library(readr)

#Reading in the DF
bfly_spring<- read.csv(file = "data/Spring_Analysis.csv")
bfly_fall<-read.csv(file = "data/fall_Analysis.csv")
View(bfly_spring)

hist(bfly_spring$total_butterfly_count)
hist(log(bfly_spring$total_butterfly_count))
hist(bfly_fall$total_butterfly_count)
hist(log(bfly_fall$total_butterfly_count))


#Attempting to set up model - DOES NOT CONVERGE allows for random slope for each site as well! 
model = lmer(log(total_butterfly_count) ~ +tmean_previous30+year + (year|Site)  ,
             data=bfly_spring,
             REML = TRUE)
summary(model)

#WORKS ;) 
model_fall = lmer(log(total_butterfly_count) ~ +tmean_previous30+year + 
                      (1|Site)  + (1|year),
                    data=bfly_fall,
                    REML = TRUE)
summary(model_fall) 
ranef(model_fall)
plot(model_fall)
library(car)

#Check out more variables need precip! ADD random ?+ (1|year)
#90days in temp not significant

model_fall1 = lmer(log(total_butterfly_count) ~ +year
                    
                    + tmin_previous30 + 
                    
                    tmax_previous30+
                    Mseason_precip+
                     Wseason_precip +
                     PartyHours +
                    (1|Site)  ,
                  data=bfly_fall,
                  REML = TRUE)
summary(model_fall1) 
ranef(model_fall1)
plot(model_fall1)
vif(model_fall1)

#spring model
model_spring1 = lmer(log(total_butterfly_count) ~ +year
                   
                   + tmin_previous30 + 
                     
                     tmax_previous30+
                     Mseason_precip+
                     Wseason_precip +
                     PartyHours +
                     (1|Site)  ,
                   data=bfly_spring,
                   REML = TRUE)
summary(model_spring1) 
ranef(model_spring1)
plot(model_spring1)
vif(model_spring1)

#test model 
#precip 365 not as good as Monsoon
model_fall2 = lmer(log(total_butterfly_count) ~ +year
                   
                   + tmin_previous30 + 
                     
                     tmax_previous30+
                     PrecipSum_previous365+
                     (1|Site)  ,
                   data=bfly_fall,
                   REML = TRUE)
summary(model_fall2) 
ranef(model_fall2)
plot(model_fall2)
vif(model_fall2)

#does not converge 
model_fall = lmer(log(total_butterfly_count) ~ +tmean_previous30+year + 
                    (1|Site)  + (0+year|Site),
                  data=bfly_fall,
                  REML = TRUE)
summary(model_fall) 
ranef(model_fall)


model_spring = lmer(log(total_butterfly_count) ~ +tmean_previous30+year + 
               (1|Site)  + (1|year),
             data=bfly_spring,
             REML = TRUE)
summary(model_spring) 
ranef(model_spring)
plot(model_spring)



model2 = lmer(total_butterfly_count ~ year + (1|Site),
             data=bfly_fall,
             REML = TRUE)
summary(model2)



model3 = lme(fixed=total_butterfly_count~year, data=bfly_fall, random= ~1|Site)
summary(model3)

model3

model4 = lme(fixed=total_butterfly_count~year tmin_previous30 + tmax_previous30, data=bfly_fall, random= ~1|Site)
summary(model4)
plot(model4)

model5 = lme(fixed=log(total_butterfly_count)~year+ tmean_previous30 + tmin_previous30 + tmax_previous30, data=bfly_fall, random= ~1|Site)
plot(model5)
summary(model5)

model6 = lme(fixed=log(total_butterfly_count)~year+ tmean_previous30 + tmin_previous30 + tmax_previous30 + PrecipSum_previous90, data=bfly_fall, random= ~1|Site) 
summary(model6)
plot(model6)

model7 = lme(log(total_butterfly_count)~year+ tmean_previous30 + tmin_previous30 + tmax_previous30 + PrecipSum_previous90 + (1|Site) + (1|year), data=bfly_fall) 

model8 = lme(fixed=log(total_butterfly_count)~year+ tmean_previous30 + tmin_previous30 + tmax_previous30 + PrecipSum_previous90, data=bfly_fall, random=list(~1|year, ~1|Site)) 
summary(model8)
plot(model8)

model9 = update(model8, correlation= corAR1())

plot(model9)
model9$varFix

cov2cor(vcov(model9))
