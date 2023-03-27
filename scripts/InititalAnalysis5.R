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
library(lattice)
library(car)
library(lmerTest)
library(ggplot2)
library(ggbreak)
library(influence.ME)
library(olsrr)

#Reading in the DF
bfly_spring<- read.csv(file = "data/Spring_Analysis.csv")
bfly_fall<-read.csv(file = "data/fall_Analysis.csv")
View(bfly_spring)

#removing the grand canyon sampling events from the spring data set as requested
bfly_spring2 <- subset(bfly_spring, Site!= 'GrandCanyonDesertView')
bfly_spring2 <- subset(bfly_spring2, Site!= 'GrandCanyonSouthRim')

bfly_fall2 <- subset(bfly_fall, Site!= 'GrandCanyonNorthRim')
bfly_fall3 <- subset(bfly_fall, Site!= 'McDowellSonoranPreserve')


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


#Check out more variables need precip! ADD random ?+ (1|year)
#90days in temp not significant

#Fall model for total butterflies
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
anova(model_fall1)

fallran <- ranef(model_fall1)
dotplot(fallran)

#spring model for total butterflies
model_spring1 = lm(log(total_butterfly_count)  ~year
                   
                   + tmin_previous30 + 
                     
                     tmax_previous30+
                     Mseason_precip+
                     Wseason_precip +
                     PartyHours 
                       ,
                   data=bfly_spring
                   )
summary(model_spring1) 



anova(model_spring1)
springran <- ranef(model_spring1)
dotplot(springran)

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


hist(bfly_fall$Unique_butterflies)
hist(bfly_spring$Unique_butterflies)
#spring model for unique butterflies
model_spring5 = lm(Unique_butterflies  ~year
                   
                   + tmin_previous30 + 
                     
                     tmax_previous30+
                     Mseason_precip+
                     Wseason_precip +
                     PartyHours 
                   ,
                   data=bfly_spring)
summary(model_spring5) 
plot(model_spring5)
#fall model for unique butterflies
model_fall5 = lmer(sqrt(Unique_butterflies) ~ +year
                   
                   + tmin_previous30 + 
                     
                     tmax_previous30+
                     Mseason_precip+
                     Wseason_precip +
                     PartyHours +
                     (1|Site)  ,
                   data=bfly_fall,
                   REML = TRUE)
summary(model_fall5) 
plot(model_fall5)
vif(model_fall5)

#creating spaghetti plots for total and unique butterflies over time for each site
springcounts <- ggplot(bfly_spring, aes(x = year, y = total_butterfly_count, color = Site)) +
  geom_line() 
springcounts2 <- springcounts + xlab("Year") + ylab("Butterfly Count") + 
  ggtitle("Total Butterfly Counts for Each Site (Spring)")
springcounts2

fallcounts <- ggplot(bfly_fall, aes(x = year, y = total_butterfly_count, color = Site)) +
  geom_line() 
fallcounts2 <- fallcounts + xlab("Year") + ylab("Butterfly Count") + 
  ggtitle("Total Butterfly Counts for Each Site (Fall)")
fallcounts2

springunique <- ggplot(bfly_spring, aes(x = year, y = Unique_butterflies, color = Site)) +
  geom_line() 
springunique2 <- springunique + xlab("Year") + ylab("Unique Butterfly Species") + 
  ggtitle("Unique Butterfly Species for Each Site (Spring)")
springunique2

fallunique <- ggplot(bfly_fall, aes(x = year, y = Unique_butterflies, color = Site)) +
  geom_line() 
fallunique2 <- fallunique + xlab("Year") + ylab("Unique Butterfly Species") + 
  ggtitle("Unique Butterfly Species for Each Site (Fall)")
fallunique2


#The models used

#Fall model for total butterflies
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
anova(model_fall1)

fallran <- ranef(model_fall1)
dotplot(fallran)

#spring model for total butterflies
model_spring1 = lm(log(total_butterfly_count)  ~year
                   
                   + tmin_previous30 + 
                     
                     tmax_previous30+
                     Mseason_precip+
                     Wseason_precip +
                     PartyHours 
                   ,
                   data=bfly_spring
)
summary(model_spring1) 



anova(model_spring1)
springran <- ranef(model_spring1)
dotplot(springran)

#spring model for unique butterflies
model_spring5 = lm(Unique_butterflies  ~year
                   
                   + tmin_previous30 + 
                     
                     tmax_previous30+
                     Mseason_precip+
                     Wseason_precip +
                     PartyHours 
                   ,
                   data=bfly_spring)
summary(model_spring5) 
plot(model_spring5)

#fall model for unique butterflies
model_fall5 = lmer(sqrt(Unique_butterflies) ~ +year
                   
                   + tmin_previous30 + 
                     
                     tmax_previous30+
                     Mseason_precip+
                     Wseason_precip +
                     PartyHours +
                     (1|Site)  ,
                   data=bfly_fall,
                   REML = TRUE)
summary(model_fall5) 
plot(model_fall5)
vif(model_fall5)

#Fall model for total butterflies without the two grand canyon north rim samples
model_fall12 = lmer(log(total_butterfly_count) ~ +year
                   
                   + tmin_previous30 + 
                     
                     tmax_previous30+
                     Mseason_precip+
                     Wseason_precip +
                     PartyHours +
                     (1|Site)  ,
                   data=bfly_fall2,
                   REML = TRUE)
summary(model_fall12) 
ranef(model_fall12)
plot(model_fall12)
vif(model_fall12)
anova(model_fall12)

fallran2 <- ranef(model_fall12)
dotplot(fallran2)

#fall model for unique butterflies without the two grand canyon north rim samples
model_fall52 = lmer(sqrt(Unique_butterflies) ~ +year
                   
                   + tmin_previous30 + 
                     
                     tmax_previous30+
                     Mseason_precip+
                     Wseason_precip +
                     PartyHours +
                     (1|Site)  ,
                   data=bfly_fall2,
                   REML = TRUE)
summary(model_fall52) 
plot(model_fall52)
vif(model_fall52)



#Calculating cooks disctance for the 4 models

#cooks distance when grouped by site for fall abundance
fall_abun <- influence(model_fall1, group = "Site")
x <- cooks.distance(fall_abun)

#cooks distance for each individual sampling for fall abundance
fall_abun2 <- influence(model_fall1, obs = TRUE)
x2 <- cooks.distance(fall_abun2)

#cooks distance for each individual sample for spring abundance
spring_abun <- influence(model_spring1, obs = TRUE)
z <- cooks.distance(model_spring1)
ols_plot_cooksd_bar(model_spring1)

#cooks distance when grouped by site for fall richness
fall_rich <- influence(model_fall5, group = "Site")
y <- cooks.distance(fall_rich)

#cooks distance for each individual sampling for fall richness
fall_rich2 <- influence(model_fall5, obs = TRUE)
y2 <- cooks.distance(fall_rich2)

#cooks distance for for each individual sample for spring richness
spring_rich <- influence(model_spring5, obs = TRUE)
z2 <- cooks.distance(model_spring5)
ols_plot_cooksd_bar(model_spring5)


#testing fall abundance without MSP
model_fall13 = lmer(log(total_butterfly_count) ~ +year
                   
                   + tmin_previous30 + 
                     
                     tmax_previous30+
                     Mseason_precip+
                     Wseason_precip +
                     PartyHours +
                     (1|Site)  ,
                   data=bfly_fall3,
                   REML = TRUE)
summary(model_fall13)






