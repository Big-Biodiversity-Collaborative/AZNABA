# ABOUT ---

# Generates the following files:
  # butterfly_analysis_spring.csv
  # butterfly_analysis_fall.csv



# LOAD LIBRARIES ---
library(tidyverse)
library(lubridate)
library(dplyr)
library(zoo)
library(lme4)
library(glme)
library(DescTools)
library(nlme)



# LOAD DATA ---
bfly_analysis <- read_csv(file = "data/butterfly_analysis.csv")



# SEPARATE DATA INTO FALL AND SPRING DATA ---

# Removing butterfly count and renaming butterly to butterfly
bfly_analysis <- subset(bfly_analysis, select = -c(ButterflyCount))
names(bfly_analysis)[names(bfly_analysis) == "total_butterly_count"] <- "total_butterfly_count"

# Rewriting the df
write_csv(x = bfly_analysis, 
          file = "data/butterfly_analysis.csv")

# Creating the fall DF
bfly_fall <- bfly_analysis %>% 
  select(year:TotalDistanceMi) %>% 
  filter(month > 6)

# Removing second sampling of santarita mountains
bfly_fall <- bfly_fall[!(bfly_fall$Site == 'SantaRitaMountains' & bfly_fall$month == 9),]
bfly_fall <- bfly_fall[!(bfly_fall$Site == 'SantaRitaMountains' & bfly_fall$month == 10),]

# Removing pre 7/15 sampling
bfly_fall <- bfly_fall[!(bfly_fall$Site == 'GrandCanyonNorthRim' & bfly_fall$day == 5),]
bfly_fall <- bfly_fall[!(bfly_fall$Site == 'SycamoreCreekAZ' & bfly_fall$day == 7),]

# Creating the spring data observations
bfly_spring <- bfly_analysis %>% 
  select(year:TotalDistanceMi) %>% 
  filter(month < 7)

# Removing post 6/15 sampling  
bfly_spring <- bfly_spring[!(bfly_spring$Site == 'RamseyCanyonAZ' & bfly_spring$month == 6),]
bfly_spring <- bfly_spring[!(bfly_spring$Site == 'AtascosaHighlandsAZ' & bfly_spring$month == 6),]

# Save files
write_csv(x = bfly_spring, 
          file = "data/butterfly_analysis_spring.csv")

write_csv(x = bfly_fall, 
          file = "data/butterfly_analysis_fall.csv")



# GENERATE MODELS ---

#Attempting to set up a basic model 
model = lmer(total_butterfly_count ~ year + (1|Site),
             data=bfly_spring,
             REML = TRUE)
summary(model)

model2 = lmer(total_butterfly_count ~ year + (1|Site),
             data=bfly_fall,
             REML = TRUE)
summary(model2)



model3 = lme(fixed=total_butterfly_count~year, data=bfly_fall, random= ~1|Site)
summary(model3)

model3

model4 = lme(fixed=total_butterfly_count~year+ tmean_previous30 + tmin_previous30 + tmax_previous30, data=bfly_fall, random= ~1|Site)
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
