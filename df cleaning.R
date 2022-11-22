#reading in necessary packages
library(tidyverse)
library(lubridate)
library(dplyr)
library(zoo)
library(lme4)
library(glme)
library(DescTools)

#Reading in the DF
bfly_analysis <- read_csv(file = "data/Butterfly_Analysis.csv")

#Removing butterfly count and renaming butterly to butterfly
bfly_analysis <- subset(bfly_analysis, select = -c(ButterflyCount))
names(bfly_analysis)[names(bfly_analysis) == "total_butterly_count"] <- "total_butterfly_count"

#Rewriting the df
write_csv(x = bfly_analysis, 
          file = "data/Butterfly_Analysis.csv")

#Creating the fall DF
bfly_fall <- bfly_analysis %>% 
  select(year:TotalDistanceMi) %>% 
  filter(month > 6)

#Removing second sampling of santarita mountains
bfly_fall <- bfly_fall[!(bfly_fall$Site == 'SantaRitaMountains' & bfly_fall$month == 9),]
bfly_fall <- bfly_fall[!(bfly_fall$Site == 'SantaRitaMountains' & bfly_fall$month == 10),]

#removing pre 7/15 sampling
bfly_fall <- bfly_fall[!(bfly_fall$Site == 'GrandCanyonNorthRim' & bfly_fall$day == 5),]
bfly_fall <- bfly_fall[!(bfly_fall$Site == 'SycamoreCreekAZ' & bfly_fall$day == 7),]

#Creating the fall data observations
bfly_spring <- bfly_analysis %>% 
  select(year:TotalDistanceMi) %>% 
  filter(month < 7)

#Removing post 6/15 sampling  
bfly_spring <- bfly_spring[!(bfly_spring$Site == 'RamseyCanyonAZ' & bfly_spring$month == 6),]
bfly_spring <- bfly_spring[!(bfly_spring$Site == 'AtascosaHighlandsAZ' & bfly_spring$month == 6),]

#Writing sring/fall df's to files
write_csv(x = bfly_spring, 
          file = "data/Spring_Analysis.csv")
write_csv(x = bfly_fall, 
          file = "data/Fall_Analysis.csv")





#Attempting to set up a basic model 
model = lmer(total_butterfly_count ~ year + (1|Site),
             data=bfly_spring,
             REML = TRUE)
summary(model)

model2 = lmer(total_butterfly_count ~ year + (1|Site),
             data=bfly_fall,
             REML = TRUE)
summary(model2)







