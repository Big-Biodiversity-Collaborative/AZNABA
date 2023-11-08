# ABOUT ---

# Runs initial tests with models



# LOAD LIBRARIES ---
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


# LOAD DATA ---
bfly_spring <- read.csv(file = "data/butterfly_analysis_spring.csv")
bfly_fall <-read.csv(file = "data/butterfly_analysis_fall.csv")



# CORRECT DATA ---

# Correcting the reporting mistake for the 2021 north rim sampling event of 126,
# party hours should be 26
bfly_fall["PartyHours"][bfly_fall["PartyHours"] == 126] <- 26

View(bfly_spring)

# Removing the grand canyon sampling events from the spring data set as requested
bfly_spring2 <- subset(bfly_spring, Site!= 'GrandCanyonDesertView')
bfly_spring2 <- subset(bfly_spring2, Site!= 'GrandCanyonSouthRim')

bfly_fall2 <- subset(bfly_fall, Site!= 'GrandCanyonNorthRim')
bfly_fall3 <- subset(bfly_fall, Site!= 'McDowellSonoranPreserve')

hist(bfly_spring$total_butterfly_count)
hist(log(bfly_spring$total_butterfly_count))
hist(bfly_fall$total_butterfly_count)
hist(log(bfly_fall$total_butterfly_count))



# INITIAL MODEL ASSESSMENTS ---

# Attempting to set up model - DOES NOT CONVERGE allows for random slope for each site as well! 
model = lmer(log(total_butterfly_count) ~ +tmean_previous30+year + (year|Site)  ,
             data=bfly_spring,
             REML = TRUE)
summary(model)

# WORKS ;) 
model_fall = lmer(log(total_butterfly_count) ~ +tmean_previous30+year + 
                    (1|Site)  + (1|year),
                  data=bfly_fall,
                  REML = TRUE)
summary(model_fall) 
ranef(model_fall)
plot(model_fall)

# Check out more variables need precip! ADD random ?+ (1|year)
# 90days in temp not significant

# Fall model for total butterflies
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

# Spring model for total butterflies
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

# ---
anova(model_spring1)
springran <- ranef(model_spring1)
dotplot(springran)

# Precip 365 not as good as Monsoon
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

# Does not converge 
model_fall = lmer(log(total_butterfly_count) ~ +tmean_previous30+year + 
                    (1|Site)  + (0+year|Site),
                  data=bfly_fall,
                  REML = TRUE)
summary(model_fall) 
ranef(model_fall)

# ---
model_spring = lmer(log(total_butterfly_count) ~ +tmean_previous30+year + 
                      (1|Site)  + (1|year),
                    data=bfly_spring,
                    REML = TRUE)
summary(model_spring) 
ranef(model_spring)
plot(model_spring)

# ---
model2 = lmer(total_butterfly_count ~ year + (1|Site),
              data=bfly_fall,
              REML = TRUE)
summary(model2)

# ---
model3 = lme(fixed=total_butterfly_count~year, data=bfly_fall, random= ~1|Site)
summary(model3)

model3

# ---
model4 = lme(fixed=total_butterfly_count~year tmin_previous30 + tmax_previous30, data=bfly_fall, random= ~1|Site)
summary(model4)
plot(model4)

# ---
model5 = lme(fixed=log(total_butterfly_count)~year+ tmean_previous30 + tmin_previous30 + tmax_previous30, data=bfly_fall, random= ~1|Site)
plot(model5)
summary(model5)

# ---
model6 = lme(fixed=log(total_butterfly_count)~year+ tmean_previous30 + tmin_previous30 + tmax_previous30 + PrecipSum_previous90, data=bfly_fall, random= ~1|Site) 
summary(model6)
plot(model6)

# ---
model7 = lme(log(total_butterfly_count)~year+ tmean_previous30 + tmin_previous30 + tmax_previous30 + PrecipSum_previous90 + (1|Site) + (1|year), data=bfly_fall) 

# ---
model8 = lme(fixed=log(total_butterfly_count)~year+ tmean_previous30 + tmin_previous30 + tmax_previous30 + PrecipSum_previous90, data=bfly_fall, random=list(~1|year, ~1|Site)) 
summary(model8)
plot(model8)

# ---
model9 = update(model8, correlation= corAR1())

plot(model9)
model9$varFix

cov2cor(vcov(model9))

# ---
hist(bfly_fall$Unique_butterflies)
hist(bfly_spring$Unique_butterflies)

# Spring model for unique butterflies
model_spring5 = lm(Unique_butterflies  ~ year + 
                     tmin_previous30 +
                     tmax_previous30 +
                     Mseason_precip +
                     Wseason_precip +
                     PartyHours,
                   data=bfly_spring)

summary(model_spring5) 
plot(model_spring5)

# Fall model for unique butterflies
model_fall5 = lmer(sqrt(Unique_butterflies) ~ + year + 
                     tmin_previous30 +
                     tmax_previous30+
                     Mseason_precip +
                     Wseason_precip +
                     PartyHours +
                     (1|Site),
                   data=bfly_fall,
                   REML = TRUE)

summary(model_fall5) 
plot(model_fall5)
vif(model_fall5)

# Creating spaghetti plots for total and unique butterflies over time for each site
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


# MODELS THAT ARE USED ---

# Fall model for total butterflies
model_fall1 = lmer(log(total_butterfly_count) ~ + year + 
                     tmin_previous30 +
                     tmax_previous30+
                     Mseason_precip +
                     Wseason_precip +
                     PartyHours +
                     (1|Site)  ,
                   data=bfly_fall,
                   REML = TRUE)

options(scipen=999)
summary(model_fall1)
ranef(model_fall1)
plot(model_fall1)
vif(model_fall1)
anova(model_fall1)

fallran <- ranef(model_fall1)
dotplot(fallran)

# Spring model for total butterflies
model_spring1 = lm(log(total_butterfly_count) ~ year + 
                     tmin_previous30 + 
                     tmax_previous30 +
                     Mseason_precip +
                     Wseason_precip +
                     PartyHours,
                   data=bfly_spring
)

summary(model_spring1) 

anova(model_spring1)
springran <- ranef(model_spring1)
dotplot(springran)

# Spring model for unique butterflies
model_spring5 = lm(Unique_butterflies ~ year + 
                     tmin_previous30 + 
                     tmax_previous30 +
                     Mseason_precip +
                     Wseason_precip +
                     PartyHours,
                   data=bfly_spring)

summary(model_spring5) 
plot(model_spring5)

# Fall model for unique butterflies
model_fall5 = lmer(sqrt(Unique_butterflies) ~ + year + 
                     tmin_previous30 + 
                     tmax_previous30 +
                     Mseason_precip +
                     Wseason_precip +
                     PartyHours +
                     (1|Site),
                   data=bfly_fall,
                   REML = TRUE)

summary(model_fall5) 
plot(model_fall5)
vif(model_fall5)

# Fall model for total butterflies without the two grand canyon north rim samples
model_fall12 = lmer(log(total_butterfly_count) ~ + year + 
                      tmin_previous30 + 
                      tmax_previous30 + 
                      Mseason_precip +
                      Wseason_precip +
                      PartyHours +
                      (1|Site),
                    data=bfly_fall2,
                    REML = TRUE)

summary(model_fall12) 
ranef(model_fall12)
plot(model_fall12)
vif(model_fall12)
anova(model_fall12)

fallran2 <- ranef(model_fall12)
dotplot(fallran2)

# Fall model for unique butterflies without the two grand canyon north rim samples
model_fall52 = lmer(sqrt(Unique_butterflies) ~ + year +
                      tmin_previous30 + 
                      tmax_previous30 +
                      Mseason_precip +
                      Wseason_precip +
                      PartyHours +
                      (1|Site),
                    data=bfly_fall2,
                    REML = TRUE)

summary(model_fall52) 
plot(model_fall52)
vif(model_fall52)



# CALCULATING COOK'S DISTANCE FOR THE 4 MODELS ---

# Cooks distance when grouped by site for fall abundance
fall_abun <- influence(model_fall1, group = "Site")
x <- cooks.distance(fall_abun)

# Cooks distance for each individual sampling for fall abundance
fall_abun2 <- influence(model_fall1, obs = TRUE)
x2 <- cooks.distance(fall_abun2)

# Cooks distance for each individual sample for spring abundance
spring_abun <- influence(model_spring1, obs = TRUE)
z <- cooks.distance(model_spring1)
ols_plot_cooksd_bar(model_spring1)

# Cooks distance when grouped by site for fall richness
fall_rich <- influence(model_fall5, group = "Site")
y <- cooks.distance(fall_rich)

# Cooks distance for each individual sampling for fall richness
fall_rich2 <- influence(model_fall5, obs = TRUE)
y2 <- cooks.distance(fall_rich2)

# Cooks distance for for each individual sample for spring richness
spring_rich <- influence(model_spring5, obs = TRUE)
z2 <- cooks.distance(model_spring5)
ols_plot_cooksd_bar(model_spring5)



# TESTING FALL ABUNDANCE WITHOUT MSP ---
model_fall13 = lmer(log(total_butterfly_count) ~ + year + 
                      tmin_previous30 + 
                      tmax_previous30 +
                      Mseason_precip +
                      Wseason_precip +
                      PartyHours +
                      (1|Site),
                    data=bfly_fall3,
                    REML = TRUE)

summary(model_fall13)

# ---
model_fall111 = lmer(log(total_butterfly_count) ~ + year + 
                       tmin_previous30 + 
                       tmax_previous30 +
                       Mseason_precip +
                       Wseason_precip +
                       PartyHours*year +
                       (1|Site),
                     data=bfly_fall,
                     REML = TRUE)

summary(model_fall111) 



# Creating data frames combining site abundance into a mean over the years for paper graphs
fallab <- bfly_fall %>% 
  select(year, total_butterfly_count) %>% 
  group_by(year) %>% 
  summarise(fall_avg_abundance = mean(total_butterfly_count))

springab <- bfly_spring %>% 
  select(year, total_butterfly_count) %>% 
  group_by(year) %>% 
  summarise(spring_avg_abundance = mean(total_butterfly_count))

# Creating data frames combining site richness into a mean over the years for paper graphs
fallri <- bfly_fall %>% 
  select(year, Unique_butterflies) %>% 
  group_by(year) %>% 
  summarise(fall_avg_richness = mean(Unique_butterflies))

springri <- bfly_spring %>% 
  select(year, Unique_butterflies) %>% 
  group_by(year) %>% 
  summarise(spring_avg_richness = mean(Unique_butterflies))

# Merging the abundance data frames 
total_ab <- left_join(fallab, springab, by="year")

# Merging the richness data frames
totalri <- left_join(fallri, springri, by="year")

# Creating long data frames for plotting
long_abun <- gather(total_ab, Group, myValue, -1)
long_rich <- gather(totalri, Group, myValue, -1)



# Graphing the average abundance of the years
ggplot(data=long_abun, aes(x=year, y=myValue, color=Group))+
  geom_point() +
  geom_smooth(method = "lm", se =TRUE) +
  labs(x="Year", y="Average butterfly abundance", title="") +
  scale_color_discrete(
    labels= c("fall_avg_abundance" = "Fall",
              "spring_avg_abundance" = "Spring"))

# Graphing the average  richness of the years
ggplot(data=long_rich, aes(x=year, y=myValue, color=Group))+
  geom_point() +
  geom_smooth(method = "lm", se =TRUE) +
  labs(x="Year", y="Average butterfly richness", title="") +
  scale_color_discrete(
    labels= c("fall_avg_richness" = "Fall",
              "spring_avg_richness" = "Spring"))



# Creating data frames for spring and fall dot plots showing abundance and richness for each site
falldot <- bfly_fall %>% 
  select(Site, total_butterfly_count, Unique_butterflies)

springdot <- bfly_spring %>% 
  select(Site, total_butterfly_count, Unique_butterflies)

# Renaming sites for easy reading
falldot$Site[falldot$Site=="McDowellSonoranPreserve"]<-"McDowell Sonoran Preserve"
falldot$Site[falldot$Site=="AtascosaHighlandsAZ"]<-"Atascosa Highlands"
falldot$Site[falldot$Site=="BoyceThompsonArboretum"]<-"Boyce Thompson Arboretum"
falldot$Site[falldot$Site=="GrandCanyonNorthRim"]<-"Grand Canyon North Rim"
falldot$Site[falldot$Site=="GrandCanyonSouthRim"]<-"Grand Canyon South Rim"
falldot$Site[falldot$Site=="PatagoniaAZ"]<-"Patagonia"
falldot$Site[falldot$Site=="PortalAZ"]<-"Portal"
falldot$Site[falldot$Site=="RamseyCanyonAZ"]<-"Ramsey Canyon"
falldot$Site[falldot$Site=="SabinoCanyonAZ"]<-"Sabino Canyon"
falldot$Site[falldot$Site=="SantaRitaMountains"]<-"Santa Rita Mountains"
falldot$Site[falldot$Site=="SycamoreCreekAZ"]<-"Sycamore Creek"

# Renaming spring sites for reading
springdot$Site[springdot$Site=="McDowellSonoranPreserve"]<-"McDowell Sonoran Preserve"
springdot$Site[springdot$Site=="GrandCanyonSouthRim"]<-"Grand Canyon South Rim"
springdot$Site[springdot$Site=="GrandCanyonDesertView"]<-"Grand Canyon Desert View"
springdot$Site[springdot$Site=="SabinoCanyonAZ"]<-"Sabino Canyon"

EnglishSites<- c("Cottonwood","McDowell Sonoran Preserve","Grand Canyon Desert View", "Grand Canyon South Rim","Sycamore Creek",
                 "Patagonia", "Ramsey Canyon","Boyce Thompson Arboretum","Grand Canyon North Rim", "Atacosa Highlands", 
                 "Sabino Canyon", "Portal","Santa Rita Mountains")



# CREATING ORGANIZED BOX PLOTS FOR FALL ---

# Fall abundance
f1 <- falldot %>% 
  mutate(Site = fct_relevel(
    Site,"Cottonwood","McDowell Sonoran Preserve", "Grand Canyon South Rim","Sycamore Creek",
    "Patagonia", "Ramsey Canyon","Boyce Thompson Arboretum","Grand Canyon North Rim", "Atascosa Highlands", 
    "Sabino Canyon", "Portal","Santa Rita Mountains" )) %>% 
  ggplot(aes(x=Site, y=total_butterfly_count))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(x="Site",y="Abundance")

# Fall richness
f2 <- falldot %>% 
  mutate(Site = fct_relevel(
    Site,"Cottonwood","McDowell Sonoran Preserve", "Grand Canyon South Rim","Sycamore Creek",
    "Patagonia", "Ramsey Canyon","Boyce Thompson Arboretum","Grand Canyon North Rim", "Atascosa Highlands", 
    "Sabino Canyon", "Portal","Santa Rita Mountains" )) %>% 
  ggplot(aes(x=Site, y=Unique_butterflies))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(x="Site",y="Richness")



# CREATING ORGANIZED BOX PLOTS FOR SPRING ---

# Spring abundance
s1 <- springdot %>% 
  mutate(Site = fct_relevel(
    Site,"McDowell Sonoran Preserve","Grand Canyon Desert View", "Grand Canyon South Rim",
    "Sabino Canyon" )) %>%
  ggplot(aes(x=Site, y=total_butterfly_count))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(x="Site",y="Abundance")

# Spring richness
s2 <- springdot %>% 
  mutate(Site = fct_relevel(
    Site,"McDowell Sonoran Preserve","Grand Canyon Desert View", "Grand Canyon South Rim",
    "Sabino Canyon" )) %>%
  ggplot(aes(x=Site, y=Unique_butterflies))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(x="Site",y="Richness")



# CREATING GRAPHS OF PARTY HOURS FOR EACH SITE ---
fallparty <- bfly_fall %>% 
  select(year, Site, PartyHours)

springparty <- bfly_spring %>% 
  select(year, Site, PartyHours)

fallparty$Site[fallparty$Site=="McDowellSonoranPreserve"]<-"McDowell Sonoran Preserve"
fallparty$Site[fallparty$Site=="AtascosaHighlandsAZ"]<-"Atascosa Highlands"
fallparty$Site[fallparty$Site=="BoyceThompsonArboretum"]<-"Boyce Thompson Arboretum"
fallparty$Site[fallparty$Site=="GrandCanyonNorthRim"]<-"Grand Canyon North Rim"
fallparty$Site[fallparty$Site=="GrandCanyonSouthRim"]<-"Grand Canyon South Rim"
fallparty$Site[fallparty$Site=="PatagoniaAZ"]<-"Patagonia"
fallparty$Site[fallparty$Site=="PortalAZ"]<-"Portal"
fallparty$Site[fallparty$Site=="RamseyCanyonAZ"]<-"Ramsey Canyon"
fallparty$Site[fallparty$Site=="SabinoCanyonAZ"]<-"Sabino Canyon"
fallparty$Site[fallparty$Site=="SantaRitaMountains"]<-"Santa Rita Mountains"
fallparty$Site[fallparty$Site=="SycamoreCreekAZ"]<-"Sycamore Creek"

springparty$Site[springparty$Site=="McDowellSonoranPreserve"]<-"McDowell Sonoran Preserve"
springparty$Site[springparty$Site=="GrandCanyonSouthRim"]<-"Grand Canyon South Rim"
springparty$Site[springparty$Site=="GrandCanyonDesertView"]<-"Grand Canyon Desert View"
springparty$Site[springparty$Site=="SabinoCanyonAZ"]<-"Sabino Canyon"

# Creating fall party hour box plot
fallpartyplot <- fallparty %>% 
  mutate(Site = fct_relevel(
    Site,"Cottonwood","McDowell Sonoran Preserve", "Grand Canyon South Rim","Sycamore Creek",
    "Patagonia", "Ramsey Canyon","Boyce Thompson Arboretum","Grand Canyon North Rim", "Atascosa Highlands", 
    "Sabino Canyon", "Portal","Santa Rita Mountains" )) %>% 
  ggplot(aes(x=Site, y=PartyHours))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(x="Site",y="Party hours")

# Creating spring party hour box plot
springpartyplot <- springparty %>% 
  mutate(Site = fct_relevel(
    Site,"McDowell Sonoran Preserve","Grand Canyon Desert View", "Grand Canyon South Rim",
    "Sabino Canyon" )) %>%
  ggplot(aes(x=Site, y=PartyHours))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(x="Site",y="Party hours")

# Fall abundance
ggplot(falldot, aes(x=Site, y=total_butterfly_count))+
  geom_dotplot(binaxis = 'y', binwidth = 125)+
  theme(axis.text.x = element_text(angle = 90))+
  labs(x="Site", y="Abundance")+
  ylim(0,11500)

# Fall richness
ggplot(falldot, aes(x=Site, y=Unique_butterflies))+
  geom_dotplot(binaxis = 'y', binwidth = 1)+
  theme(axis.text.x = element_text(angle = 90))+
  labs(x="Site", y="Richness")+
  ylim(0,105)

# Spring abundance
ggplot(springdot, aes(x=Site, y=total_butterfly_count))+
  geom_dotplot(binaxis = 'y', binwidth = 80)+
  theme(axis.text.x = element_text(angle = 90))+
  labs(x="Site",y="Abundance")+
  ylim(0,4000)

# Spring richness
ggplot(springdot, aes(x=Site, y=Unique_butterflies))+
  geom_dotplot(binaxis = 'y', binwidth = 1.5)+
  theme(axis.text.x = element_text(angle = 90))+
  labs(x="Site",y="Richness")+
  ylim(0,80)

# Fall abundance boxplot
ggplot(falldot, aes(x=Site, y=total_butterfly_count))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(x="Site",y="Abundance")

# Fall richness boxplot
ggplot(falldot, aes(x=Site, y=Unique_butterflies))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(x="Site",y="Richness")

# Spring abundance boxplot  
ggplot(springdot, aes(x=Site, y=total_butterfly_count))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(x="Site",y="Abundance")

# Spring richness boxplot
ggplot(springdot, aes(x=Site, y=Unique_butterflies))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(x="Site",y="Richness")



# Spring model for total butterflies with no GC
model_springGC = lm(log(total_butterfly_count) ~ year + 
                      tmin_previous30 + 
                      tmax_previous30 +
                      Mseason_precip +
                      Wseason_precip +
                      PartyHours,
                    data=bfly_spring2
)
summary(model_springGC) 

# Spring model for unique butterflies with no GC
model_springGC2 = lm(log(Unique_butterflies) ~ year + 
                       tmin_previous30 + 
                       tmax_previous30 +
                       Mseason_precip +
                       Wseason_precip +
                       PartyHours,
                     data=bfly_spring2
)

summary(model_springGC2) 




