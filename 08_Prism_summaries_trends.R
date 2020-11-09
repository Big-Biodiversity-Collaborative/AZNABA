library(tidyverse)
library(data.table)


#Read in extracted PRISM data
dat <- fread("prism_dat.csv")
dat1 <- dat

#Define season
dat1.win <- subset(dat1, month >= 12 | month <= 2)
dat1.win$seas <- "win"
dat1.spr <- subset(dat1, month >= 3 & month <= 5)
dat1.spr$seas <- "spr"
dat1.sum <- subset(dat1, month >= 6 & month <= 8)
dat1.sum$seas <- "sum"
dat1.fal <- subset(dat1, month >= 9 & month <= 11)
dat1.fal$seas <- "fal"

#Combine seasonal data 
dat1 <- rbind(dat1.win, dat1.spr, dat1.sum, dat1.fal)
dat1$new_var <- paste(dat1$var, dat1$seas, sep = "_")

#dat1 <- dat1 %>% filter(year >= 1990)

###############################################
#######  Summarise climate by sites  ##########
###############################################

#Seasonal summary
summ_dat1 <- dat1 %>% 
  group_by(long, lat, mypoints.id, new_var) %>% 
  summarise(value = mean(value)) %>% 
  spread(new_var, value)

#Annual summary
summ_dat2 <- dat1 %>% 
  group_by(long, lat, mypoints.id, var) %>% 
  summarise(value = mean(value)) %>% 
  spread(var, value)
colnames(summ_dat2)[4:6] <- c("ppt_ann", "tmax_ann", "tmin_ann")

#merge into one dataset
summ_dat1$ppt_ann <- summ_dat2$ppt_ann
summ_dat1$tmax_ann <- summ_dat2$tmax_ann
summ_dat1$tmin_ann <- summ_dat2$tmin_ann

#Save climate 
#write.csv(summ_dat1, "site_climates(1970).csv", row.names = F)

###############################################
#######  Summarise trends by sites  ###########
###############################################

#Seasonal summaries
seasonal_trends <- dat1 %>% 
  group_by(long, lat, mypoints.id, var, seas, year) %>%
  summarise(value = mean(value)) %>% 
  filter(mypoints.id != "Helena")

#Loop for seasonal trends (no Z)
sites <- unique(seasonal_trends$mypoints.id)
results.seas <- data.frame(site = NA, var = NA, season = NA, slope = NA)
for (i in 1:length(sites)) {
  temp1 <- seasonal_trends %>% filter(mypoints.id == sites[i])
  vars <- unique(temp1$var)
  for (j in 1:length(vars)) {
    temp2 <- temp1 %>% filter(var == vars[j])
    seas1 <- unique(temp2$seas)
    for (k in 1:length(seas1)) {
      temp3 <- temp2 %>% filter(seas == seas1[k])
      mod <- glm(value~year, data = temp3)
      results.seas1 <- data.frame(site = sites[i], var = vars[j], season = seas1[k], slope = coef(mod)[[2]])
      results.seas <- rbind(results.seas, results.seas1)
    }
  }
  print(i)
}
results.seas <- results.seas[-1,]

#Summ to annual trends. ann
annual_trends <- dat1 %>% 
  group_by(long, lat, mypoints.id, var, year) %>%
  summarise(value = mean(value)) %>% 
  filter(mypoints.id != "Helena")

#Loop for annual trends (no Z)
sites <- unique(annual_trends$mypoints.id)
results.ann <- data.frame(site = NA, var = NA, season = NA, slope = NA)
for (i in 1:length(sites)) {
  temp1 <- annual_trends %>% filter(mypoints.id == sites[i])
  vars <- unique(temp1$var)
  for (j in 1:length(vars)) {
    temp2 <- temp1 %>% filter(var == vars[j])
    mod <- glm(value~year, data = temp2)
    results.ann1 <- data.frame(site = sites[i], var = vars[j], season = "ann", slope = coef(mod)[[2]])
    results.ann <- rbind(results.ann, results.ann1)
  }
  print(i)
}
  
results.ann <- results.ann[-1,]

fin <- rbind(results.seas, results.ann)

fin1 <- fin %>% 
  mutate(var1 = paste(var, season, "slope", sep = "_")) %>% 
  select(-var, -season) %>% 
  spread(var1, slope)
colnames(fin1)[1] <- "mypoints.id"

#
write.csv(fin1, "site_trends(1970).csv", row.names = F) 

  