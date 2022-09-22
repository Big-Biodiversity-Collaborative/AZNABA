#Merges NABA_Forister and NABA_Rowe data
#Ensure names are read in appropriately 
#Deletes duplicates 
#Adds lat/long
#9/01/2022

# Load additional packages
library(tidyverse)
library(lubridate)
library(dplyr)
library(zoo)
library(ggplot2)

# Load data and Corrects data read error with '
matt <- read_csv(file = "data/NABA_Forister.csv")
matt$NABAEnglishName <- gsub("^'|'$", "", matt$NABAEnglishName)

helen <- read_csv(file = "data/NABA_Rowe.csv")
helen$NABAEnglishName <- gsub("^'|'$", "", helen$NABAEnglishName)
helen$NABAEnglishName <- gsub("[\x82\x91\x92]", "'", helen$NABAEnglishName)

#Renaming site names in Helen's file to be consistent with Matt and lat long files
helen$Site[helen$Site=="McDowell Sonoran Preserve"]<-"McDowellSonoranPreserve"
helen$Site[helen$Site=="Grand Canyon North Rim"]<-"GrandCanyonNorthRim"
helen$Site[helen$Site=="Patagonia"]<-"PatagoniaAZ"
helen$Site[helen$Site=="Portal"]<-"PortalAZ"
helen$Site[helen$Site=="Ramsey Canyon"]<-"RamseyCanyonAZ"
helen$Site[helen$Site=="Sabino Canyon"]<-"SabinoCanyonAZ"
helen$Site[helen$Site=="Santa Rita Mountains"]<-"SantaRitaMountains"


# Rename variables for consistent naming
matt <- matt%>%
  rename(PartyHours=Party_Hours) 

# Combine files  
az_naba <- matt %>%
  bind_rows(helen) 
 
#Delete duplicate rows
Az_naba_all <- az_naba %>%
  distinct() 

#BRADLY: Add lat/long to Az_naba_all
#Check the names in Helen's file ' 
#??Urbanization get file from Helen

#Importing in the climate and lat/long data files
lat_long_site <- read_csv(file = "data/lat-long-site.csv")
all_sites_climate <- read_csv("data/all-sites-climate.csv")

#Adding Lat/Long to the az_naba_all file
az_naba_lat_long <- left_join(Az_naba_all, lat_long_site, by ="Site")

#Creating a csv for the NABA data with the lat longs added
write_csv(x = az_naba_lat_long, 
          file = "data/AZNABA_lat_long.csv")

#Adding climate data to the az_naba_lat_long
climate_az_naba <- left_join(az_naba_lat_long, all_sites_climate, by=c("Year"="year", "Month"="month", "Site"="site"))

#removing the duplicate lat/long from the climate file
climate_az_naba = select(climate_az_naba, -latitude,-longitude)

#Creating a data file with Total unique butterfly species for each outing 
Total_butterfly2 <- Az_naba_all %>% 
  select(Year, Month, Day, Site, NABAEnglishName) %>% 
  group_by(Year, Month, Day, Site) %>% 
  summarize(Unique_butterflies = n_distinct(NABAEnglishName))

#Creating a data file with Total butterfly count for each outing 
Total_butterfly <- Az_naba_all %>% 
  select(Year, Month, Day, Site, ButterflyCount) %>% 
  group_by(Year, Month, Day, Site) %>% 
  summarize(total_butterly_count = sum(ButterflyCount)) 

#merging the two files with species count and total number of butterflies
Butterfly_summary <- left_join(Total_butterfly, Total_butterfly2, by=c("Year"="Year", "Month"="Month","Day"="Day", "Site"="Site"))

#Creating a csv with the butterfly richness and abundance for each outing
write_csv(x = Butterfly_summary, 
          file = "data/Butterfly_summary.csv")

#Creating a climate lag 
climate_lag <- all_sites_climate %>% 
  group_by(site) %>% 
  dplyr::mutate(tmean_previous = dplyr::lag(tmean, n = 1, default = NA))

#combining the Climate lag df with the butterfly_summary df
Butterfly_summary_lag <- left_join(Butterfly_summary, climate_lag, by=c("Year"="year", "Month"="month", "Site"="site"))

#Creating a new daily weather file 
daily_weather <- NULL
read_daily<- 
  function(path, pattern = "*.csv") {
list.files(path = "data/Climate-Data/DAILY", pattern = ".csv", full.names = TRUE) %>% 
      map_df(~read.csv(., skip=10))
      }

daily_weather <- read_daily('./data/Climate-Data/DAILY')

#Separating the date into year, month, day format
daily_weather <- daily_weather %>%
  mutate(year = as.integer(substr(x = Date, start = 1, stop = 4)),
         month = as.integer(substr(x = Date, start = 6, stop = 7)),
         day = as.integer(substr(x = Date, start = 9, stop = 10))) %>%
  select(-Date)

#Moving the date to the front of the data frame 
daily_weather <- daily_weather %>%
  relocate(year, month, day, Name, Latitude, Longitude, Elevation..m., ppt..mm.,tmin..degrees.C.,
           tmean..degrees.C.,tmax..degrees.C.,tdmean..degrees.C.,vpdmin..hPa.,vpdmax..hPa.)

#Renaming column names
daily_weather <- daily_weather %>% 
  rename(Elevation  = 'Elevation..m.',
         Precip = 'ppt..mm.',
         tmin = 'tmin..degrees.C.',
         tmean ='tmean..degrees.C.',
         tmax = 'tmax..degrees.C.',
         tdmean ='tdmean..degrees.C.',
         vpdmin ='vpdmin..hPa.',
         vpdmax ='vpdmax..hPa.',
         Site = 'Name'
  )

#Renaming the sites to match with the butterfly data site names
daily_weather$Site[daily_weather$Site=="AtascosaHigh"]<-"AtascosaHighlandsAZ"
daily_weather$Site[daily_weather$Site=="RamseyCanyon"]<-"RamseyCanyonAZ"
daily_weather$Site[daily_weather$Site=="SabinoCanyon"]<-"SabinoCanyonAZ"
daily_weather$Site[daily_weather$Site=="SantaRitaMou"]<-"SantaRitaMountains"
daily_weather$Site[daily_weather$Site=="SycamoreCree"]<-"SycamoreCreekAZ"
daily_weather$Site[daily_weather$Site=="BoyceThompso"]<-"BoyceThompsonArboretum"
daily_weather$Site[daily_weather$Site=="GrandCanyonD"]<-"GrandCanyonDesertView"
daily_weather$Site[daily_weather$Site=="GrandCanyonS"]<-"GrandCanyonSouthRim"
daily_weather$Site[daily_weather$Site=="GuadalupeCan"]<-"GuadalupeCanyonAZNM"
daily_weather$Site[daily_weather$Site=="Springervill"]<-"SpringervilleAZ"
daily_weather$Site[daily_weather$Site=="McDowellSono"]<-"McDowellSonoranPreserve"
daily_weather$Site[daily_weather$Site=="GrandCanyonN"]<-"GrandCanyonNorthRim"

#Creating a csv file for the daily weather data frame 
write_csv(x = daily_weather, 
          file = "data/daily_weather.csv")


#Reading in the daily weather and butterfly summary csv
daily_weather <- read_csv("data/daily_weather.csv")
Butterfly_summary <- read_csv("data/Butterfly_summary.csv")

#Create a new data frame with all the rows from daily_weather and Butterfly_Summary
Butterly_daily_weather <- full_join(daily_weather, Butterfly_summary,
                                by =c("year"="Year", "month"="Month", "day"="Day", "Site"="Site"))

#adding the previous 30, 90, and 365 day high/low/mean temp, and adding sum of the last 30/90/365 day precipitation 
Final_Butterly<- Butterly_daily_weather %>% 
  group_by(Site) %>% 
  arrange(Site) %>%
  mutate(tmean_previous30=rollmean(tmean,30, na.pad = TRUE, align = "right")) %>% 
  mutate(tmax_previous30=rollmax(tmax,30, na.pad = TRUE, align = "right")) %>% 
  mutate(tmin_previous30=-rollmax(-tmin,30, na.pad = TRUE, align = "right")) %>%
  mutate(PrecipSum_previous30=rollsum(Precip,30, na.pad = TRUE, align = "right")) %>% 
  mutate(tmean_previous90=rollmean(tmean,90, na.pad = TRUE, align = "right")) %>% 
  mutate(tmax_previous90=rollmax(tmax,90, na.pad = TRUE, align = "right")) %>% 
  mutate(tmin_previous90=-rollmax(-tmin,90, na.pad = TRUE, align = "right")) %>%
  mutate(tmean_previous365=rollmean(tmean,365, na.pad = TRUE, align = "right")) %>% 
  mutate(tmax_previous365=rollmax(tmax,365, na.pad = TRUE, align = "right")) %>% 
  mutate(tmin_previous365=-rollmax(-tmin,365, na.pad = TRUE, align = "right")) %>%
  mutate(PrecipSum_previous90=rollsum(Precip,90, na.pad = TRUE, align = "right")) %>% 
  mutate(PrecipSum_previous365=rollsum(Precip,365, na.pad = TRUE, align = "right"))
  
#Adding # of days over 28 and 30 
rollapply(Final_Butterly, 30, )


#Creating winter precip data
winter_precip <- Final_Butterly %>% 
  select(year, month, day, Site, Precip) %>% 
  group_by(Site, year, month) %>% 
  summarise(monthly_precip = sum(Precip))

#deleting months that are not in winter season
winter_precip<- subset(winter_precip, month!="5" & month!="6" & month!="7" & month!="8" & month!="9")

#combining the winter months into a season
winter_precip<- winter_precip %>% 
  mutate(PrecipSum_previous7=rollsum(monthly_precip,7, na.pad = TRUE, align = "right"))
  
#creating winter months of 10-12
winter_precip_firsthalf <- subset(
  winter_precip, month!="5" & month!="6" & month!="7" & month!="8" & month!="9" & month!="1" & month!="2"
  & month!="3" & month!="4")

#combining months 10-12
winter_precip_firsthalf<- winter_precip_firsthalf %>% 
  mutate(PrecipSum_previous3=rollsum(monthly_precip,3, na.pad = TRUE, align = "right"))

#adding 1 to each year to align with the second half of winter season
winter_precip_firsthalf$year<- winter_precip_firsthalf$year +1

#creating winter months of 1-4
winter_precip_secondhalf<- subset(
  winter_precip, month!="5" & month!="6" & month!="7" & month!="8" & month!="9" & month!="10" & month!="11"
  & month!="12")

#combining months 1-4
winter_precip_secondhalf<- winter_precip_secondhalf %>% 
  mutate(PrecipSum_previous4=rollsum(monthly_precip,4, na.pad = TRUE, align = "right"))

#joining the two winter halves
Wseason_precip<- merge(x=winter_precip_firsthalf, y=winter_precip_secondhalf, 
                       by=c( "Site", "year", "month", "monthly_precip"), all = TRUE)

#replacing NAs with 0 so rows can be added
Wseason_precip[is.na(Wseason_precip)]<-0

#adding the two rows
Wseason_precip$Precip_total<- Wseason_precip$PrecipSum_previous3 + Wseason_precip$PrecipSum_previous4

#removing unneeded columns
Wseason_precip <- subset(Wseason_precip, select = -c(PrecipSum_previous3, PrecipSum_previous4))

#combing the two halves into 1 season precip
total_season_precip <- Wseason_precip %>% 
  select(Site, year, Precip_total) %>% 
  group_by(Site, year) %>% 
  summarise(season_precip = sum(Precip_total))

#count_above30 = sum(tmax>30)

#Creating monsoon precip data
monsoon_precip <- Final_Butterly %>% 
  select(year, month, day, Site, Precip) %>% 
  group_by(Site, year, month) %>% 
  summarise(monthly_precip = sum(Precip))

#Deleting months that are not in monsoon season
monsoon_precip<- subset(monsoon_precip, month!="1" & month!="2" & month!="3" & month!="4" & month!="5" & 
                          month!="6" & month!="10" & month!="11" & month!="12" )

#Adding the monsoon season months up for the year/site
monsoon_precip <- monsoon_precip %>% 
  select(Site, year, monthly_precip) %>% 
  group_by(Site, year) %>% 
  summarise(Mseason_precip = sum(monthly_precip))

#Creating the previous year monsoon precip
monsoon_precip<- monsoon_precip %>% 
  dplyr::mutate(previous_Mseason_precip = dplyr::lag(Mseason_precip, n = 1, default = NA))

#Creating monsoon temperature data
monsoon_temp <- Final_Butterly %>% 
  select(year, month, day, Site, tmin, tmean, tmax) %>% 
  group_by(Site, year, month) %>% 
  summarise(monthly_tmean = mean(tmean), monthly_tmin = min(tmin), monthly_tmax = max(tmax))

#removing months not in monsoon season
monsoon_temp<- subset(monsoon_temp, month!="1" & month!="2" & month!="3" & month!="4" & month!="5" & 
                          month!="6" & month!="10" & month!="11" & month!="12" )

#combing all the months for one monsoon season
monsoon_temp<- monsoon_temp %>% 
  select(year, month, Site, monthly_tmean, monthly_tmin, monthly_tmax) %>% 
  group_by(Site, year) %>% 
  summarise(Mseason_tmean = mean(monthly_tmean), Mseason_tmin = min(monthly_tmin), Mseason_tmax = max(monthly_tmax))

#combining the monsoon temp and precip data
monsoon_all <- merge(x=monsoon_temp, y=monsoon_precip, by=c("Site", "year"), all = TRUE)

#writing the monsoon data to csv
write_csv(x = monsoon_all, 
          file = "data/monsoon_all.csv")









#Removing all of the rows that do not contain a butterfly count
Final_Butterly %>% drop_na(Unique_butterflies)


#Counting the number of samples per month
sample_months <-Butterfly_summary %>% 
  count(Month)

#renaming n to samples
sample_months <-sample_months %>% 
  rename(Sample_Number = n)

#Creating a bar plot
month_count <- sample_months$Sample_Number
names(month_count)<- sample_months$Month
barplot(month_count, 
        names.arg = c("March", "April", "May", "June", "July", "August", "September", "October"),
        las=2,
        main = "Number of Butterfly Samples for Each Month",
        xlab = "Month",
        ylab = "Number of Samples",
        ylim = c(0,100))
