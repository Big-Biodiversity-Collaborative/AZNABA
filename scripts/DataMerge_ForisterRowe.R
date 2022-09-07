#Merges NABA_Forister and NABA_Rowe data
#Ensure names are read in appropriately 
#Deletes duplicates 
#Adds lat/long
#9/01/2022

# Load additional packages
library(tidyverse)
library(lubridate)

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

#Adding climate data to the az_naba_lat_long
climate_az_naba <- left_join(az_naba_lat_long, all_sites_climate, by=c("Year"="year", "Month"="month", "Site"="site"))

#removing the duplicate lat/long from the climate file
climate_az_naba = select(climate_az_naba, -latitude,-longitude)







