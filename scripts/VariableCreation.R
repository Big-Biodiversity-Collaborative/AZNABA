# Variable creation for analysis of NABA/Climate data
# Jennifer Broatch
# jennifer.broatch@asu.edu
# created 2022-06-07

# Load additional packages
library(tidyverse)

# Load data 
all_sites_bflies <- read_csv("data/all-sites-bflies.csv")
all_sites_climate <- read_csv("data/all-sites-climate.csv")

#View data- remove if not desired
View(all_sites_climate)
View(all_sites_bflies)

<<<<<<< HEAD
#TO DO - Merge files
x=2

#Brad Trial Comment 
=======
#Create temperature lag variables
#Data must be in time order (otherwise add arrange statement)
climate_lag <- all_sites_climate %>%                            
  group_by(site) %>%
  dplyr::mutate(tmin_previous = dplyr::lag(tmin, n = 1, default = NA)) %>% 
  dplyr::mutate(tmax_previous = dplyr::lag(tmax, n = 1, default = NA)) %>% 
  dplyr::mutate(tmean_previous = dplyr::lag(tmax, n = 1, default = NA)) %>% 
  dplyr::mutate(tprecip_previous = dplyr::lag(tmax, n = 1, default = NA)) %>% 
  as.data.frame()
View(climate_lag)     

#Winter = Dec, Jan, Feb
#Fall = Sept, Oct, Nov
#Spring = March, April, May
#Monsoon June July August 

count_bflies<- all_sites_bflies %>% arrange(Site, Year, Month, Day) 


#Pull out unique latitude and longitude data
#Combine lat and long from 2 data sets
lat_long <- matt_obs %>%
  select(Latitude, Longitude, Site) %>%
  bind_rows(helen_obs %>% select(Latitude, Longitude, Site)) %>%
  distinct() 
>>>>>>> 8ff2d6f8faac7377f7f255ee8e83b09820358b22
