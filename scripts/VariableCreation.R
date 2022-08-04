# Variable creation for analysis of NABA/Climate data
# Jennifer Broatch
# jennifer.broatch@asu.edu
# created 2022-06-07

# Load additional packages
library(tidyverse)

# Load data 
all_sites_bflies <- read_csv("data/all-sites-bflies14-21Helen.csv")
all_sites_climate <- read_csv("data/all-sites-climate.csv")

View(all_sites_bflies) 
View(all_sites_climate)


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


#The observations are off by 1 observation??  11572 distinct and 11573 in Helen. 
#Need to investigate and fix. 

count_bflies<- all_sites_bflies %>% arrange(Site, Year, Month, Day, AcceptedName)  %>%
 filter(DataSource=="Helen")

View(count_bflies)


#Pull out unique counts ??Hours Observed not in helen
distinct_bflies <- count_bflies %>%
  select(Year, Month, Day, Site, AcceptedName, ButterflyCount) %>%
  distinct() 

View(distinct_bflies)
