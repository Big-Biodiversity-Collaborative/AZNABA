# Data Cleaning of Western NABA to AZ NABA
# Kathleen L Prudic
# klprudic@arizona.edu
# Created 2022-04-14
# Modified 2023-11-07 (Maxine Cruz)



# LOAD PACKAGES ---
library(tidyverse)
library(lubridate)



# LOAD DATA ---
matt_obs <- read_csv(file = "data/NABA-AZ-Matt.csv")
helen_obs <- read_csv(file = "data/NABA-AZ-Helen.csv")
helenPost2018_obs <- read_csv("data/NABA-AZ-Helenpost2018.csv")



# DATA CLEANING ---

# Drop data with no LatinName information
matt_obs <- matt_obs %>%
  filter(!is.na(LatinName))
helen_obs <- helen_obs %>%
  filter(!is.na(LatinName))
helenPost2018_obs <- helenPost2018_obs %>% 
  filter(!is.na(LatinName))

# Convert date to Date
matt_obs <- matt_obs %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))
helen_obs <- helen_obs %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))
helenPost2018_obs <- helenPost2018_obs %>% 
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))

# Convert Date to Year, Month, Day columns
matt_obs <- matt_obs %>%
  mutate(Year = year(Date), 
         Month = month(Date),
         Day = day(Date))
helen_obs <- helen_obs %>%
  mutate(Year = year(Date), 
         Month = month(Date),
         Day = day(Date))
helenPost2018_obs <- helenPost2018_obs %>% 
  mutate(Year = year(Date),
         Month = month(Date),
         Day = day(Date))

# Drop rows not identified to species or subspecies 
# Rows with one word in LatinName
helen_obs <- helen_obs %>%
  filter(str_count(string = LatinName, pattern = " ") > 0)
helenPost2018_obs <- helenPost2018_obs %>% 
  filter(str_count(string = LatinName, pattern = " ") > 0)

# Make vector of SourceNames
source_names <- unique(c(matt_obs$LatinName, helen_obs$LatinName, helenPost2018_obs$LatinName))

# Make dataframe with AcceptedNames
accepted_table <- data.frame(SourceName = source_names,
                             AcceptedName = NA)
accepted_table <- accepted_table %>% 
  separate(col = SourceName, 
           into = c("GenusName", "SpeciesName"),
           sep = " ",
           remove = FALSE, 
           extra = "drop") %>%
  mutate(AcceptedName = paste(GenusName, SpeciesName)) %>%
  select(-c(GenusName, SpeciesName)) %>%
  arrange(AcceptedName)

# Join AcceptedName to both Matt and Helen data
matt_obs <- matt_obs %>%
  left_join(accepted_table, by = c("LatinName" = "SourceName")) %>%
  mutate(DataSource = "Matt")
helen_obs <- helen_obs %>%
  left_join(accepted_table, by = c("LatinName" = "SourceName")) %>%
  mutate(DataSource = "Helen")
helenPost2018_obs <- helenPost2018_obs %>% 
  left_join(accepted_table, by = c("LatinName" = "SourceName")) %>%
  mutate(DataSource = "Helen")

# Pull out unique latitude and longitude data
# Combine lat and long from 3 data sets
lat_long <- matt_obs %>%
  select(Latitude, Longitude, Site) %>%
  bind_rows(helen_obs %>% select(Latitude, Longitude, Site)) %>%
  distinct() 



# [SAVE DATA] Site latitude and longitude combined data ---
write_csv(x=lat_long, file = "data/sites_lat_long.csv")



# Remove columns 
az_naba <- matt_obs %>%
  select(Year, Month, Day, DataSource, Site, AcceptedName, ButterflyCount, HoursObserved) %>%
  bind_rows(helen_obs %>% select(Year, Month, Day, DataSource, Site, AcceptedName, ButterflyCount)) %>% 
  bind_rows(helenPost2018_obs %>% select(Year, Month, Day, DataSource, Site, AcceptedName, ButterflyCount))
  


# [SAVE DATA] Butterfly Counts ---
write_csv(x = az_naba, file = "data/all_sites_bflies.csv")

# Make sure all is correct
view(az_naba)




