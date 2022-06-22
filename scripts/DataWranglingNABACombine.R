# Data cleaning of western NABA to AZ NABA
# Kathleen L Prudic
# klprudic@arizona.edu
# created 2022-04-14

# Load additional packages
library(tidyverse)
library(lubridate)

# Load data 
matt_obs <- read_csv(file = "data/NABA-AZ-Matt.csv")
helen_obs <- read_csv(file = "data/NABA-AZ-Helen.csv")
#Bradly add here helenPost2018_obs <- 

# Drop data with no LatinName information
matt_obs <- matt_obs %>%
  filter(!is.na(LatinName))
helen_obs <- helen_obs %>%
  filter(!is.na(LatinName))


# Convert date to Date
matt_obs <- matt_obs %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))
helen_obs <- helen_obs %>%
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

# Drop rows not identified to species or subspecies 
# Rows with one word in LatinName
helen_obs <- helen_obs %>%
  filter(str_count(string = LatinName, pattern = " ") > 0)

# Make vector of SourceNames
source_names <- unique(c(matt_obs$LatinName, helen_obs$LatinName))

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

#Pull out unique latitude and longitude data
#Combine lat and long from 2 data sets
lat_long <- matt_obs %>%
  select(Latitude, Longitude, Site) %>%
  bind_rows(helen_obs %>% select(Latitude, Longitude, Site)) %>%
  distinct() 

# Write site latitude and longitude combined data to file
write_csv(x=lat_long, file = "data/lat-long-site.csv")

# Remove columns 
az_naba <- matt_obs %>%
  select(Year, Month, Day, DataSource, Site, AcceptedName, ButterflyCount, HoursObserved) %>%
  bind_rows(helen_obs %>% select(Year, Month, Day, DataSource, Site, 
                                 AcceptedName, ButterflyCount))
  
# Write to file
write_csv(x = az_naba, file = "data/all-sites-bflies.csv")






