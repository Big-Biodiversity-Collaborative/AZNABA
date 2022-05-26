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

# Drop data with no LatinName information
matt_obs <- matt_obs %>%
  filter(!is.na(LatinName))
helen_obs <- helen_obs %>%
  filter(!is.na(LatinName))

#Convert date to Date
matt_obs <- matt_obs %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))
helen_obs <- helen_obs %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))

# Drop rows not identified to species or subspecies 
# Rows with one word in LatinName
helen_obs <- helen_obs %>%
  filter(str_count(string = LatinName, pattern = " ") > 0)

# Make vector of SourceNames
source_names <- unique(c(matt_obs$LatinName, helen_obs$LatinName))

# Make dataframe with AcceptedNames
accepted_table <- data.frame(SourceName = source_names,
                             AcceptedName = NA)
accepted_table %>% 
  separate(col = SourceName, 
           into = c("GenusName", "SpeciesName"),
           sep = " ",
           remove = FALSE, 
           extra = "drop") %>%
  mutate(AcceptedName = paste(GenusName, SpeciesName)) %>%
  select(-c(GenusName, SpeciesName)) %>%
  arrange(AcceptedName)

#Pull out unique latitude and longitude data
#Combine lat and long from 2 data sets
lat_long <- matt_obs %>%
  select(Latitude, Longitude, Site) %>%
  bind_rows(helen_obs %>% select(Latitude, Longitude, Site)) %>%
  distinct() 
lat_long
write_csv(x=lat_long, file = "output/Lat-Long-Site.csv")

#Identify year range
obs_date <- matt_obs %>%
  select(Date) %>%
  mutate(Source = "matt") %>%
  bind_rows(helen_obs %>% select(Date) %>% mutate(Source = "helen")) %>%
  mutate(Year = year(Date)) %>%
  select(-Date) %>%
  group_by(Source) %>%
  summarize(MinYear = min(Year), 
            MaxYear = max(Year))
obs_date




##### 
# Old code below here

# Make table of LatinNames comparing Matt and Helen
latin_table <- matt_obs %>%
  select(LatinName) %>%
  distinct() %>% 
  full_join(helen_obs %>% select(LatinName, EnglishName) %>% distinct(), 
             copy = TRUE,
             suffix = c("_Matt", "_Helen"),
             keep = TRUE)

# Output LatinNames to file
write_csv(x = latin_table, file = "data/LatinNameList.csv")

# Compare Sites to find potential differences
matt_sites <- unique(matt_obs$Site)
helen_sites <- unique(helen_obs$Site)
different_sites <- c(setdiff(x = matt_sites, y = helen_sites), 
                     setdiff(x = helen_sites, y = matt_sites))
different_sites




