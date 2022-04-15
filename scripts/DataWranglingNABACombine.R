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

# Drop rows not identified to species or subspecies 
# Rows with one word in LatinName
helen_obs <- helen_obs %>%
  filter(str_count(string = LatinName, pattern = " ") > 0)

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

# Drop columns not in common except EnglishNames

# Create taxonomy of EnglishNames to LatinNames

# Add EnglishNames to Matt data

# Combine Matt and Helen datasets

# Remove duplicates (LatinName, Site, Date, Latitude, Longitude)



