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

