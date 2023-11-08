# LOAD LIBRARIES ---
library(tidyverse)
library(lubridate)
library(dplyr)
library(zoo)
library(ggplot2)



# LOAD DATA ---
winter <- read_csv(file = "data/winter_all.csv")
monsoon <- read_csv(file = "data/monsoon_all.csv")
butterfly <- read_csv(file = "data/final_butterfly.csv") 
baseline <- read_csv(file= "data/butterfly_summary.csv")
party_values <- read_csv(file= "data/AZNABA_lat_long.csv")



# COMBINE DATA FRAMES ---

# Joining the monsoon and winter data
Seasonal <- left_join(winter, monsoon, by=c("Site", "year"))

# Joining the seasonal data to the butterfly data
seasonal_butterfly <- left_join(butterfly, Seasonal, by=c("Site", "year"))

# Dropping days without a sampling event
sampling_events <-seasonal_butterfly %>% 
  drop_na(Unique_butterflies)

# Dropping sample events that do not have weather data i.e. prior to 1981
sampling_events <- sampling_events %>% 
  drop_na(tmin)

# Adding recent precip option 1
sampling_events <- mutate(sampling_events, recent_precip1 = 
                            ifelse(month%in% 3:7, sampling_events$Wseason_precip,
                                                ifelse(month%in% 8:9, sampling_events$PrecipSum_previous90,
                                                       sampling_events$Mseason_precip)))

# Adding recent precip option 2
sampling_events <- mutate(sampling_events, recent_precip2 = 
                            ifelse(month%in% 3:8, sampling_events$Wseason_precip,
                                                ifelse(month%in% 9, sampling_events$PrecipSum_previous90,
                                                       sampling_events$Mseason_precip)))

# Removing unneeded columns
sampling_events<- subset(sampling_events, select = -c(Group_Thirty, Group_Twentyeight, previous_Mseason_precip))

# Creating a csv of the sampling events
write_csv(x = sampling_events, 
          file = "data/sampling_events.csv")

# Renaming party variables
party_values <- party_values %>% 
  rename(Total_Parties = '#Parties',
         Total_Observers = '#Observers'
  )
# Removing unneeded rows
party_values <- subset(party_values, select = -c(NABAEnglishName, Longitude, Latitude))

# Trying to group sampling events
party_values <- party_values %>% 
  select(Year, ButterflyCount, Month, Day, Site, PartyHours, Total_Parties, Total_Observers, TotalDistanceMi) %>% 
  group_by(Year, Month, Day, Site,Total_Observers, TotalDistanceMi, ButterflyCount)

# Removing duplicates (however we have 271 values when we should have only 219)
party_values <- party_values %>% 
  distinct(Year, Month, Day, Site, Total_Parties, PartyHours)

# Removing helen/matt duplicates where distance and observers were added
party_values<- party_values[!duplicated(party_values[c("Year", "Month", "Day", "Site", "PartyHours", "Total_Parties")],
                                        fromLast = FALSE),]

# Removing duplicates in helen's data where I assume a copying error occurred
party_values<- party_values[!duplicated(party_values[c("Year", "Month", "Day", "Site", "PartyHours", "Total_Parties")],
                                        fromLast = TRUE),]


# Joining the party variables to sampling events
party_butterfly <- left_join(sampling_events, party_values, by=c("year"="Year", "month"="Month",
                                                                 "Site"="Site", "day"="Day"))


# Creating a final csv of the sampling event data along with weather data
write_csv(x = party_butterfly, 
          file = "data/butterfly_analysis.csv")




