library(tidyverse)
library(lubridate)
library(dplyr)
library(zoo)
library(ggplot2)

#Reading in CSVs containing variables
winter <- read_csv(file = "data/winter_all.csv")
monsoon <- read_csv(file = "data/monsoon_all.csv")
butterfly <- read_csv(file = "data/Final_butterfly2.csv") 

#joining the monsoon and winter data
Seasonal <- left_join(winter, monsoon, by=c("Site", "year"))

#joining the seasonal data to the butterfly data
butterfly <- butterfly %>% 
  drop_na(Unique_butterflies)

seasonal_butterfly <- left_join(butterfly, Seasonal, by=c("Site", "year"="year"))

#dropping days without a sampling event
sampling_events <-seasonal_butterfly %>% 
  drop_na(Unique_butterflies)

#dropping sample events that do not have weather data i.e. prior to 1981
sampling_events <- sampling_events %>% 
  drop_na(tmin)

#Finding out why we have 18 extra rows (portal has 2 listed lats)
sampling_events %>% 
  count(Site)

Butterfly_summary %>% 
  count(Site)

#adding recent precip option 1
sampling_events <- mutate(sampling_events, recent_precip1 = 
                            ifelse(month%in% 3:7, sampling_events$Wseason_precip,
                                                ifelse(month%in% 8:9, sampling_events$PrecipSum_previous90,
                                                       sampling_events$Mseason_precip)))

#adding recent precip option 2
sampling_events <- mutate(sampling_events, recent_precip2 = 
                            ifelse(month%in% 3:8, sampling_events$Wseason_precip,
                                                ifelse(month%in% 9, sampling_events$PrecipSum_previous90,
                                                       sampling_events$Mseason_precip)))
