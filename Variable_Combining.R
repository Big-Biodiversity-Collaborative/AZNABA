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
seasonal_butterfly <- left_join(butterfly, Seasonal, by=c("Site", "year"))

#dropping days without a sampling event
sampling_events <-seasonal_butterfly %>% 
  drop_na(Unique_butterflies)

sampling_events <- sampling_events %>% 
  drop_na(tmin)
