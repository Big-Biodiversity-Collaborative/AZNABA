#reading in necessary packages
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)


#Creating a new daily weather file 
daily_weather <- NULL
read_daily<- 
  function(path, pattern = "*.csv") {
    list.files(path = "data/Climate-Data/DAILY", pattern = ".csv", full.names = TRUE) %>% 
      map_df(~read.csv(., skip=10))
  }
daily_weather <- read_daily('./data/Climate-Data/DAILY')

#renaming variables
daily_weather <- daily_weather %>% 
  rename(Elevation  = 'Elevation..m.',
         Precip = 'ppt..mm.',
         tmin = 'tmin..degrees.C.',
         tmean ='tmean..degrees.C.',
         tmax = 'tmax..degrees.C.',
         tdmean ='tdmean..degrees.C.',
         vpdmin ='vpdmin..hPa.',
         vpdmax ='vpdmax..hPa.',
         Site = 'Name'
  )


#converting date to work with the lubridate package
daily_weather$Date <- lubridate::ymd(daily_weather$Date)
#adding month and year columns
daily_weather$month <- month(daily_weather$Date)
daily_weather$year <- year(daily_weather$Date)

#Grouping by monthly average temp
monthly_tmean<- daily_weather %>% 
  group_by(Site, year, month) %>% 
  summarize(Tmonth = mean(tmean))

#grouping by yearly precip
yearly_precip<- daily_weather %>% 
  group_by(Site, year) %>% 
  summarize(Pyear = sum(Precip))

#reading in analysis csv
bfly_summary <- read_csv(file = "data/Butterfly_Analysis.csv")

#selecting only the needed rows
bfly_summary <- bfly_summary %>% 
  select(year, month, day, Site, total_butterly_count, Unique_butterflies)

#combining year/month/day back into a date
bfly_summary$date <- as.Date(with(bfly_summary, paste(year,month,day, sep = "-")), "%Y-%m-%d")

#creating graph for date and butterfly count
interaction.plot(bfly_summary$date, bfly_summary$Site, bfly_summary$total_butterly_count,
                 xlab = "Date", ylab = "Butterfly Count", col = c(1:15), legend = F)

#creating graph for date and butterfly count
ggplot(bfly_summary, aes(x = date, y = total_butterly_count, color = Site)) +
  geom_line() +
  scale_x_date(date_labels = "%Y-%m")




