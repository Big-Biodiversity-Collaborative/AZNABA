#reading in necessary packages
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(ggbreak)
library(zoo)

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

#creating a year-month date column
monthly_tmean$Date <- as.yearmon(paste(monthly_tmean$year, monthly_tmean$month), "%Y %m")

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

#creating graph for date and butterfly count for each site
p <- ggplot(bfly_summary, aes(x = date, y = total_butterly_count, color = Site)) +
  geom_line() +
  scale_x_date(date_labels = "%Y-%m")

BflyAbundance <- p + scale_y_break(breaks = c(20000, 132000)) +
  scale_y_continuous(labels = scales::comma) +
  xlab("Date") + ylab("Butterfly Count") + ggtitle("Total Butterfly Counts for Each Sampling Event")

BflyAbundance

#creating graph for date and butterfly richness for each site
r <- ggplot(bfly_summary, aes(x = date, y = Unique_butterflies, color = Site)) +
  geom_line() +
  scale_x_date(date_labels = "%Y-%m")

BflyRichness <- r + xlab("Date") + ylab("Unique Butterfly Species") + 
  ggtitle("Total Butterfly Species for Each Sampling Event")

BflyRichness

#creating graph for month and monthly site temperature
t <- ggplot(monthly_tmean, aes(x = Date, y = Tmonth, color = Site)) + geom_line()
BflyTemp <- t + xlab("Date") + ylab("Monthly Temperature") +
  ggtitle("Monthly Temperature for Each Sampling Site")

BflyTemp

#creating a graph for yearly precip for each site
p <- ggplot(yearly_precip, aes(x = year, y = Pyear, color = Site)) + geom_line()
BflyPrecip <- p + xlab("Year") + ylab("Yearly Precipitation") +
  ggtitle("Yearly Precipitation for Each Sampling Site")

BflyPrecip

#viewing weird counts
santa_check <- Az_naba_all %>% filter((Site == "SantaRitaMountains") & (Year == 2021))



