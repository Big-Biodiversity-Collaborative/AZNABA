#reading in necessary packages
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(ggbreak)
library(zoo)
library(lme4)
library(glme)
library(DescTools)

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
#creating a 2021 only monthly tmean
monthly_tmean2 <- monthly_tmean %>% 
  select(Site, year, month, Tmonth, Date) %>% 
  filter(year > 2020)

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

p2 <- ggplot(bfly_summary, aes(x = month, y = total_butterly_count, color = Site)) +
  geom_line() 

p2

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

#creating a graph for 2021 monthly site temperature
t2 <- ggplot(monthly_tmean2, aes(x = Date, y = Tmonth, color = Site)) + geom_line()
BflyTemp2 <- t2 + xlab("Date") + ylab("Monthly Temperature") +
  ggtitle("Monthly Temperature for Each Sampling Site")

BflyTemp2

#creating a graph for yearly precip for each site
p <- ggplot(yearly_precip, aes(x = year, y = Pyear, color = Site)) + geom_line()
BflyPrecip <- p + xlab("Year") + ylab("Yearly Precipitation") +
  ggtitle("Yearly Precipitation for Each Sampling Site")

BflyPrecip

#viewing weird counts
santa_check <- Az_naba_all %>% filter((Site == "SantaRitaMountains") & (Year == 2021))

bfly_analysis <- read_csv(file = "data/Butterfly_Analysis.csv")

temp_fit <- lmer(total_butterly_count~tmean_previous90 +PrecipSum_previous90 + (1|Site), data=bfly_analysis)
temp_fit
summary(temp_fit)

res<- resid(temp_fit)
plot(fitted(temp_fit), res) 
abline(0,0)



temp_fit <- lm(total_butterly_count~tmean_previous90 +PrecipSum_previous90 + Site, data=bfly_analysis)
#Repeated measures model - site random effect and repeated over time
#why random effect for site



#Winsorizing the butterfly count 
bfly_summary$Win1 <- DescTools::Winsorize(bfly_summary$total_butterly_count, probs = c(0.005, 0.995))
bfly_summary$Win1.5 <- DescTools::Winsorize(bfly_summary$total_butterly_count, probs = c(0.0075, 0.9925))
bfly_summary$Win2 <- DescTools::Winsorize(bfly_summary$total_butterly_count, probs = c(0.01, 0.99))
bfly_summary$Win9915 <- DescTools::Winsorize(bfly_summary$total_butterly_count, probs = c(0, 0.9915))


#Adding in a new column to identify if in monsoon season or not
bfly_summary$Mseason <-NA
bfly_summary$Mseason2 <- NA

#Filling the column with 1 for monsoon and 0 for non-monsoon
bfly_summary <- bfly_summary %>% 
  mutate(Mseason = case_when(
    month <=6 ~ 0,
    month >6 ~ 1
  ))

bfly_summary <- bfly_summary %>% 
  mutate(Mseason2 = case_when(
    month <=7 ~ 0,
    month >7 ~ 1
  ))

#creating the spring data observations
bfly_spring <- bfly_summary %>% 
  select(year, month, day, Site, total_butterfly_count, Unique_butterflies, date) %>% 
  filter(month > 6)

#Removing second sampling of santarita mountains
bfly_spring <- bfly_spring[!(bfly_spring$Site == 'SantaRitaMountains' & bfly_spring$month == 9),]
bfly_spring <- bfly_spring[!(bfly_spring$Site == 'SantaRitaMountains' & bfly_spring$month == 10),]

#removing pre 7/15 sampling
bfly_spring <- bfly_spring[!(bfly_spring$Site == 'GrandCanyonNorthRim' & bfly_spring$day == 5),]
bfly_spring <- bfly_spring[!(bfly_spring$Site == 'SycamoreCreekAZ' & bfly_spring$day == 7),]

#Creating the fall data observations
bfly_fall <- bfly_summary %>% 
  select(year, month, day, Site, total_butterfly_count, Unique_butterflies, date) %>% 
  filter(month < 7)

#Removing post 6/15 sampling  
bfly_fall <- bfly_fall[!(bfly_fall$Site == 'RamseyCanyonAZ' & bfly_fall$month == 6),]
bfly_fall <- bfly_fall[!(bfly_fall$Site == 'AtascosaHighlandsAZ' & bfly_fall$month == 6),]


