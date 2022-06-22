# Data Cleaning of PRISM Climate Data Download
# Kathleen L Prudic
# klprudic@arizona.edu
# created 2022-05-26

# Load additional packages
library(tidyverse)
library(lubridate)
<<<<<<< HEAD

=======
>>>>>>> 39cd5251b65df789171244bff41b9fe55c9e6583

# Load latitude and longitude site name data
lat_long_site <- read_csv(file = "output/Lat-Long-Site.csv") 

# Large dataframe that will hold climate data for all sites
all_climate_sites <- NULL

# Loop over each site
for(i in 1:nrow(lat_long_site)){
  site_name <- lat_long_site$Site[i]
  site_lat <- lat_long_site$Latitude[i]
  site_long <- lat_long_site$Longitude[i]
  message(paste0("loading data for ", site_name, ", ", site_lat, ", ", site_long))
  
  # Build Climate File Name
  climate_file <- paste0("PRISM_ppt_tmin_tmean_tmax_stable_4km_199701_202012_", 
                         format(site_lat, nsmall = 4), "_", 
                         format(site_long, nsmall = 4), ".csv")
  climate_file <- paste0("data/Climate-Data/", climate_file)
  # Check to see if there are climate data for this latitude and longitude
  if(file.exists(climate_file)) {
    # Load corresponding climate data for site
    # Skip metadata rows in climate data
    climate_data <- read_csv(file = climate_file, skip = 10, 
                             show_col_types = FALSE)
  
    # Add latitude, longitude, and site columns
    climate_data <- climate_data %>%
      mutate(site = site_name) %>%
      mutate(latitude = site_lat) %>%
      mutate(longitude = site_long)
    
    # Add to dataframe to large climate data
    if(is.null(all_climate_sites)) {
      all_climate_sites <- climate_data
    } else {
      all_climate_sites <- all_climate_sites %>%
        bind_rows(climate_data)
    }
  } else {
    message(paste0("climate file ", climate_file, " does not exist."))
  }
}

# Rename climate columns
all_climate_sites <- all_climate_sites %>%
  rename(precip = `ppt (mm)`, 
         tmin = `tmin (degrees C)`,
         tmean = `tmean (degrees C)`,
         tmax = `tmax (degrees C)`)

# Parse Date to year and month
all_climate_sites <- all_climate_sites %>%
  mutate(year = as.integer(substr(x = Date, start = 1, stop = 4)),
         month = as.integer(substr(x = Date, start = 6, stop = 7))) %>%
  select(-Date)

# Rearrange column order because I can
all_climate_sites <- all_climate_sites %>%
  relocate(year, month, site, latitude, longitude, precip, tmean, tmin, tmax)

# Write to file
write_csv(x = all_climate_sites, 
          file = "data/all-sites-climate.csv")
  