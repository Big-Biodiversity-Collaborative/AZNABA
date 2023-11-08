# Maxine Cruz
# tmcruz@arizona.edu
# Created: 21 September 2023
# Last modified: 17 October 2023




# ----- ABOUT THE SCRIPT -----

# Generates Figure 1 for AZNABA

# KATY NOTES:
  # Make black and white
  # Have 3 shapes for season sampled (Fall, Spring, Both)
    # Black circle, white circle, black and white circle
  # Data frame headers:
    # Site no. | Site name | Lat | Long | Elevation | Season | Years | Times

# Original figure is terrain map with different color for each site
  # and three different shapes (circle, square, triangle)




# ----- LOAD LIBRARIES -----

library(ggmap)
library(ggplot2)
library(usmap)
library(tidyverse)
library(ggthemes)




# ----- LOAD DATA -----

# Coordinates and names of sample sites
site_coords <- read.csv("data/lat-long-site.csv")

# For filtering spring sample sites
spring_data <- read.csv("data/Spring_Analysis.csv")

# For filtering fall sample sites
fall_data <- read.csv("data/Fall_Analysis.csv")




# ----- DEAL WITH FALL SITES -----

# Subset columns from fall_data
fall_df <- select(fall_data, 
                  Site, Latitude, Longitude, Elevation, 
                  Year = year, 
                  Month = month, 
                  Day = day)

# Add column designating that these are fall samples
fall_df$Season_Sampled <- "Fall"




# ----- DEAL WITH SPRING SITES -----

# Subset columns from spring_data
spring_df <- select(spring_data, 
                  Site, Latitude, Longitude, Elevation, 
                  Year = year, 
                  Month = month, 
                  Day = day)

# Add column designating that these are spring samples
spring_df$Season_Sampled <- "Spring"




# ----- ORGANIZE DATA FRAME FOR FIGURE -----

# UPDATE - Anything in "[]" was added to help in making Figure 4

# Combine filtered fall and spring data into one data frame
both_szn <- rbind(fall_df, spring_df)

# Sort both_szn alphabetically by site, and then by Year
both_szn <- both_szn[order(both_szn$Site, both_szn$Year), ]

# [FOR FIGURE 4: Create separate version of both_szn data]
both_szn_2 <- both_szn

# GrandCanyonSouthRim, McDowellSonoranPreserve, and SabinoCanyonAZ have samples 
# from fall and spring. We want to change their Season_Sampled column to
# Fall/Spring.
both_szn <- both_szn %>%
  mutate(Season_Sampled = 
           ifelse(Site == "GrandCanyonSouthRim" |
                    Site == "McDowellSonoranPreserve" |
                    Site == "SabinoCanyonAZ", 
                  "Fall / Spring", # If Site is named one of the above, then this
                  Season_Sampled)) # Otherwise, keep original season

# Create column for assigning site numbers to sites_coords
nums <- data.frame(Site_Number = cbind(seq(1, 13)))

# Remove sites not used in study from site_coords
site_coords <- site_coords[-c(12, 13, 14), ]

# Reorganize site_coords by driest to wettest (Table 1)
site_coords <- site_coords %>%
  arrange(factor(Site, levels = c("Cottonwood",
                                  "McDowellSonoranPreserve",
                                  "GrandCanyonDesertView",
                                  "GrandCanyonSouthRim",
                                  "SycamoreCreekAZ",
                                  "PatagoniaAZ",
                                  "RamseyCanyonAZ",
                                  "BoyceThompsonArboretum",
                                  "GrandCanyonNorthRim",
                                  "AtascosaHighlandsAZ",
                                  "SabinoCanyonAZ",
                                  "PortalAZ",
                                  "SantaRitaMountains")))

# Combine number assignment column to site_coords
site_coords <- cbind(nums, site_coords)

# Count number of years that each site was sampled
fig_df <- merge(site_coords, aggregate(Year ~ Site, 
                                       data = both_szn, 
                                       FUN = function(x) length(unique(x))))

# Count number of times that each site was sampled
fig_df <- merge(fig_df, aggregate(Month ~ Site, 
                                  data = both_szn, 
                                  FUN = length))

# Note to self: The difference between the two chunks above is the the former 
# accounts for unique years, whereas the latter counts the number of year 
# entries. Since using Year twice might get confusing, Month was used since it
# yielded the same results.

# Get Elevations and Season_Sampled of each site from both_szn
eleva_szn <- unique(select(both_szn,
                           Site, Elevation, Season_Sampled))

# Add column with elevations to fig_df
fig_df <- merge(fig_df, eleva_szn, all.x = TRUE)

# Reorganize column order in data frame
fig_df <- select(fig_df,
                 2, 1, 3, 4, 7, 8 , 5, 6)

# Rename some columns
names(fig_df)[7] <- "Number_Years_Sampled"
names(fig_df)[8] <- "Number_Times_Sampled"

# Order by Site_Number for my own pleasure
fig_df <- fig_df[order(fig_df$Site_Number), ]

# Save data frame
write_csv(fig_df, "data_mc/figure_1_data.csv")

# [FOR FIGURE 4: Combine number assignment column to both_szn_2]
both_szn_2 <- merge(both_szn_2, site_coords, all.y = TRUE)

# [FOR FIGURE 4: Reorganize and select column order in data frame]
both_szn_2 <- select(both_szn_2,
                     9, 1, 5, 6, 7, 8)

# [FOR FIGURE 4: Order by Site_Number for my own pleasure]
both_szn_2 <- both_szn_2[order(both_szn_2$Site_Number), ]

# [FOR FIGURE 4: Save data frame]
write_csv(both_szn_2, "data_mc/site_season_sampledates.csv")




# ----- GENERATE FIGURE 1 (ALTERNATE VERSION 2: JUST AZ POLYGON) -----

# Get state border lines
az_lines <- map_data("state", "arizona")

# Add coordinates for major cities (Flagstaff, Phoenix, and Tucson)
# Coordinates determined by Google search
az_cities <- data.frame(City = c("Flagstaff", "Phoenix", "Tucson"),
                        Longitude = c(-111.651299, -112.074036, -110.911789),
                        Latitude = c(35.198284, 33.448376, 32.253460))

# Plot map
ggplot() +
  geom_point(data = fig_df,
             aes(x = Longitude, y = Latitude, color = Site)) +
  geom_polygon(data = az_lines,
               aes(x = long, y = lat, group = group),
               fill = NA,
               color = "black",
               linewidth = 1) + 
  geom_point(data = az_cities,
             aes(x = Longitude, y = Latitude),
             size = 2) + 
  geom_point(data = fig_df,
             aes(x = Longitude, y = Latitude, 
                 shape = Season_Sampled),
             size = 6) + 
  geom_text(data = fig_df,
            aes(x = Longitude, y = Latitude, label = Site_Number),
            color = "white",
            size = 3) + 
  geom_text(data = az_cities,
            aes(x = Longitude, y = Latitude, label = City),
            fontface = "bold",
            vjust = 0,
            nudge_y = -0.2,
            size = 3.5) + 
  xlab("Longitude") +
  ylab("Latitude") +
  scale_color_manual(
    name = "Site",
    breaks = c("Cottonwood",
               "McDowellSonoranPreserve",
               "GrandCanyonDesertView",
               "GrandCanyonSouthRim",
               "SycamoreCreekAZ",
               "PatagoniaAZ",
               "RamseyCanyonAZ",
               "BoyceThompsonArboretum",
               "GrandCanyonNorthRim",
               "AtascosaHighlandsAZ",
               "SabinoCanyonAZ",
               "PortalAZ",
               "SantaRitaMountains"),
    values = c("Cottonwood" = "grey13", 
               "McDowellSonoranPreserve" = "grey12",
               "GrandCanyonDesertView" = "grey11", 
               "GrandCanyonSouthRim" = "grey10",
               "SycamoreCreekAZ" = "grey9", 
               "PatagoniaAZ" = "grey8",
               "RamseyCanyonAZ" = "grey7", 
               "BoyceThompsonArboretum" = "grey6", 
               "GrandCanyonNorthRim" = "grey5",
               "AtascosaHighlandsAZ" = "grey4", 
               "SabinoCanyonAZ" = "grey3",
               "PortalAZ" = "grey2", 
               "SantaRitaMountains" = "grey1"),
    labels = c("1 - Cottonwood", 
               "2 - McDowell Sonoran Preserve",
               "3 - Grand Canyon Desert View", 
               "4 - Grand Canyon South Rim",
               "5 - Sycamore Creek", 
               "6 - Patagonia",
               "7 - Ramsey Canyon", 
               "8 - Boyce Thompson Arboretum", 
               "9 - Grand Canyon North Rim",
               "10 - Atascosa Highlands", 
               "11 - Sabino Canyon",
               "12 - Portal", 
               "13 - Santa Rita Mountains") 
  ) +
  theme_pander() +
  guides(shape = guide_legend(title = "Season Sampled",
                              override.aes = list(size = 4)),
         color = guide_legend(title = "Site Name",
                              override.aes = list(shape = NA,
                                                  size = 3))) +
  theme(axis.title.x = element_text(margin = margin(t = 10), size = 12),
        axis.title.y = element_text(margin = margin(r = 10), size = 12),
        legend.position = "right") 

# Save as shown on Plots
rstudioapi::savePlotAsImage(file = "output/fig_1_attempt4.png", 
                            format = "png",
                            width = 786, 
                            height = 609)




# ----- GENERATE FIGURE 1 (ALTERNATE VERSION: GOOGLE MAP) -----

# get_stamenmap has stopped working for some reason so here is the backup plan

# Need to register Google API to retrieve a map
# google_api <- "AIzaSyCizpWs57KwBxhIUf_MoS5RW9V1OZAhoKY"
# register_google(google_api)

# Get bounding box coordinates (from Google Maps)
az_bb <- c(left = -115.187847, 
           bottom = 31.046601, 
           right = -108.502422, 
           top = 37.371670)

# Retrieve map from Google
basemap <- get_map(location = az_bb,
                   maptype = "satellite",
                   color = "bw")

# Get state border lines
az_lines <- map_data("state", "arizona")

# Add coordinates for major cities (Flagstaff, Phoenix, and Tucson)
# Coordinates determined by Google search
az_cities <- data.frame(City = c("Flagstaff", "Phoenix", "Tucson"),
                        Longitude = c(-111.651299, -112.074036, -110.911789),
                        Latitude = c(35.198284, 33.448376, 32.253460))

# Plot map
ggmap(basemap) +
  geom_point(data = fig_df,
             aes(x = Longitude, y = Latitude, color = Site)) +
  geom_polygon(data = az_lines,
               aes(x = long, y = lat, group = group),
               fill = NA,
               color = "black",
               linewidth = 1) + 
  geom_point(data = az_cities,
             aes(x = Longitude, y = Latitude),
             size = 2) + 
  geom_point(data = fig_df,
             aes(x = Longitude, y = Latitude, 
                 shape = Season_Sampled),
             size = 6) + 
  geom_text(data = fig_df,
            aes(x = Longitude, y = Latitude, label = Site_Number),
            color = "white",
            size = 3) + 
  geom_text(data = az_cities,
             aes(x = Longitude, y = Latitude, label = City),
             fontface = "bold",
             vjust = 0,
             nudge_y = -0.2,
             size = 3.5) + 
  xlab("Longitude") +
  ylab("Latitude") +
  scale_color_manual(
    name = "Site",
    breaks = c("Cottonwood",
               "McDowellSonoranPreserve",
               "GrandCanyonDesertView",
               "GrandCanyonSouthRim",
               "SycamoreCreekAZ",
               "PatagoniaAZ",
               "RamseyCanyonAZ",
               "BoyceThompsonArboretum",
               "GrandCanyonNorthRim",
               "AtascosaHighlandsAZ",
               "SabinoCanyonAZ",
               "PortalAZ",
               "SantaRitaMountains"),
    values = c("Cottonwood" = "grey13", 
               "McDowellSonoranPreserve" = "grey12",
               "GrandCanyonDesertView" = "grey11", 
               "GrandCanyonSouthRim" = "grey10",
               "SycamoreCreekAZ" = "grey9", 
               "PatagoniaAZ" = "grey8",
               "RamseyCanyonAZ" = "grey7", 
               "BoyceThompsonArboretum" = "grey6", 
               "GrandCanyonNorthRim" = "grey5",
               "AtascosaHighlandsAZ" = "grey4", 
               "SabinoCanyonAZ" = "grey3",
               "PortalAZ" = "grey2", 
               "SantaRitaMountains" = "grey1"),
    labels = c("1 - Cottonwood", 
               "2 - McDowell Sonoran Preserve",
               "3 - Grand Canyon Desert View", 
               "4 - Grand Canyon South Rim",
               "5 - Sycamore Creek", 
               "6 - Patagonia",
               "7 - Ramsey Canyon", 
               "8 - Boyce Thompson Arboretum", 
               "9 - Grand Canyon North Rim",
               "10 - Atascosa Highlands", 
               "11 - Sabino Canyon",
               "12 - Portal", 
               "13 - Santa Rita Mountains") 
  ) +
  guides(shape = guide_legend(title = "Season Sampled",
                              override.aes = list(size = 4)),
         color = guide_legend(title = "Site Name",
                              override.aes = list(shape = NA,
                                                  size = 3))) +
  theme(axis.title.x = element_text(margin = margin(t = 10), size = 12),
        axis.title.y = element_text(margin = margin(r = 10), size = 12)) +
  theme_bw()

# legend.key = element_rect(fill = NA)




# ----- GENERATE FIGURE 1 (GET_STAMENMAP) -----

# Get bounding box coordinates (from Google Maps)
az_bb <- c(left = -115.187847, 
           bottom = 31.046601, 
           right = -108.502422, 
           top = 37.471670)

# Use bounding box to get map
az_stamen <- get_stamenmap(
  bbox = az_bb,
  zoom = 8,
  maptype = "terrain-background",
  color = "bw"
)

# Get state border lines
az_lines <- map_data("state", "arizona")

# Add coordinates for major cities (Flagstaff, Phoenix, and Tucson)
# Coordinates determined by Google search
az_cities <- data.frame(City = c("Flagstaff", "Phoenix", "Tucson"),
                        Longitude = c(-111.651299, -112.074036, -110.911789),
                        Latitude = c(35.198284, 33.448376, 32.253460))

# Plot map
ggmap(az_stamen) +
  geom_point(data = fig_df,
             aes(x = Longitude, y = Latitude, 
                 shape = Season_Sampled),
             size = 5) + # Layers on different shapes depending on when each site was sampled
  geom_point(data = az_cities,
             aes(x = Longitude, y = Latitude)) + # Add points for major cities
  geom_polygon(data = az_lines,
               aes(x = long, y = lat, group = group),
               fill = NA,
               color = "black",
               linewidth = 0.7
  ) + # Adds border outline of Arizona
  geom_text(data = fig_df,
            aes(x = Longitude, y = Latitude, label = Site_Number),
            color = "white",
            size = 2) + # Adds Site_Number on top of each site point
  geom_text(data = az_cities,
            aes(x = Longitude, y = Latitude, label = City),
            hjust = 0, nudge_x = 0.05) + # Add city labels
  xlab("Longitude") +
  ylab("Latitude") +
  guides(shape = guide_legend(title = "Season Sampled")) + # Change legend title

  theme(axis.title.x = element_text(margin = margin(t = 10), size = 12),
        axis.title.y = element_text(margin = margin(r = 10), size = 12)
  ) # Modify alignment of axis titles so they're not so close to plot

# Save plot
# Since ggsave is being silly, I am saving the plot from the Plots export




# ----- FORMER MODIFICATIONS TO MAP -----

# This is just here in case I need it.

# Site legend - problem was that we don't want the black dots
geom_point(data = fig_df,
           aes(x = Longitude, y = Latitude, color = Site)) + # Adds background points so we can have a legend for Site
scale_color_manual(
  name = "Site",
  breaks = c("Cottonwood",
             "McDowellSonoranPreserve",
             "GrandCanyonDesertView",
             "GrandCanyonSouthRim",
             "SycamoreCreekAZ",
             "PatagoniaAZ",
             "RamseyCanyonAZ",
             "BoyceThompsonArboretum",
             "GrandCanyonNorthRim",
             "AtascosaHighlandsAZ",
             "SabinoCanyonAZ",
             "PortalAZ",
             "SantaRitaMountains"
  ), # Generated legend names
  values = c("Cottonwood" = "grey13", 
             "McDowellSonoranPreserve" = "grey12",
             "GrandCanyonDesertView" = "grey11", 
             "GrandCanyonSouthRim" = "grey10",
             "SycamoreCreekAZ" = "grey9", 
             "PatagoniaAZ" = "grey8",
             "RamseyCanyonAZ" = "grey7", 
             "BoyceThompsonArboretum" = "grey6", 
             "GrandCanyonNorthRim" = "grey5",
             "AtascosaHighlandsAZ" = "grey4", 
             "SabinoCanyonAZ" = "grey3",
             "PortalAZ" = "grey2", 
             "SantaRitaMountains" = "grey1"
  ), # Make it look like they're all just bullet points
  labels = c("1 - Cottonwood", 
             "2 - McDowell Sonoran Preserve",
             "3 - Grand Canyon Desert View", 
             "4 - Grand Canyon South Rim",
             "5 - Sycamore Creek", 
             "6 - Patagonia",
             "7 - Ramsey Canyon", 
             "8 - Boyce Thompson Arboretum", 
             "9 - Grand Canyon North Rim",
             "10 - Atascosa Highlands", 
             "11 - Sabino Canyon",
             "12 - Portal", 
             "13 - Santa Rita Mountains"
  ) # Change legend names to these
) + # Modify legend with Site names


  

# ----- FIRST ATTEMPT MAPPING WITH LEAFLET THAT DID NOT WORK WELL -----

# This is just here for my own records.

# library(leaflet)

# Add column to data for marker designation 
# (Since we want a different one per season)
# fig_df <- fig_df %>%
#  mutate(Icon = case_when(
#    Season_Sampled == "Fall" ~ "filled_circle",
#    Season_Sampled == "Spring" ~ "empty_circle",
#    Season_Sampled == "Fall/Spring" ~ "half_fill_circle"
#  ))

# Assign icons
# my_icons <- iconList(
#  "filled_circle" = makeIcon(iconUrl = "C:/Users/maxin/Documents/Prudic_Lab/AZNABA_figures/circle-solid.png",
#                             iconWidth = 14, iconHeight = 14),
#  "empty_circle" = makeIcon(iconUrl = "C:/Users/maxin/Documents/Prudic_Lab/AZNABA_figures/circle-regular.png",
#                            iconWidth = 14, iconHeight = 14),
#  "half_fill_circle" = makeIcon(iconUrl = "C:/Users/maxin/Documents/Prudic_Lab/AZNABA_figures/circle-half-stroke-solid.png",
#                                iconWidth = 14, iconHeight = 14)
# )

# Generate legend for map
# html_legend <- "<script src='https://unpkg.com/ionicons@latest/dist/ionicons.js'></script> <ion-icon name='ellipse'></ion-icon> Fall <br/> <ion-icon name='ellipse-outline'></ion-icon> Spring <br/> <ion-icon name='contrast-outline'></ion-icon> Fall/Spring"

# Generate map of sites
# leaflet(fig_df,
#        options = leafletOptions(zoomControl = FALSE, # No zoom button
#                                 attributionControl = FALSE)) %>%  # No banner
#  addProviderTiles("Esri.WorldTerrain") %>%
#  addProviderTiles("Stamen.TonerLines") %>%
#  setView(lng = -111.682299, lat = 34.296789, zoom = 6) %>%
#  addMarkers(
#    lng = fig_df$Longitude,
#    lat = fig_df$Latitude,
#    icon = my_icons[fig_df$Icon]
#  ) %>%
#  addControl(html = html_legend, position = "bottomleft") # Icons not showing??!



