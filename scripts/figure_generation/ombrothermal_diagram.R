# Maxine Cruz
# tmcruz@arizona.edu
# Created: 16 November 2023
# Last modified: 21 November 2023




# ----- ABOUT THE SCRIPT -----

# Generates umbrothermal plot (ombrothermic diagram?)
  # Temperature (C) and precipitation (mm) on y-axis
  # Months on x-axis

# Show variation in precipitation per year

# Line graph of the average precipitation / temperature across all years
  # Layer over bar plots showing variation in precipitation over years
  # One line for average precipitation
  # Another line for average temperature




# ----- LOAD LIBRARIES -----

library(tidyverse)
library(cowplot)
library(ggplot2)
library(ggthemes)




# ----- LOAD DATA -----

weather <- read.csv("data/sites_daily_weather.csv") # 224625 observations

sites <- read.csv("data/sites_lat_long.csv")



# ----- REORGANIZE DATA -----

# Remove sites not used in study
sites <- sites[-c(12, 13, 14), ]

# Remove those sites from weather data
weather <- semi_join(weather, sites) # 194675 observations

# Select columns being used for figure
weather <- select(weather, 4, 1, 2, 8, 10)

# Rename columns for myself
weather <- rename(weather, c("site" = "Site",
                             "precip" = "Precip"))

# Filter by site and:
  # 1) Find monthly total precipitation / average temperature per year
  # 2) Make a separate data frame for each with respective precip and tmean

# Site names to loop through
site_name <- select(sites, 3)

# Loop for each site (there are 13)
for (i in 1:13) {
  
  # Get site name i (note: indexing is [row, column])
  site_i <- site_name[i, ]
  
  # To make sure
  print(paste("Processing:", site_i))
  
  # New data frame name specifically for that site, using its name
  new_df_name <- paste("", site_i, sep = "")
  
  # Total precipitation and average temperature per month per year for that site
  site_i_vals <- weather %>%
    filter(site == site_i) %>%
    group_by(year, month) %>%
    summarise(total_precip = sum(precip, na.rm = TRUE), 
              avg_temp = mean(tmean, na.rm = TRUE))
  
  # Average per month across all years for each site
  site_i_vals <- site_i_vals %>%
    group_by(month) %>%
    summarise(avg_precip = mean(total_precip, na.rm = TRUE),
              avg_temp = mean(avg_temp, na.rm = TRUE))
  
  # Assign new data frame to summary results
  assign(new_df_name, site_i_vals)
  
  # To make sure
  print(paste("Completed:", site_i))
  
}




# ----- PLOT OMBROTHERMAL DIAGRAMS (13 SITES INDIVIDUALLY) -----

# Generates an ombrothermic diagram for each of the sites
  # These are then compiled into one plot, showing all 13 graphs

# 1) Atascosa Highlands
coeff <- 4

p1 <- ggplot(AtascosaHighlandsAZ, aes(x = month)) +
  geom_col(aes(y = avg_precip),
           fill = "cornflowerblue") +
  geom_line(aes(y = avg_temp*coeff),
            colour = "black",
            linewidth = 1) +
  scale_y_continuous(
    name = "Precipitation (mm)",
    sec.axis = sec_axis(trans = ~./coeff,
                        name = "Temperature (°C)")
  ) +
  scale_x_continuous(
    breaks = seq_along(month.name),
    labels = month.name
  ) +
  xlab("Month") +
  ggtitle("Atascosa Highlands") +
  theme_pander() +
  theme(axis.title.x = element_text(margin = margin(t = 15),
                                    size = 8),
        axis.text.x = element_text(angle = 55, 
                                   hjust = 1,
                                   size = 6),
        axis.title.y.left = element_text(margin = margin(r = 10),
                                         size = 8),
        axis.text.y.left = element_text(size = 6),
        axis.title.y.right = element_text(margin = margin(l = 15),
                                          size = 8),
        axis.text.y.right = element_text(size = 6),
        plot.title = element_text(size = 10),
        legend.position = "bottom",
        plot.margin = margin(0.5, 1, 1, 1, "cm"))

# 2) Boyce Thompson Arboretum
coeff <- 2

p2 <- ggplot(BoyceThompsonArboretum, aes(x = month)) +
  geom_col(aes(y = avg_precip),
           fill = "cornflowerblue") +
  geom_line(aes(y = avg_temp*coeff),
            colour = "black",
            linewidth = 1) +
  scale_y_continuous(
    name = "Precipitation (mm)",
    sec.axis = sec_axis(trans = ~./coeff,
                        name = "Temperature (°C)")
  ) +
  scale_x_continuous(
    breaks = seq_along(month.name),
    labels = month.name
  ) +
  xlab("Month") +
  ggtitle("Boyce Thompson Arboretum") +
  theme_pander() +
  theme(axis.title.x = element_text(margin = margin(t = 15),
                                    size = 8),
        axis.text.x = element_text(angle = 55, 
                                   hjust = 1,
                                   size = 6),
        axis.title.y.left = element_text(margin = margin(r = 10),
                                         size = 8),
        axis.text.y.left = element_text(size = 6),
        axis.title.y.right = element_text(margin = margin(l = 15),
                                          size = 8),
        axis.text.y.right = element_text(size = 6),
        plot.title = element_text(size = 10),
        legend.position = "bottom",
        plot.margin = margin(0.5, 1, 1, 1, "cm"))

# 3) Cottonwood
coeff <- 2

p3 <- ggplot(Cottonwood, aes(x = month)) +
  geom_col(aes(y = avg_precip),
           fill = "cornflowerblue") +
  geom_line(aes(y = avg_temp*coeff),
            colour = "black",
            linewidth = 1) +
  scale_y_continuous(
    name = "Precipitation (mm)",
    sec.axis = sec_axis(trans = ~./coeff,
                        name = "Temperature (°C)")
  ) +
  scale_x_continuous(
    breaks = seq_along(month.name),
    labels = month.name
  ) +
  xlab("Month") +
  ggtitle("Cottonwood") +
  theme_pander() +
  theme(axis.title.x = element_text(margin = margin(t = 15),
                                    size = 8),
        axis.text.x = element_text(angle = 55, 
                                   hjust = 1,
                                   size = 6),
        axis.title.y.left = element_text(margin = margin(r = 10),
                                         size = 8),
        axis.text.y.left = element_text(size = 6),
        axis.title.y.right = element_text(margin = margin(l = 15),
                                          size = 8),
        axis.text.y.right = element_text(size = 6),
        plot.title = element_text(size = 10),
        legend.position = "bottom",
        plot.margin = margin(0.5, 1, 1, 1, "cm"))

# 4) Grand Canyon Desert View
coeff <- 2

p4 <- ggplot(GrandCanyonDesertView, aes(x = month)) +
  geom_col(aes(y = avg_precip),
           fill = "cornflowerblue") +
  geom_line(aes(y = avg_temp*coeff),
            colour = "black",
            linewidth = 1) +
  scale_y_continuous(
    name = "Precipitation (mm)",
    sec.axis = sec_axis(trans = ~./coeff,
                        name = "Temperature (°C)")
  ) +
  scale_x_continuous(
    breaks = seq_along(month.name),
    labels = month.name
  ) +
  xlab("Month") +
  ggtitle("Grand Canyon Desert View") +
  theme_pander() +
  theme(axis.title.x = element_text(margin = margin(t = 15),
                                    size = 8),
        axis.text.x = element_text(angle = 55, 
                                   hjust = 1,
                                   size = 6),
        axis.title.y.left = element_text(margin = margin(r = 10),
                                         size = 8),
        axis.text.y.left = element_text(size = 6),
        axis.title.y.right = element_text(margin = margin(l = 15),
                                          size = 8),
        axis.text.y.right = element_text(size = 6),
        plot.title = element_text(size = 10),
        legend.position = "bottom",
        plot.margin = margin(0.5, 1, 1, 1, "cm"))

# 5) Grand Canyon North Rim
coeff <- 4

p5 <- ggplot(GrandCanyonNorthRim, aes(x = month)) +
  geom_col(aes(y = avg_precip),
           fill = "cornflowerblue") +
  geom_line(aes(y = avg_temp*coeff),
            colour = "black",
            linewidth = 1) +
  scale_y_continuous(
    name = "Precipitation (mm)",
    sec.axis = sec_axis(trans = ~./coeff,
                        name = "Temperature (°C)")
  ) +
  scale_x_continuous(
    breaks = seq_along(month.name),
    labels = month.name
  ) +
  xlab("Month") +
  ggtitle("Grand Canyon North Rim") +
  theme_pander() +
  theme(axis.title.x = element_text(margin = margin(t = 15),
                                    size = 8),
        axis.text.x = element_text(angle = 55, 
                                   hjust = 1,
                                   size = 6),
        axis.title.y.left = element_text(margin = margin(r = 10),
                                         size = 8),
        axis.text.y.left = element_text(size = 6),
        axis.title.y.right = element_text(margin = margin(l = 15),
                                          size = 8),
        axis.text.y.right = element_text(size = 6),
        plot.title = element_text(size = 10),
        legend.position = "bottom",
        plot.margin = margin(0.5, 1, 1, 1, "cm"))

# 6) Grand Canyon South Rim
coeff <- 2

p6 <- ggplot(GrandCanyonSouthRim, aes(x = month)) +
  geom_col(aes(y = avg_precip),
           fill = "cornflowerblue") +
  geom_line(aes(y = avg_temp*coeff),
            colour = "black",
            linewidth = 1) +
  scale_y_continuous(
    name = "Precipitation (mm)",
    sec.axis = sec_axis(trans = ~./coeff,
                        name = "Temperature (°C)")
  ) +
  scale_x_continuous(
    breaks = seq_along(month.name),
    labels = month.name
  ) +
  xlab("Month") +
  ggtitle("Grand Canyon South Rim") +
  theme_pander() +
  theme(axis.title.x = element_text(margin = margin(t = 15),
                                    size = 8),
        axis.text.x = element_text(angle = 55, 
                                   hjust = 1,
                                   size = 6),
        axis.title.y.left = element_text(margin = margin(r = 10),
                                         size = 8),
        axis.text.y.left = element_text(size = 6),
        axis.title.y.right = element_text(margin = margin(l = 15),
                                          size = 8),
        axis.text.y.right = element_text(size = 6),
        plot.title = element_text(size = 10),
        legend.position = "bottom",
        plot.margin = margin(0.5, 1, 1, 1, "cm"))

# 7) McDowell Sonoran Preserve
coeff <- 1

p7 <- ggplot(McDowellSonoranPreserve, aes(x = month)) +
  geom_col(aes(y = avg_precip),
           fill = "cornflowerblue") +
  geom_line(aes(y = avg_temp*coeff),
            colour = "black",
            linewidth = 1) +
  scale_y_continuous(
    name = "Precipitation (mm)",
    sec.axis = sec_axis(trans = ~./coeff,
                        name = "Temperature (°C)")
  ) +
  scale_x_continuous(
    breaks = seq_along(month.name),
    labels = month.name
  ) +
  xlab("Month") +
  ggtitle("McDowell Sonoran Preserve") +
  theme_pander() +
  theme(axis.title.x = element_text(margin = margin(t = 15),
                                    size = 8),
        axis.text.x = element_text(angle = 55, 
                                   hjust = 1,
                                   size = 6),
        axis.title.y.left = element_text(margin = margin(r = 10),
                                         size = 8),
        axis.text.y.left = element_text(size = 6),
        axis.title.y.right = element_text(margin = margin(l = 15),
                                          size = 8),
        axis.text.y.right = element_text(size = 6),
        plot.title = element_text(size = 10),
        legend.position = "bottom",
        plot.margin = margin(0.5, 1, 1, 1, "cm"))

# 8) Patagonia
coeff <- 4

p8 <- ggplot(PatagoniaAZ, aes(x = month)) +
  geom_col(aes(y = avg_precip),
           fill = "cornflowerblue") +
  geom_line(aes(y = avg_temp*coeff),
            colour = "black",
            linewidth = 1) +
  scale_y_continuous(
    name = "Precipitation (mm)",
    sec.axis = sec_axis(trans = ~./coeff,
                        name = "Temperature (°C)")
  ) +
  scale_x_continuous(
    breaks = seq_along(month.name),
    labels = month.name
  ) +
  xlab("Month") +
  ggtitle("Patagonia") +
  theme_pander() +
  theme(axis.title.x = element_text(margin = margin(t = 15),
                                    size = 8),
        axis.text.x = element_text(angle = 55, 
                                   hjust = 1,
                                   size = 6),
        axis.title.y.left = element_text(margin = margin(r = 10),
                                         size = 8),
        axis.text.y.left = element_text(size = 6),
        axis.title.y.right = element_text(margin = margin(l = 15),
                                          size = 8),
        axis.text.y.right = element_text(size = 6),
        plot.title = element_text(size = 10),
        legend.position = "bottom",
        plot.margin = margin(0.5, 1, 1, 1, "cm"))

# 9) Portal
coeff <- 4

p9 <- ggplot(PortalAZ, aes(x = month)) +
  geom_col(aes(y = avg_precip),
           fill = "cornflowerblue") +
  geom_line(aes(y = avg_temp*coeff),
            colour = "black",
            linewidth = 1) +
  scale_y_continuous(
    name = "Precipitation (mm)",
    sec.axis = sec_axis(trans = ~./coeff,
                        name = "Temperature (°C)")
  ) +
  scale_x_continuous(
    breaks = seq_along(month.name),
    labels = month.name
  ) +
  xlab("Month") +
  ggtitle("Portal") +
  theme_pander() +
  theme(axis.title.x = element_text(margin = margin(t = 15),
                                    size = 8),
        axis.text.x = element_text(angle = 55, 
                                   hjust = 1,
                                   size = 6),
        axis.title.y.left = element_text(margin = margin(r = 10),
                                         size = 8),
        axis.text.y.left = element_text(size = 6),
        axis.title.y.right = element_text(margin = margin(l = 15),
                                          size = 8),
        axis.text.y.right = element_text(size = 6),
        plot.title = element_text(size = 10),
        legend.position = "bottom",
        plot.margin = margin(0.5, 1, 1, 1, "cm"))

# 10) Ramsey Canyon
coeff <- 4

p10 <- ggplot(RamseyCanyonAZ, aes(x = month)) +
  geom_col(aes(y = avg_precip),
           fill = "cornflowerblue") +
  geom_line(aes(y = avg_temp*coeff),
            colour = "black",
            linewidth = 1) +
  scale_y_continuous(
    name = "Precipitation (mm)",
    sec.axis = sec_axis(trans = ~./coeff,
                        name = "Temperature (°C)")
  ) +
  scale_x_continuous(
    breaks = seq_along(month.name),
    labels = month.name
  ) +
  xlab("Month") +
  ggtitle("Ramsey Canyon") +
  theme_pander() +
  theme(axis.title.x = element_text(margin = margin(t = 15),
                                    size = 8),
        axis.text.x = element_text(angle = 55, 
                                   hjust = 1,
                                   size = 6),
        axis.title.y.left = element_text(margin = margin(r = 10),
                                         size = 8),
        axis.text.y.left = element_text(size = 6),
        axis.title.y.right = element_text(margin = margin(l = 15),
                                          size = 8),
        axis.text.y.right = element_text(size = 6),
        plot.title = element_text(size = 10),
        legend.position = "bottom",
        plot.margin = margin(0.5, 1, 1, 1, "cm"))

# 11) Sabino Canyon
coeff <- 4

p11 <- ggplot(SabinoCanyonAZ, aes(x = month)) +
  geom_col(aes(y = avg_precip),
           fill = "cornflowerblue") +
  geom_line(aes(y = avg_temp*coeff),
            colour = "black",
            linewidth = 1) +
  scale_y_continuous(
    name = "Precipitation (mm)",
    sec.axis = sec_axis(trans = ~./coeff,
                        name = "Temperature (°C)")
  ) +
  scale_x_continuous(
    breaks = seq_along(month.name),
    labels = month.name
  ) +
  xlab("Month") +
  ggtitle("Sabino Canyon") +
  theme_pander() +
  theme(axis.title.x = element_text(margin = margin(t = 15),
                                    size = 8),
        axis.text.x = element_text(angle = 55, 
                                   hjust = 1,
                                   size = 6),
        axis.title.y.left = element_text(margin = margin(r = 10),
                                         size = 8),
        axis.text.y.left = element_text(size = 6),
        axis.title.y.right = element_text(margin = margin(l = 15),
                                          size = 8),
        axis.text.y.right = element_text(size = 6),
        plot.title = element_text(size = 10),
        legend.position = "bottom",
        plot.margin = margin(0.5, 1, 1, 1, "cm"))

# 12) Santa Rita Mountains
coeff <- 4

p12 <- ggplot(SantaRitaMountains, aes(x = month)) +
  geom_col(aes(y = avg_precip),
           fill = "cornflowerblue") +
  geom_line(aes(y = avg_temp*coeff),
            colour = "black",
            linewidth = 1) +
  scale_y_continuous(
    name = "Precipitation (mm)",
    sec.axis = sec_axis(trans = ~./coeff,
                        name = "Temperature (°C)")
  ) +
  scale_x_continuous(
    breaks = seq_along(month.name),
    labels = month.name
  ) +
  xlab("Month") +
  ggtitle("Santa Rita Mountains") +
  theme_pander() +
  theme(axis.title.x = element_text(margin = margin(t = 15),
                                    size = 8),
        axis.text.x = element_text(angle = 55, 
                                   hjust = 1,
                                   size = 6),
        axis.title.y.left = element_text(margin = margin(r = 10),
                                         size = 8),
        axis.text.y.left = element_text(size = 6),
        axis.title.y.right = element_text(margin = margin(l = 15),
                                          size = 8),
        axis.text.y.right = element_text(size = 6),
        plot.title = element_text(size = 10),
        legend.position = "bottom",
        plot.margin = margin(0.5, 1, 1, 1, "cm"))

# 13) Sycamore Creek
coeff <- 2

p13 <- ggplot(SycamoreCreekAZ, aes(x = month)) +
  geom_col(aes(y = avg_precip),
           fill = "cornflowerblue") +
  geom_line(aes(y = avg_temp*coeff),
            colour = "black",
            linewidth = 1) +
  scale_y_continuous(
    name = "Precipitation (mm)",
    sec.axis = sec_axis(trans = ~./coeff,
                        name = "Temperature (°C)")
  ) +
  scale_x_continuous(
    breaks = seq_along(month.name),
    labels = month.name
  ) +
  xlab("Month") +
  ggtitle("Sycamore Creek") +
  theme_pander() +
  theme(axis.title.x = element_text(margin = margin(t = 15),
                                    size = 8),
        axis.text.x = element_text(angle = 55, 
                                   hjust = 1,
                                   size = 6),
        axis.title.y.left = element_text(margin = margin(r = 10),
                                         size = 8),
        axis.text.y.left = element_text(size = 6),
        axis.title.y.right = element_text(margin = margin(l = 15),
                                          size = 8),
        axis.text.y.right = element_text(size = 6),
        plot.title = element_text(size = 10),
        legend.position = "bottom",
        plot.margin = margin(0.5, 1, 1, 1, "cm"))




# ----- PLOT ALL ON ONE GRAPH -----

plot_grid(
  p1, p2, p3, p4, 
  p5, p6, p7, p8, 
  p9, p10, p11, p12, 
  p13, NULL, NULL, NULL,
  ncol = 4,
  align = "hv"
)

ggsave2("ombrothermics.png",
        plot = last_plot(),
        path = "output",
        width = 45,
        height = 30,
        units = "cm")




# ----- ANOTHER FIGURE: AVERAGE THE 13 SITES INTO ONE DIAGRAM -----

# Figure above will be used in supplementary material
  # Main manuscript will contain this averaged ombrothermic diagram

# Roughly the same loop as above, 
  # but modified to allow for averaging across all sites

# New data frame to store values in
vals_df <- data.frame()

# L o o p
for (i in 1:13) {
  
  # Get site name i (note: indexing is [row, column])
  site_i <- site_name[i, ]
  
  # Total precipitation and average temperature per month per year for that site
  site_i_vals <- weather %>%
    filter(site == site_i) %>%
    group_by(year, month) %>%
    summarise(total_precip = sum(precip, na.rm = TRUE), 
              avg_temp = mean(tmean, na.rm = TRUE))
  
  # Average per month across all years for each site
  site_i_vals <- site_i_vals %>%
    group_by(month) %>%
    summarise(avg_precip = mean(total_precip, na.rm = TRUE),
              avg_temp = mean(avg_temp, na.rm = TRUE))
  
  # Specify which site values are from
  site_i_vals <- mutate(site_i_vals, site = site_i)
  
  # Add new values to existing data frame
  vals_df <- rbind(vals_df, site_i_vals)
  
  # Note that site values have been appended
  print(paste("Added to data frame:", site_i))
  
}

# Rearrange for saving
vals_df <- select(vals_df, 4, 1, 2, 3)

# Save data frame
write.csv(vals_df, "data/figure_generation/sites_monthly_avg_temp_precip.csv",
          row.names = FALSE) # Suppresses random addition of X column when saving

# If starting from here, load data
vals_df <- read.csv("data/figure_generation/sites_monthly_avg_temp_precip.csv")

# Average the values across all sites
all_sites_vals <- vals_df %>%
  group_by(month) %>%
  summarise(avg_precip = mean(avg_precip, na.rm = TRUE),
            avg_temp = mean(avg_temp, na.rm = TRUE))

# Save data frame
write.csv(all_sites_vals, 
          "data/figure_generation/all_sites_monthly_avg_temp_precip.csv",
          row.names = FALSE)




# ----- GENERATE ANOTHER OMBROTHERMIC DIAGRAM -----

# If starting from here, load data
all_sites_vals <- read.csv("data/figure_generation/all_sites_monthly_avg_temp_precip.csv")

# Plot
coeff <- 3

ggplot(all_sites_vals, aes(x = month)) +
  geom_col(aes(y = avg_precip),
           fill = "cornflowerblue") +
  geom_line(aes(y = avg_temp*coeff),
            colour = "black",
            linewidth = 1) +
  scale_y_continuous(
    name = "Precipitation (mm)",
    sec.axis = sec_axis(trans = ~./coeff,
                        name = "Temperature (°C)")
  ) +
  scale_x_continuous(
    breaks = seq_along(month.name),
    labels = month.name
  ) +
  xlab("Month") +
  theme_pander() +
  theme(axis.title.x = element_text(margin = margin(t = 15)),
        axis.text.x = element_text(angle = 55, 
                                   hjust = 1),
        axis.title.y.left = element_text(margin = margin(r = 10)),
        axis.title.y.right = element_text(margin = margin(l = 15)),
        plot.margin = margin(1, 1, 0.5, 1, "cm"))

# Save plot
ggsave2("all_sites_avg_ombrothermic.png",
        plot = last_plot(),
        path = "output",
        width = 15,
        height = 11,
        units = "cm")



