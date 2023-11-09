# Maxine Cruz
# tmcruz@arizona.edu
# Created: 21 September 2023
# Last modified: 9 November 2023




# ----- ABOUT THE SCRIPT -----

# Generates Figure 4 for AZNABA

# KATY NOTES:
  # 8 panels of regressions
  # 2 rows (4 panels each row)
    # One row is abundance and the other is richness
    # 4 panels are abundance/richness v.
      # Monsoon precipitation
      # Winter precipitation
      # 30-day Maximum Temperature (prior to sampling event)
      # 30-day Minimum Temperature (prior to sampling event)
  # Data frame headers:
  # Site no. | Predictor | Response | Predictor Value | Response Value | Season
  # facet_wrap(response ~ predictor)

# Original figure is one regression with Precipitation on Y and Year on X.
  # Three colors of lines for Monsoon, Winter, and Annual

# NOTES FOR MYSELF:
  # Richness: Number of unique species within study area
  # Abundance: Number of individuals for each species within study area




# ----- LOAD LIBRARIES -----

library(tidyverse)
library(ggplot2)




# ----- LOAD DATA -----

# Site numbers, names, dates, and season sampled
sites_df <- read.csv("data/figure_generation/site_season_sampledates.csv")

# Butterfly, monsoons, and temperature data
bfly_df <- read.csv("data/butterfly_analysis.csv")




# ----- PRELIMINARY ORGANIZATION OF DATA FRAME -----

# Combine the two data so bfly_df has Site_Number and Season_sampled

# Keep necessary variables from Butterfly_Analysis.csv
bfly_df <- select(bfly_df,
                  4, 1, 2, 3, 15, 16, 18, 19, 35, 42)

# NOTES:
  # 4 = Site
  # 1 = Sample year
  # 2 = Sample month
  # 3 = Sample day
  # 15 = total_butterfly_count = Abundance
  # 16 = Unique_butterflies = Richness
  # 18 = tmax_previous30 = 30-day max. temperature prior to sampling event
  # 19 = tmin_previous30 = 30-day min. temperature prior to sampling event
  # 35 = Wseason_precip = Winter season precipitation
  # 42 = Mseason_precip = Monsoon (Summer) season precipitation

# Rename some columns in sites_df so that date merging happens properly
sites_df <- rename(sites_df, 
                   year = Year,
                   month = Month,
                   day = Day)

# Merge Site_Numbers and Season_Sampled with the bfly_df
bfly_df <- merge(bfly_df, sites_df, all.y = TRUE)

# Reorganize and rename columns
bfly_df <- bfly_df %>%
  select(11, everything()) %>%
  rename(Year = year,
         Month = month,
         Day = day,
         Abundance = total_butterfly_count,
         Richness = Unique_butterflies,
         Tmax = tmax_previous30,
         Tmin = tmin_previous30,
         Winter = Wseason_precip,
         Monsoon = Mseason_precip)




# ----- TURN PREDICTOR COLUMNS INTO ROWS -----

# Need to make it so that the column header for predictors is in another column.
# And all predictor values are in one column. Same with richness and abundance.

# Create empty data frame to store all the new data frames
bfly_predictors_df <- data.frame()

# Loop for creating new data frames (df) for PREDICTOR variables (columns 8-11)

# For i values 8 to 11, starting with 8...
for (i in 8:11) {
  
  # Select these columns from bfly_predictors_df and store it in a new df
  new_df <- data.frame(select(bfly_df, 1, 2, 3, 4, 5, 12, i))
  
  # Get the name of that column i and store it in a new variable
  new_df_name <- paste(colnames(bfly_df)[i])
  
  # Add another column to the new df containing the name of those selected variables 
  new_df <- mutate(new_df, Predictor = new_df_name)
  
  # Rename i column (should be 7th one in new_df) as Value
  new_df <- rename(new_df, Predictor_Value = 7)
  
  # Add new_df to another new df that will contain all the new dfs
  bfly_predictors_df <- rbind(bfly_predictors_df, new_df)
  
  # Then the loop repeats with the next i value (starts with 8, stops after 11)
}

# Order predictors data frame
bfly_predictors_df <- bfly_predictors_df[order(bfly_predictors_df$Site_Number,
                                               bfly_predictors_df$Year,
                                               bfly_predictors_df$Month,
                                               bfly_predictors_df$Day,
                                               bfly_predictors_df$Predictor), ]

# Save predictors data frame
write.csv(bfly_predictors_df, "data/figure_generation/predictor_values.csv")




# ----- TURN RESPONSE COLUMNS INTO ROWS -----

# Loop for creating new data frames (df) for RESPONSE variables (columns 6-7)
# (Same as for the predictors)

bfly_responses_df <- data.frame()

for (i in 6:7) {
  new_df <- data.frame(select(bfly_df, 1, 2, 3, 4, 5, 12, i))
  new_df_name <- paste(colnames(bfly_df)[i])
  new_df <- mutate(new_df, Response = new_df_name)
  new_df <- rename(new_df, Response_Value = 7)
  bfly_responses_df <- rbind(bfly_responses_df, new_df)
}

# Order response data frame
bfly_responses_df <- bfly_responses_df[order(bfly_responses_df$Site_Number,
                                             bfly_responses_df$Year,
                                             bfly_responses_df$Month,
                                             bfly_responses_df$Day,
                                             bfly_responses_df$Response), ]

# Save response data frame
write.csv(bfly_responses_df, "data/figure_generation/response_values.csv")




# ----- MERGE PREDICTOR AND RESPONSE DATA FRAMES -----

# Original data has 202 sample dates ~
  # 202 abundance + 202 richness = 404 response data
  # 202 tmax + 202 tmin + 202 monsoon + 202 winter = 808 predictor data
  # Each of the 404 response data will have 4 corresponding predictor data
  # So final data frame should have 404 * 4 = 1616 observations

# ******* PROBLEM - NEW DF HAS 1712 RATHER THAN 1616 - MAYBE DUPLICATING DATES?

# Merge data frames
bfly_df2 <- merge(bfly_responses_df, bfly_predictors_df, all = TRUE)

# Order response data frame
bfly_df2 <- bfly_df2[order(bfly_df2$Site_Number,
                           bfly_df2$Year,
                           bfly_df2$Month,
                           bfly_df2$Day,
                           bfly_df2$Response,
                           bfly_df2$Predictor), ]

# Save new data frame
write.csv(bfly_df2, "data/figure_generation/figure_4_data.csv")




# ----- GENERATE FIGURE 4 -----

# Generate new labels for predictors so they are more descriptive
pred_labels <- c(
  Monsoon = "Monsoon Precipitation",
  Tmax = "30-day Maximum Temperature prior to Sampling Date",
  Tmin = "30-day Minimum Temperature prior to Sampling Date",
  Winter = "Winter Precipitation"
)

# Match labels with Predictor label in bfly_df2
mods <- match(bfly_df2$Predictor, names(pred_labels))

# Add column with new Predictor labels
bfly_df2$Predictor2 <- pred_labels[mods]

# Generate plot
ggplot(bfly_df2, aes(x = Predictor_Value, 
                     y = Response_Value,
                     color = Season_Sampled)) +
  geom_point() +
  geom_smooth(method = "lm", # Add regression line
              color = "cornflowerblue",
              fill = "lightskyblue") + 
  facet_grid(rows = vars(Response), # Rows are separated by Response variables
             cols = vars(Predictor2), # Columns are separated by Predictor variables
             scales = "free", # Each panel can have a more appropriate scale
             switch = "y", # Move y labels to left side
             labeller = labeller(Predictor2 = label_wrap_gen(25))) + # Wrap label text
  scale_y_continuous(position = "right") + # Move y-axis scale to right side
  xlab("") + # Remove x-axis label
  ylab("") + # Remove y-axis label
  theme_minimal() + # General ggplot theme
  theme(strip.text = element_text(face = "bold", # Modify x label
                                    color = "black",
                                    size = 10),
        panel.border = element_rect(fill = "transparent", # Add border around each panel
                                    color = "black",
                                    linewidth = 1),
        strip.background = element_rect(fill = "grey88",
                                        linetype = "solid",
                                        color = "black",
                                        linewidth = 1))

# Save as shown in Plots




# ----- GENERATE FIGURE 4 (VERSION 2: NESTED FACETS) -----

# Extension to create nested facets
library(ggh4x)

# Generate figure again, but further facet into fall and spring samples
ggplot(bfly_df2, aes(x = Predictor_Value, 
                     y = Response_Value)) +
  geom_point() +
  geom_smooth(method = "lm",
              color = "cornflowerblue",
              fill = "lightskyblue") + 
  facet_nested(Response + Season_Sampled ~ Predictor2,
               scales = "free",
               switch = "y", 
               labeller = labeller(Predictor2 = label_wrap_gen(25))) +
  scale_y_continuous(position = "right") + 
  xlab("") + 
  ylab("") + 
  theme_minimal() +
  theme(strip.text = element_text(face = "bold", 
                                  color = "black",
                                  size = 10),
        panel.border = element_rect(fill = "transparent",
                                    color = "black",
                                    linewidth = 1),
        strip.background = element_rect(fill = "grey88",
                                        linetype = "solid",
                                        color = "black",
                                        linewidth = 1))

# Save as shown on Plots
rstudioapi::savePlotAsImage(file = "output/fig_4_attempt2.png", 
                            format = "png",
                            width = 921, 
                            height = 610)




