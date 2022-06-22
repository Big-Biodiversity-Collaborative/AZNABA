# Variable creation for analysis of NABA/Climate data
# Jennifer Broatch
# jennifer.broatch@asu.edu
# created 2022-06-07

# Load additional packages
library(tidyverse)

# Load data 
all_sites_bflies <- read_csv("data/all-sites-bflies.csv")
all_sites_climate <- read_csv("data/all-sites-climate.csv")

#View data- remove if not desired
View(all_sites_climate)
View(all_sites_bflies)

#TO DO - Merge files
x=2

#Brad Trial Comment 