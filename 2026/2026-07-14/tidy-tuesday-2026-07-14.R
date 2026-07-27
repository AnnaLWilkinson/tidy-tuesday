
#  About this script ------------------------------------------------------

#  Purpose: Many penguins
#  Project: Tidy Tuesday
#  Author: Anna Wilkinson; anna.wilkinson@burnet.edu.au
#  Date started: 21 July 2026


#  Load libraries ---------------------------------------------------------
library(tidyverse)


#  Load data --------------------------------------------------------------

# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2026-07-14')
## OR
#tuesdata <- tidytuesdayR::tt_load(2026, week = 28)

many_penguins <- tuesdata$many_penguins

# Option 2: Read directly from GitHub
#many_penguins <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-07-14/many_penguins.csv')

many_penguins <- many_penguins %>% 
  janitor::clean_names()


# Create datasets for practicing joins ------------------------------------

many_penguins %>% 
  count(species)

many_penguins %>% 
  count(genus)

# create an identifier (linkage key)
key_length <-  10
pool <- c(letters, LETTERS, 0:9)
many_penguins$linkage_key <- replicate(nrow(many_penguins), paste(sample(pool, key_length, replace = TRUE), collapse = ""))

df_penguins_attr  <- many_penguins %>% 
  select(linkage_key, 
         species,
         genus,
         shortname, 
         sex)
  
df_penguins_beak  <- many_penguins %>% 
  select(linkage_key, 
         starts_with("beak"))
  
df_penguins_other <- many_penguins %>% 
  select(linkage_key, 
         tarsus_length, 
         wing_length,
         kipps_distance,
         secondary1, 
         hand_wing_index)

# split other df into genus
genus <- factor(unique(many_penguins$genus))
df_genus_split <- split(df_penguins_other, genus)  
list2env(df_genus_split, envir = .GlobalEnv)


















