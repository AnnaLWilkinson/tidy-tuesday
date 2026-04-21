
# About this script -------------------------------------------------------

# Purpose: Tidy Tuesday
# Project: Bird Sightings at Sea
# Author: Anna Wilkinson; anna.wilkinson@burnet.edu.au
# Date started: 16 April 2026



# Load libraries ----------------------------------------------------------

library(rio)
library(here)
library(janitor)
library(tidyverse)


# Load data ---------------------------------------------------------------

# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2026-04-14')
## OR
#tuesdata <- tidytuesdayR::tt_load(2026, week = 15)

beaufort_scale <- tuesdata$beaufort_scale
birds <- tuesdata$birds
sea_states <- tuesdata$sea_states
ships <- tuesdata$ships

# Option 2: Read directly from GitHub

# beaufort_scale <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-14/beaufort_scale.csv')
# birds <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-14/birds.csv')
# sea_states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-14/sea_states.csv')
# ships <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-14/ships.csv')


# Summary -----------------------------------------------------------------

glimpse(beaufort_scale)
glimpse(birds)
glimpse(sea_states)
glimpse(ships)
glimpse(ships)



# Join  -------------------------------------------------------------------

birds_ships <-  left_join(birds, ships, by = "record_id")
birds_ships <-  birds_ships %>% 
  select(record_id, date, time, everything())


# Exploratory data analysis -----------------------------------------------

tabyl(birds, species_common_name)

birds_ships %>% 
  mutate(year = lubridate::year(date)) %>% 
  summarise(sum_count = sum(count), .by = year) %>% 
  arrange(year)

birds_ships %>% 
  mutate(year = lubridate::year(date)) %>% 
  summarise(sum_count = sum(count), .by = c(year, species_common_name)) %>% 
  arrange(year)

















