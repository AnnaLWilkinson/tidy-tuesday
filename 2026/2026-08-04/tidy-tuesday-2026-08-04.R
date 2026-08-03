
# About this script -------------------------------------------------------

# Purpose: Basotho Wool
# Project: Tidy Tuesday
# Author: Anna Wilkinson; anna.wilkinson@burnet.edu.au
# Date started: 3rd Aug 2026

# Load libraries ----------------------------------------------------------

library(tidyverse)


# Load data ---------------------------------------------------------------

# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2026-08-04')
## OR
#tuesdata <- tidytuesdayR::tt_load(2026, week = 31)

basotho_wool <- tuesdata$basotho_wool

# Option 2: Read directly from GitHub

#basotho_wool <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-08-04/basotho_wool.csv')


# Summary -----------------------------------------------------------------

summary(basotho_wool)


# Exploratory Data Analysis -----------------------------------------------

basotho_wool %>% 
  ggplot() + 
  geom_histogram((aes(x = alt_qty)))

basotho_wool %>% 
  ggplot() + 
  geom_histogram(aes(x = cifvalue))

basotho_wool %>% 
  ggplot() + 
  geom_histogram(aes(x = fobvalue))

basotho_wool %>% 
  ggplot() + 
  geom_histogram(aes(x = primary_value))




