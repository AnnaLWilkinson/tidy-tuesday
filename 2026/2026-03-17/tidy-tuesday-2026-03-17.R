

# About this script -------------------------------------------------------

#  Project:  Tidy Tuesday
#  Purpose:  Salmonid Mortality Data
#  Author:   Anna Wilkinson; anna.wilkinson@burnet.edu.au
#  Date started: 17 March 2026

# Notes on the data:

# Fish Health Report on health and welfare of farmed fished. The Norwegian government goal is to push for lower mortality.
# Libraries ---------------------------------------------------------------

pacman::p_load(rio,
               here,
               tidyverse,
               janitor,
               patchwork)


# Import data -------------------------------------------------------------

# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2026-03-17')
## OR
tuesdata <- tidytuesdayR::tt_load(2026, week = 11)

monthly_losses_data <- tuesdata$monthly_losses_data
monthly_mortality_data <- tuesdata$monthly_mortality_data

# Option 2: Read directly from GitHub

#monthly_losses_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-03-17/monthly_losses_data.csv')
#monthly_mortality_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-03-17/monthly_mortality_data.csv')

