
#  About this script ------------------------------------------------------

#  Project:
#  Purpose: 
#  Author: 
#  Date started: 
#  Last updated: 


# https://github.com/rfordatascience/tidytuesday/blob/main/data/2025/2025-11-18/readme.md
# The Complete Sherlock Holmes

# Libraries ---------------------------------------------------------------

pacman::p_load(rio, 
               here, 
               tidyverse,
               janitor,
               lubridate,
               gtsummary
               )


# Import data -------------------------------------------------------------

# Imports
library(tidyverse)
library(devtools)

devtools::install_github("EmilHvitfeldt/sherlock")
library(sherlock)

# Load the dataset
holmes <- sherlock::holmes |>
  # Add line numbers to preserve narrative order
  mutate(line_num = row_number(), .by = "book") |>
  # Reorder columns
  select("book", "text", "line_num")






