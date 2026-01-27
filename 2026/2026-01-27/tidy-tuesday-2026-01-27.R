
# About this script -------------------------------------------------------

#  Project: Tidy Tuesday
#  Purpose: Brazilian Companies
#  Author:  Anna Wilkinson; anna.wilkinson@burnet.edu.au
#  Date started: 27 jan 2026
#  Last update: 



# Libraries ---------------------------------------------------------------

library(rio)
library(here)
library(tidyverse)
library(janitor)
library(lubridate)
library(GGally)
library(viridis)
library(patchwork)

library(conflicted)
conflicted::conflicts_prefer(dplyr::select)
conflicted::conflicts_prefer(dplyr::filter)

# Import data  ------------------------------------------------------------

# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2026-01-27')
## OR
tuesdata <- tidytuesdayR::tt_load(2026, week = 4)

companies <- tuesdata$companies
legal_nature <- tuesdata$legal_nature
qualifications <- tuesdata$qualifications
size <- tuesdata$size

# Option 2: Read directly from GitHub

companies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-01-27/companies.csv')
legal_nature <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-01-27/legal_nature.csv')
qualifications <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-01-27/qualifications.csv')
size <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-01-27/size.csv')