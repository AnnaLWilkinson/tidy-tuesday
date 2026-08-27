
# About this script -------------------------------------------------------


#  Purpose: Country Music Lyrics
#  Project: Tidy Tuesday
#  Author: Anna Wilkinson; anna.wilkinson@burnet.edu.au
#  Date started: 27 August 2026


# Load libraries ----------------------------------------------------------

library(tidyverse)


# Load data ---------------------------------------------------------------

# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2026-08-25')
## OR
#tuesdata <- tidytuesdayR::tt_load(2026, week = 34)

country_lyrics <- tuesdata$country_lyrics
top_all_writers <- tuesdata$top_all_writers
top_primary_writers <- tuesdata$top_primary_writers
top_producers <- tuesdata$top_producers

# Option 2: Read directly from GitHub

# country_lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-08-25/country_lyrics.csv')
# top_all_writers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-08-25/top_all_writers.csv')
# top_primary_writers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-08-25/top_primary_writers.csv')
# top_producers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-08-25/top_producers.csv')


