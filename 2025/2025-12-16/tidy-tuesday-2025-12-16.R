
# About this script -------------------------------------------------------

#  Project: Tidy Tuesday
#  Purpose: Roundabouts across the world
#  Author:  
#  Date started: 
#  Last update: 



# Libraries ---------------------------------------------------------------

library(rio)
library(here)
library(tidyverse)
library(janitor)
library(lubridate)
library(GGally)
library(viridis)

library(conflicted)
conflicted::conflicts_prefer(dplyr::select)
conflicted::conflicts_prefer(dplyr::filter)

# Import data  ------------------------------------------------------------


# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2025-12-16')
## OR
#tuesdata <- tidytuesdayR::tt_load(2025, week = 50)

roundabouts_clean <- tuesdata$roundabouts_clean

# Option 2: Read directly from GitHub

#roundabouts_clean <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-12-16/roundabouts_clean.csv')



# Summary  ----------------------------------------------------------------

summary(roundabouts_clean)


# Exploratory data analysis -----------------------------------------------

tabyl(roundabouts_clean, country )
tabyl(roundabouts_clean, type)
tabyl(roundabouts_clean, year_completed)
tabyl(roundabouts_clean, approaches)
tabyl(roundabouts_clean, lane_type)

range(roundabouts_clean$year_completed)

roundabouts_clean |> 
  filter(year_completed >0) |> 
  ggplot(aes(x  = factor(year_completed))) + 
  geom_bar(stat = "count")

roundabouts_clean |> 
  ggplot(aes(x  = factor(lane_type))) + 
  geom_bar(stat = "count") + 
  coord_flip()


roundabouts_clean |> 
  ggplot(aes(x  = factor(approaches))) + 
  geom_bar(stat = "count") + 
  coord_flip()

roundabouts_clean |> 
  ggplot(aes(x  = country)) + 
  geom_bar(stat = "count") + 
  coord_flip()


















