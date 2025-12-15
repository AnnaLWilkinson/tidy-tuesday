

# About this script -------------------------------------------------------

# Purpose: Tidy Tuesday 
# Project: World TB data
# Author: Anna Wilkinson; anna.wilkinson@burnet.edu.au
# Date started: 28 Nov 2025
# Date updated: 


# Load libraries ----------------------------------------------------------

pacman::p_load(tidyverse,
               rio,
               janitor,
               here,
               fishualize,
               lubridate)



# Import data -------------------------------------------------------------

# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2025-11-11')
## OR
#tuesdata <- tidytuesdayR::tt_load(2025, week = 45)

who_tb_data <- tuesdata$who_tb_data

# Option 2: Read directly from GitHub

#who_tb_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-11-11/who_tb_data.csv')



# Explore -----------------------------------------------------------------

glimpse(who_tb_data)
skimr::skim(who_tb_data)
View(who_tb_data)

who_tb_data |> 
  group_by(country) |> 
  count()
  
range(who_tb_data$year)  

# Plot --------------------------------------------------------------------

myanmar_df <- who_tb_data |> 
  filter(country == "Myanmar")

myanmar_df |> 
  ggplot(mapping = aes(x = year ,
                       y = e_inc_100k )) +
  geom_line()

# have each year written on the x axis
myanmar_df |> 
  ggplot(mapping = aes(x = factor(year) ,
                       y = e_inc_100k ,
                       group = 1)) +
  geom_line()


# start the y axis at 0 & end at 600
myanmar_df |> 
  ggplot(mapping = aes(x = factor(year) ,
                       y = e_inc_100k ,
                       group = 1)) +
  geom_line() +
  scale_y_continuous(limits = c(0,600))



# horizontal line at 500
myanmar_df |> 
  ggplot(mapping = aes(x = factor(year) ,
                       y = e_inc_100k ,
                       group = 1)) +
  geom_line() + 
  geom_hline(yintercept = 500,
             linetype = 2,
             colour = "blue") + 
  scale_y_continuous(limits = c(0,600))


# coding challenge! show the period that COVID-19 impacted TB data
# geom_vline
# geom_annotate
# geom_rect


# if you have time - be great to try:

# 1. add labels, including a title, y axis, and x axis labels
# 2. change the theme, so the background is not grey
# 3. adding geom_points to the line as well

# END



