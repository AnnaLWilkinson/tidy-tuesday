
# About this script -------------------------------------------------------

#  Project: Tidy Tuesday
#  Purpose: Italian industrial production
#  Author:  Anna Wilkinson; anna.wilkinson@burnet.edu.au
#  Date started:  6 May 2026
#  Date last changed:  



# Load libraries ----------------------------------------------------------

library(rio)
library(here)
library(lubridate)
library(janitor)
library(tidyverse)


# Load data ---------------------------------------------------------------

# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2026-05-05')
## OR
#tuesdata <- tidytuesdayR::tt_load(2026, week = 18)

food_beverages <- tuesdata$food_beverages
textiles <- tuesdata$textiles
transport <- tuesdata$transport

# Option 2: Read directly from GitHub

# food_beverages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-05-05/food_beverages.csv')
# textiles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-05-05/textiles.csv')
# transport <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-05-05/transport.csv')
# 

# Summary of the data -----------------------------------------------------

summary(textiles)
summary(transport)
summary(food_beverages)

# Exploratory data analysis -----------------------------------------------

# notes on year: 
# The figures for the years between 1871 and 1950 refer to the fiscal year, which 
# does not necessarily coincide with the calendar year; in particular, for the years 
# between 1931 to 1950 the fiscal year began on 1st July. From 1951 onwards, 
# the figures refer to the calendar year.

food_beverages %>% 
  filter(Year>=1951) %>% 
  ggplot() + 
  geom_line(aes(x = Year,
               y = Sugar)) 
  
  geom_line(aes(x = Year,
                y = Glucose)) 


# write a plot function
  
my_lineplot_func_food <-  function(y_var) {

   food_beverages %>% 
    filter(Year>=1951) %>% 
    ggplot() + 
    geom_line(aes(x = Year,
                  y = .data[[y_var]])) +
    labs(title = y_var)
}

lapply(c("Sugar", "Glucose", "Coffee_substitute", "Seed_oil", 
         "Ethyl_alcohol_1", "Ethyl_alcohol_2", "Beer"), my_lineplot_func_food)
  
  

food_beverages %>% 
  pivot_longer(cols = -Year, 
               values_to = "amount",
               names_to = "product") %>% 
  filter(Year >=1951) %>% 
  ggplot() + 
    geom_line(aes(x = Year, 
                  y = amount,
                  group = product)) + 
    facet_grid(~ product)



  
  
  
  
  
  
  
  
  


