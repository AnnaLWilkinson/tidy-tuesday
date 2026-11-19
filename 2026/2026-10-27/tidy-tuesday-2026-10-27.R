
# About this script -------------------------------------------------------

# Purpose: 
# Project:
# Author:
# Date started: 


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(janitor)


# Load data ---------------------------------------------------------------

# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2026-10-27')
## OR
#tuesdata <- tidytuesdayR::tt_load(2026, week = 43)

ikea_places <- tuesdata$ikea_places
ikea_seating <- tuesdata$ikea_seating

# Option 2: Read directly from GitHub
# 
# ikea_places <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-10-27/ikea_places.csv')
# ikea_seating <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-10-27/ikea_seating.csv')
# 


# Exploratory data analysis -----------------------------------------------

ikea_seating |> 
  group_by(product_type) |> 
  count() |> 
  arrange(-n)

ikea_seating |> 
  ggplot() + 
  geom_histogram(aes(x = price_usd))

ikea_seating |> 
  ggplot() + 
  geom_density(aes(x = price_usd))

top_10_products <- ikea_seating |> 
  group_by(product_type) |> 
  count() |> 
  arrange(-n) |> 
  head(n=10) |> 
  pull(product_type)

ikea_seating |> 
  filter(product_type %in% top_10_products) |> 
  
  ggplot() + 
  geom_histogram(aes(x =  price_usd)) + 
  facet_wrap( ~ product_type)







