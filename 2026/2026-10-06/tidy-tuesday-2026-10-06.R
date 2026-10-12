
# About this script -------------------------------------------------------

#  Purpose: Tidy Tuesday 
#  Project: Avocado Oil Authenticity
#  Author: Anna Wilkinson; anna.wilkinson@burnet.edu.au
#  Date started: 6 October 2026


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(treemap)




# Load data ---------------------------------------------------------------

# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2026-10-06')
## OR
#tuesdata <- tidytuesdayR::tt_load(2026, week = 40)

avocado_oil_bottles <- tuesdata$avocado_oil_bottles
avocado_oil_processed_foods <- tuesdata$avocado_oil_processed_foods

# Option 2: Read directly from GitHub

# avocado_oil_bottles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-10-06/avocado_oil_bottles.csv')
# avocado_oil_processed_foods <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-10-06/avocado_oil_processed_foods.csv')


# Summary -----------------------------------------------------------------

summary(avocado_oil_bottles)

avocado_oil_bottles |> 
  count(grade_labeled)

avocado_oil_bottles |> 
  count(purity_result)

avocado_oil_processed_foods |> 
  count(category, authentic)


avocado_oil_processed_foods |> 
  count(oil_type, authentic)

avocado_oil_processed_foods |> 
  ggplot() + 
  geom_point(aes(x = package_size_oz,
                 y = retail_price_usd, 
                 colour = authentic)) + 
  facet_wrap(~category)







