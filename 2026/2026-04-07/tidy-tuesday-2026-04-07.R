
# About this script -------------------------------------------------------

#  Project:  Tidy Tuesday
#  Purpose:  Repair Cafes Worldwide
#  Author:   Anna Wilkinson; anna.wilkinson@burnet.edu.au
#  Date started: 7 April 2026


# Load Libraries ---------------------------------------------------------------
library(rio)
library(here)
library(tidyverse)
library(showtext)
library(ggtext)
library(lubridate)


# Load data -------------------------------------------------------------

# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2026-04-07')
## OR
#tuesdata <- tidytuesdayR::tt_load(2026, week = 14)

repairs <- tuesdata$repairs
repairs_text <- tuesdata$repairs_text

# # Option 2: Read directly from GitHub
# 
# repairs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-07/repairs.csv')
# repairs_text <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-07/repairs_text.csv')
# 



# Load fonts --------------------------------------------------------------

# font_add_google("Oswald")
# font_add_google("Nunito")
# showtext_auto()
# showtext_opts(dpi = 300)
# title_font <- "Oswald"
# body_font <- "Nunito"

showtext_auto(FALSE)

# Summary  ----------------------------------------------------------------

summary(repairs)
tabyl(repairs, country)
tabyl(repairs, kind_of_product)

repairs %>% 
  summarise(n = n(), .by = kind_of_product) %>% 
  arrange(-n)

repairs %>% 
  summarise(n = n(), .by = kind_of_product) %>% 
  arrange(n)


tabyl(repairs, category)
tabyl(repairs, repaired)


# Exploratory data analysis -----------------------------------------------

repairs %>% 
  ggplot() + 
  geom_bar(aes(x = repaired),
           stat = "count")


repairs %>% 
  ggplot() + 
  geom_bar(aes(x = repaired),
           stat = "count") + 
  facet_wrap(~ category)


repairs %>% 
  ggplot() + 
  geom_bar(aes(y = repairability)) + 
  facet_wrap(~ category)


repairs %>% 
  mutate(repair_year = year(repair_date)) %>% 
  ggplot() + 
  geom_jitter(aes(y = repairability, 
                 x = factor(repair_year)))


repairs %>% 
  mutate(repair_year = year(repair_date)) %>% 
  ggplot() + 
  geom_jitter(aes(y = repairability, 
                  x = factor(repair_year)),
              alpha = 0.4) + 
  facet_wrap(~ category)




