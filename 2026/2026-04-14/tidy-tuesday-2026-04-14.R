
# About this script -------------------------------------------------------

# Purpose: Tidy Tuesday
# Project: Bird Sightings at Sea
# Author: Anna Wilkinson; anna.wilkinson@burnet.edu.au
# Date started: 16 April 2026



# Load libraries ----------------------------------------------------------

library(rio)
library(here)
library(janitor)
library(tidyverse)


# Load data ---------------------------------------------------------------

# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2026-04-14')
## OR
#tuesdata <- tidytuesdayR::tt_load(2026, week = 15)

beaufort_scale <- tuesdata$beaufort_scale
birds <- tuesdata$birds
sea_states <- tuesdata$sea_states
ships <- tuesdata$ships

# Option 2: Read directly from GitHub

# beaufort_scale <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-14/beaufort_scale.csv')
# birds <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-14/birds.csv')
# sea_states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-14/sea_states.csv')
# ships <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-14/ships.csv')


# Summary -----------------------------------------------------------------

glimpse(beaufort_scale)
glimpse(birds)
glimpse(sea_states)
glimpse(ships)
glimpse(ships)



# Join  -------------------------------------------------------------------

birds_ships <-  left_join(birds, ships, by = "record_id")
birds_ships <-  birds_ships %>% 
  select(record_id, date, time, everything())


# Exploratory data analysis -----------------------------------------------

tabyl(birds, species_common_name)

birds_ships %>% 
  mutate(year = lubridate::year(date)) %>% 
  summarise(sum_count = sum(count), .by = year) %>% 
  arrange(year)

birds_ships %>% 
  mutate(year = lubridate::year(date)) %>% 
  summarise(sum_count = sum(count), .by = c(year, species_common_name)) %>% 
  arrange(year)

birds_ships %>% 
  summarise(n = n(), .by = time) %>% 
  ggplot() + 
  geom_bar(aes(x = time, 
               y = n),
           stat = "identity")

birds_ships %>% 
  summarise(n = n(), .by = time) %>% 
  ggplot(aes(x = time, 
               y = n)) + 
  geom_point() + 
  geom_line()


birds_ships %>% 
  summarise(n = n(), .by = c(observer,time)) %>% 
  ggplot(aes(x = time, 
             y = n,
             group = observer,
             colour = observer)) + 
  geom_line()


birds_ships %>% 
  drop_na(season) %>% 
  summarise(n = n(), .by = c(season,time)) %>% 
  ggplot(aes(x = time, 
             y = n,
             group = season,
             fill = season)) + 
  geom_col(width = 500) +    # width must be in seconds - one hour wide is 3600 secs
  scale_x_time() + 
  facet_wrap(~ season)



# Pretty plot -------------------------------------------------------------


p <- birds_ships %>% 
  drop_na(season) %>% 
  summarise(n = n(), .by = c(season,time)) %>% 
  ggplot(aes(x = time, 
             y = n,
             group = stringr::str_to_sentence(season),
             fill = stringr::str_to_sentence(season))) + 
  geom_col(width = 500,  # width must be in seconds - one hour wide is 3600 secs
           fill = "darkgrey") +   
  labs(y = "Count of bird observations (n)",
       x = "Time of observation (24hr clock)",
       fill = "") + 
  scale_x_time(date_labels = "%H:%M") + 
  theme_bw() +
  theme(
    
    legend.position = "none",
    strip.background = element_rect(fill = "navy"),
    strip.text.x.top = element_text(face = "bold", 
                                    colour = "white")
  ) + 
  facet_wrap(~ fct_relevel(stringr::str_to_sentence(season), "Summer","Autumn","Winter","Spring"))

p
# Export plot -------------------------------------------------------------

ggsave(filename = "2026/2026-04-14/20260414.png",
       plot = p)

## END


