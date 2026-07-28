
# About this script -------------------------------------------------------

#  Purpose: Ecotourism 
#  Project:  tidy Tuesday 
#  Author:   Anna Wilkinson; anna.wilkinson@burnet.edu.au
#  Date started: 27 july 2026


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(sf)
library(ggview)


# Load data ---------------------------------------------------------------

# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2026-07-28')
## OR
#tuesdata <- tidytuesdayR::tt_load(2026, week = 30)

occurrences <- tuesdata$occurrences
tourism <- tuesdata$tourism
weather <- tuesdata$weather

# Option 2: Read directly from GitHub

#occurrences <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-07-28/occurrences.csv')
#tourism <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-07-28/tourism.csv')
#weather <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-07-28/weather.csv')


# Summary -----------------------------------------------------------------

glimpse(tourism)
glimpse(occurrences)
glimpse(weather)


tourism %>% 
  count(region) %>% 
  arrange(-n)

occurrences %>% 
  count(organism_name) %>% 
  arrange(-n)


# Mapping -----------------------------------------------------------------

world <- sf::st_read("C:/Users/anna.wilkinson/Documents/Mapping/ne_10m_land/ne_10m_land.shp")
class(world)
st_geometry(world)

base_map <- ggplot(data = world) + 
  geom_sf(fill = "white", 
          colour = "grey50") + 
  theme_minimal(base_size = 11.5)  
base_map

# create df of observations of organisms
map_organism <- occurrences %>% 
  select(organism_name,
         obs_lat, 
         obs_lon)

# convert to spatial object
map_organism_sf <- st_as_sf(map_organism, 
                             coords = c("obs_lon", "obs_lat"), 
                             crs = 4326)

base_map + 
  geom_sf(
    data = map_organism_sf,
    aes(colour = organism_name)
  ) + 
  coord_sf(
    xlim = c(112, 156), 
    ylim = c(-44,-10),
    expand = FALSE
  )

## colours
org_palette <-  c("#00CFEF", "#F0C419", "#FFF", "#8E44AD")

base_map + 
  geom_sf(
    data = map_organism_sf,
    aes(colour = organism_name),
    alpha = 0.5, 
    size = 2.5
  ) + 
  
  scale_colour_manual(values = org_palette) + 
  
  coord_sf(
    xlim = c(112, 156), 
    ylim = c(-44,-10),
    expand = FALSE
  ) + 
  
  theme(
    
    legend.position = "inside",
    legend.position.inside = c(0.50, 0.60), 
    legend.background = element_rect(fill = "transparent"), 
    legend.key = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "navy", colour = NA), 
    panel.background = element_rect(fill = "navy", colour = NA),
    panel.border = element_blank(),
    axis.text = element_blank(), 
    plot.title = element_text(colour = "white", face = "bold", size = 20), 
    plot.caption = element_text(colour = "white"),
    plot.margin = margin(c(t=30, r=100, b=100, l=100 , unit = "pt")),

  ) + 
  
  labs(
    title = "Where to go to see natural wonders!",
    caption = "Ecotourism r package.\nCook D, Cook L, Vahdat Atashgah J (2025). \necotourism: Collection of data on records of wild life sightings, tourism counts and \nweather from Australia. R package version 0.0.0.9000.",
    colour = ""
  ) + 
  
  canvas(
    
    width = 8, 
    height = 8,
    units = "in", 
    scale = 1, 
    dpi = 300, 
    bg = "navy"
  )










