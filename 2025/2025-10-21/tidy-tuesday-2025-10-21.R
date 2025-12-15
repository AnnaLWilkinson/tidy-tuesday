
#  About this script ------------------------------------------------------

# Project: Tidy Tuesday
# Purpose: Historic UK Meteorological & Climate Data
# Author: Anna Wilkinson; anna.wilkinson@burnet.edu.au
# Date started: 20 October 2025

# https://github.com/rfordatascience/tidytuesday/blob/main/data/2025/2025-10-21/readme.md

# Libraries ---------------------------------------------------------------

pacman::p_load(rio, 
               here, 
               tidyverse,
               janitor,
               lubridate,
               gtsummary,
               ggdist,
               randomcoloR,
               beeswarm,
               sf,
               raster,
               spData,
               tmap,
               ggdibbler,
               distributional
               )
#install.packages("spDataLarge", repos = "https://geocompr.r-universe.dev")
library(spDataLarge)

# Import data -------------------------------------------------------------

# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2025-10-21')
## OR
#tuesdata <- tidytuesdayR::tt_load(2025, week = 42)

historic_station_met <- tuesdata$historic_station_met
station_meta <- tuesdata$station_meta

# Option 2: Read directly from GitHub

# historic_station_met <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-10-21/historic_station_met.csv')
# station_meta <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-10-21/station_meta.csv')



# Summary -----------------------------------------------------------------

summary(historic_station_met)
summary(station_meta)



# Exploratory data analysis -----------------------------------------------

historic_station_met |> 
 ggplot() + 
  geom_point(aes(x = factor(month), 
                 y = tmax))


historic_station_met |> 
  ggplot() + 
  geom_line(aes(x = factor(month), 
                y = tmax,
                group = station))


historic_station_met |> 
  ggplot() + 
  geom_line(aes(x = factor(month), 
                y = tmax,
                group = year))


historic_station_met |> 
  ggplot() + 
  geom_point(aes(x = factor(month), 
                y = tmax,
                group = factor(year),
                colour = factor(year))) + 
  theme(
    legend.position = "none"
  )

historic_station_met |> 
  ggplot() + 
  geom_line(aes(x = factor(month), 
                 y = tmax,
                 group = factor(year),
                 colour = factor(year))) + 
  theme(
    legend.position = "none"
  )



#  Rainfall ---------------------------------------------------------------

historic_station_met |> 
  summarise(mean_rain = mean(rain, na.rm = TRUE), .by = c(month,year)) |> 
  ggplot() + 
  geom_point(aes(x = factor(month),
                 y = mean_rain, 
                 group = year))


historic_station_met |> 
  summarise(mean_rain = mean(rain, na.rm = TRUE), .by = c(month,year)) |> 
  ggplot() + 
  geom_line(aes(x = factor(month),
                 y = mean_rain, 
                 group = year))


historic_station_met |> 
  ggplot(aes(y = factor(month),
             x = rain,
             fill = factor(month))) +
  stat_slab() 


historic_station_met |> 
  ggplot(aes(y = factor(month),
             x = rain,
             fill = factor(month))) +
  stat_dotsinterval(side = "bottom", scale = 0.7, slab_linewidth = NA) +
  scale_fill_manual(values = randomColor(12))


historic_station_met |> 
  ggplot(aes(y = factor(month),
             x = rain,
             fill = factor(month))) +
  stat_dots(position = "dodge", color = NA, layout = "weave") +
  scale_fill_manual(values = randomColor(12))


historic_station_met |> 
  ggplot(aes(y = factor(month),
             x = rain,
             fill = factor(month))) +
  geom_dots(binwidth = unit(c(1, Inf), "mm"), overflow = "compress", alpha = 0.75) +
  scale_fill_manual(values = randomColor(12))


historic_station_met |> 
  ggplot(aes(x = factor(month),
             y = rain,
             fill = factor(month))) + 
  geom_weave(position = "dodge") + 
  scale_fill_manual(values = randomColor(12))
  

historic_station_met |> 
  ggplot(aes(x = factor(month),
             y = rain,
             colour = factor(month))) + 
  geom_swarm() + 
  scale_fill_manual(values = randomColor(12))


set.seed(2025-10-20-1605)
historic_station_met |> 
  ggplot(aes(x = factor(month),
             y = rain)) + 
  geom_dots(binwidth = unit(c(1, Inf), "mm"),
            overflow = "compress") + 
  scale_colour_grey() + 
  theme_minimal() 

historic_station_met |> 
  ggplot(aes(x = factor(month),
             y = rain)) + 
  geom_dots(binwidth = unit(c(1, Inf), "mm"),
            overflow = "compress",
            side = "both") + 
  scale_colour_grey() + 
  theme_minimal() 


historic_station_met |> 
  summarise(mean_rain = mean(rain, na.rm = TRUE), .by = station) |> 
  arrange(desc(mean_rain))


# use the station with the highest av rain

historic_station_met |> 
  filter(station == 'cwmystwyth') |> 
  
  ggplot(aes(y = factor(month),
             x = rain,
             fill = factor(month))) +
  stat_slab(scale = 0.7) + 
  stat_dotsinterval(side = "bottom",
                    scale = 0.7,
                    slab_linewidth = NA)


# Pretty plot -------------------------------------------------------------


# use the station with the highest av rain

historic_station_met |> 
  filter(station == 'cwmystwyth') |>
  mutate(month = factor(month.abb[month], levels = month.abb)) |> 
    
    
  ggplot(aes(y = factor(month),
             x = rain,
             fill = factor(month))) +
  stat_slab(scale = 0.7) + 
  stat_dotsinterval(side = "bottom",
                    scale = 0.7,
                    slab_linewidth = NA) + 
  
  labs(title = "Rain in Cwmystwyth (a village in Wales)",
       x = "Total rainfall (mm)", 
       y = "")  + 
  
  scale_fill_grey() + 
  
  theme_minimal() + 
  theme(
    
    legend.position = "none"
  )



p <- historic_station_met |> 
  filter(station == 'cwmystwyth') |>
  mutate(month = factor(month.abb[month], levels = month.abb)) |> 
  
  
  ggplot(aes(y = forcats::fct_rev(month),
             x = rain)) +
  stat_slab(scale = 0.6) + 
  stat_dotsinterval(side = "bottom",
                    scale = 0.6,
                    slab_linewidth = NA,
                    colour = "blue",
                    fill = "lightblue",
                    overflow = "compress") + 
  
  labs(title = "Rain in Cwmystwyth (a village in Wales)",
       x = "Total rainfall (mm)", 
       y = "")  + 
  

  theme_minimal() + 
  theme(
    
    legend.position = "none"
  )
p

ggsave(filename = here::here("figs", "weather_stations.jpeg"),
       plot = p)





# Map ---------------------------------------------------------------------

# simple maps: https://simplemaps.com/gis/country/gb
my_sf <- read_sf(here::here("data", "simple_maps_uk", "gb.shp"))
class(my_sf)

my_sf |> 
  ggplot() + 
  geom_sf() + 
  theme_void()


# UK Climate project
my_uk_land_region <-  read_sf(here::here("data", "ukcp18-uk-land-region-hires.shp"))


# office of national statistics UK
my_uk_regions  <-  read_sf(here::here("data", "national_stats_uk", "RGN_DEC_24_EN_BFC.shp"))
class(my_uk_regions)

my_uk_regions |> 
  ggplot() + 
  geom_sf() + 
  theme_void()


# mean and variance in the data
historic_station_met_est <-  historic_station_met |> 
  summarise(rain_mean = mean(rain, na.rm = TRUE),
            rain_se   = sd(rain,   na.rm = TRUE)/sqrt(n()), .by = station)
  

historic_station_met_dist <-  historic_station_met_est |> 
  mutate(rain_dist = distributional::dist_normal(rain_mean, rain_se)) 



historic_station_met_dist |> 
  mutate(station = str_to_title(station)) |> 
  ggplot(aes( y = reorder(station, -rain_mean))) + 
  stat_halfeye(aes(dist = rain_dist), 
               show.legend = TRUE) + 
  
  labs(title = "Distribution of rain measurements at UK weather stations", 
       y = 'Station name', 
       x = "Rain (mm)") + 
  theme_bw()



