# About this script -------------------------------------------------------

#  Project: Tidy Tuesday
#  Purpose: Agriculture Production Statistics in New Zealand
#  Author:  Anna Wilkinson; anna.wilkinson@burnet.edu.au
#  Date started: 17 Feb 2026
#  Last update: 



# Libraries ---------------------------------------------------------------
library(tidytuesdayR)
library(rio)
library(here)
library(tidyverse)
library(janitor)
library(lubridate)
library(GGally)
library(viridis)
library(patchwork)
library(ggdist)
library(paletteer)
library(fontawesome)
library(scales)
conflicted::conflicts_prefer(dplyr::select)
conflicted::conflicts_prefer(dplyr::filter)

# Import data  ------------------------------------------------------------


# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2026-02-17')
## OR
#tuesdata <- tidytuesdayR::tt_load(2026, week = 7)

dataset <- tuesdata$dataset

# Option 2: Read directly from GitHub

#dataset <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-02-17/dataset.csv')



# Summary -----------------------------------------------------------------

summary(dataset)

dataset %>% 
  tabyl(measure)

dataset %>% 
 distinct(year_ended_june) %>% 
 count()

dataset %>% 
  tabyl(year_ended_june)

dataset %>% 
  summarise(n = n(), .by = measure) %>% 
  arrange(-n) %>% 
  print(n = 20)



# Exploratory data analysis -----------------------------------------------

dataset %>% 
  filter(measure == "Wheat (yield)") %>% 
  ggplot() + 
  geom_bar(aes(x = year_ended_june,
               y = value),
           stat = "identity")


dataset %>% 
  filter(measure == "Barley (yield)") %>% 
  ggplot() + 
  geom_bar(aes(x = year_ended_june,
               y = value),
           stat = "identity")

dataset %>% 
  filter(measure == "Oats (yield)") %>% 
  ggplot() + 
  geom_bar(aes(x = year_ended_june,
               y = value),
           stat = "identity")

dataset %>% 
  filter(measure == "Maize (yield)") %>% 
  ggplot() + 
  geom_bar(aes(x = year_ended_june,
               y = value),
           stat = "identity")


dataset %>% 
  filter(measure == "Feijoas") %>% 
  ggplot() + 
  geom_bar(aes(x = year_ended_june, 
               y = value),
           stat = "identity")

## Fruits
# Tamarillos
# Tangelos
# Persimmons
# Kiwifruit
# Asian Pears (Nashi)
# 
# 
# Raspberries
# Strawberries
# Blueberries 
# Blackcurrants
# Blackberries/Brambles
# Boysenberries
# 
# 
# Passionfruit
# Apples
# 
# Peaches
# Nectarines
# Cherries
# Apricots
# 
# Lime
# Lemons
# Oranges
# Grapefruit/Goldfruit


fruits <- c(
  "Tamarillos",
  "Tangelos",
  "Persimmons",
  "Kiwifruit",
  "Asian Pears (Nashi)",
  
  "Raspberries",
  "Strawberries",
  "Blueberries",
  "Blackcurrants",
  "Blackberries/Brambles",
  "Boysenberries",
  
  "Passionfruit",
  "Apples",
  
  "Peaches",
  "Nectarines",
  "Cherries",
  "Apricots",
  
  "Lime",
  "Lemons",
  "Oranges",
  "Grapefruit/Goldfruit"
)

berries <- c("Raspberries",
   "Strawberries",
   "Blueberries",
   "Blackcurrants",
   "Blackberries/Brambles",
   "Boysenberries")


dataset %>% 
  filter(measure %in% berries) %>% 
  ggplot() + 
  geom_bar(aes(x = year_ended_june,
               y = value),
           stat = "identity") + 
  facet_wrap( ~measure)


citrus <-  c("Lime",
   "Lemons",
   "Oranges",
   "Grapefruit/Goldfruit")

## note that limes are in tonnes
dataset %>% 
  filter(measure %in% citrus) %>% 
  ggplot() + 
  geom_bar(aes(x = year_ended_june,
               y = value),
           stat = "identity") + 
  facet_wrap( ~measure,
              scales = "free_y")


stone_fruit <-  c("Peaches",
   "Nectarines",
   "Cherries",
   "Apricots")


dataset %>% 
  filter(measure %in% stone_fruit) %>% 
  ggplot() + 
  geom_bar(aes(x = year_ended_june,
               y = value),
           stat = "identity") + 
  facet_wrap( ~measure,
              scales = "free_y")


unusual_fruits <-  c("Tamarillos",
   "Tangelos",
   "Persimmons")


dataset %>% 
  filter(measure %in% unusual_fruits) %>% ## number per hectare
  ggplot() + 
  geom_bar(aes(x = year_ended_june,
               y = value),
           stat = "identity") + 
  facet_wrap( ~measure,
              scales = "free_y")




dataset %>% 
  filter(measure %in% fruits) %>% 
  filter(value_label == "Hectares") %>% 
  
  summarise(sum_value = sum(value), .by = measure) %>% 
  ggplot() + 
  geom_col(aes(x = reorder(measure, sum_value),
           y = sum_value)) +
  coord_flip()
    
  
dataset %>% 
  filter(measure =="Kiwifruit") %>% 
  ggplot() + 
  geom_bar(aes(x = year_ended_june,
               y = value),
           stat = "identity")



# Pretty plot -------------------------------------------------------------

kiwi_palette <- c(
  vibrant_kiwi_green = "#8DC73F",
  kiwi_green_artyclick = "#8EE53F",
  light_kiwi_green = "#90C825",
  kiwi_skin_brown = "#91631D",
  kiwi_pulp_yellow_green = "#A08556"
)

  
# Kiwifruit

p_1 <- dataset %>% 
  filter(measure %in% fruits) %>% 
  filter(value_label == "Hectares") %>% 
  
  summarise(sum_value = sum(value), .by = measure) %>% 
  ggplot() + 
  geom_col(aes(x = reorder(measure, sum_value),
               y = sum_value), 
           fill = "#963FE5") +
  scale_y_continuous(labels = scales::label_number(scale_cut = cut_short_scale())) + 
  labs(title = "Do Kiwi's still produce kiwi's?",
       y = "Number of units per hectare", 
       x = "", 
       caption = "One hectare is about two soccer fields") + 
  theme_bw() + 
  coord_flip()


p_2 <- dataset %>% 
  filter(measure =="Kiwifruit") %>% 
  ggplot() + 
  geom_bar(aes(x = year_ended_june,
               y = value),
           stat = "identity", 
           width = .9, 
           colour = kiwi_palette[4],
           fill = kiwi_palette[1]) +
  scale_y_continuous(labels = scales::label_number(scale_cut = cut_short_scale())) + 
  
  labs(title = "Kiwifruit production over time",
       y = "Number of units per hectare",
       x = "") + 
  theme_bw()


p_1 + p_2


# Save plot ---------------------------------------------------------------

ggsave(filename = here::here("2026", "2026-02-17", "20260217.png"),
       plot = last_plot(),
       dpi = 300)


## END

