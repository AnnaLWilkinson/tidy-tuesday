
# About this script -------------------------------------------------------

#  Project: Tidy Tuesday
#  Purpose: Edible Plants Database
#  Author:  Anna Wilkinson; anna.wilkinson@burnet.edu.au
#  Date started: 3 Feb 2026
#  Last update: 



# Libraries ---------------------------------------------------------------

library(rio)
library(here)
library(tidyverse)
library(janitor)
library(lubridate)
library(GGally)
library(viridis)
library(patchwork)
library(ggdist)
library(baffle)
library(conflicted)
library(sysfonts)
library(showtextdb)
library(showtext)
library(paletteer)
conflicted::conflicts_prefer(dplyr::select)
conflicted::conflicts_prefer(dplyr::filter)

# Import data  ------------------------------------------------------------

# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2026-02-03')
## OR
#tuesdata <- tidytuesdayR::tt_load(2026, week = 5)

edible_plants <- tuesdata$edible_plants

# Option 2: Read directly from GitHub

#edible_plants <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-02-03/edible_plants.csv')


# Summary -----------------------------------------------------------------
summary(edible_plants)

edible_plants |> 
  tabyl(cultivation)

edible_plants |> 
  tabyl(cultivation)

edible_plants |> 
  tabyl(temperature_class)

edible_plants |> 
  tabyl(temperature_class, season)
  
edible_plants |> 
  tabyl(sensitivities)



# Data prep ---------------------------------------------------------------

edible_plants <- edible_plants |> 
  separate(temperature_germination, into = c("temp_germ_lower", "temp_germ_upper"), sep = "-" , remove = FALSE)



# Exploratory data analysis -----------------------------------------------

edible_plants |> 
  ggplot() + 
  geom_bar(aes(x = water),
           stat = "count")

unique(edible_plants$water)

edible_plants <- edible_plants |> 
  mutate(water = str_to_lower(water))

unique(edible_plants$water)

edible_plants |> 
  ggplot() + 
  geom_bar(aes(x = water),
           stat = "count")

edible_plants <-  edible_plants |> 
  mutate(water = fct_relevel(water, 
                             "very low",
                             "low", 
                             "medium",
                             "high",
                             "very high"))
table(edible_plants$water)


edible_plants |> 
  ggplot() + 
  geom_bar(aes(x = water),
           stat = "count")

edible_plants |> 
  ggplot() + 
  geom_bar(aes(x = sunlight),
           stat = "count")

edible_plants |> 
  ggplot() + 
  geom_bar(aes(x = temperature_class),
           stat = "count")


water <- table(edible_plants$water)
names(water)

baffle::waffle(water, 
               stacked = FALSE,
               gap = 1)
legend("top",
       legend = names(water),
       fill = palette.colors(length(water), "Set 1"))



# Waffle plot -------------------------------------------------------------

my_colors <- paletteer::paletteer_d("lisa::FridaKahlo")

water <- edible_plants |> 
  summarise(n = n(), .by = water) |> 
  mutate(prop = n/sum(n), 
         water = as.character(water))  


water |> 
  ggplot(aes(forcats::fct_reorder(water, prop), prop, fill = water)) + 
  geom_col(colour = 'black')  +
  scale_fill_brewer(palette = 'Dark2') +
  scale_y_continuous(labels = scales::label_percent(1)) +
  labs(x = '', y = '', caption = 'Water requirements prevalence among edible plants') +
  theme_bw() +
  theme(
    legend.position = 'none',
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )


water_waffle <-  water |> 
  mutate(
    remainder = prop * 100 - floor(prop * 100),
    floored = floor(prop *100)
  ) |> 
  arrange(desc(remainder)) |> 
  mutate(number = ifelse(100 - sum(floored) >=row_number(), floored +1, floored)) |> 
  arrange(prop)

## waffle plot function

font_add(family = "FontAwesome", regular = "fa-solid-900.ttf")
waffle_plot <- function(number, colour, colour_palette, symbol, symbol_size=8) {
  p <- expand.grid(x = 0:9,
                   y = 0:9) %>%
    rowwise() |>
    mutate(index = 1+sum(x * 10 + y >= cumsum(number)),
           col = colour[[index]]) |>
    ggplot(aes(x, y, color = forcats::fct_inorder(col))) +
    geom_text(label = symbol,
              family = 'FontAwesome',
              size = symbol_size) +
    scale_color_manual(values = colour_palette) +
    coord_equal() +
    theme_void() +
    labs(color = '') +
    theme(
      legend.position = 'top',
      legend.margin = margin(1, 3, 1, 1, unit = 'mm'),
      plot.margin = margin(3,3,3,3,unit = 'mm'),
      legend.background = element_rect(fill = 'grey100', color = 'grey')
    )
  return(p)
}


waffle_plot(number = water_waffle$number,
            colour = water_waffle$water,
            colour_palette = my_colors,
            symbol = '\uf0c8', symbol_size=7) +
  labs(caption='Water requirements prevalence among edible plants') 





