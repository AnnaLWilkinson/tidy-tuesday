# About this script -------------------------------------------------------

#  Project: Tidy Tuesday
#  Purpose: The 2026 Winter Olympics
#  Author:  Anna Wilkinson; anna.wilkinson@burnet.edu.au
#  Date started: 10 Feb 2026
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
library(conflicted)
library(cowplot)
library(magick)
library(png)

conflicted::conflicts_prefer(dplyr::select)
conflicted::conflicts_prefer(dplyr::filter)

# Import data  ------------------------------------------------------------


# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2026-02-10')
## OR
#tuesdata <- tidytuesdayR::tt_load(2026, week = 6)

schedule <- tuesdata$schedule

# Option 2: Read directly from GitHub

#schedule <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-02-10/schedule.csv')



# Summary -----------------------------------------------------------------

summary(schedule)
View(schedule)

schedule %>% 
  tabyl(discipline_name)

schedule %>% 
  tabyl(is_medal_event)

schedule %>% 
  tabyl(discipline_name, is_medal_event)


schedule %>% 
  summarise(n = n(), .by = start_datetime_local) %>% 
  arrange(-n)


schedule %>% 
  summarise(n = n(), .by = event_description) %>% 
  arrange(-n)



# Processing --------------------------------------------------------------


schedule <- schedule %>% 
  mutate(gender = case_when(
    str_detect(event_description, "Men")       ~ "Men's",
    str_detect(event_description, "man")       ~ "Men's",
    str_detect(event_description, "Women")     ~ "Women's",
    str_detect(event_description, "woman")     ~ "Women's",
    str_detect(event_description, "Mixed")     ~ "Mixed",
    str_detect(event_description, "Team")      ~ "Team or pair",
    str_detect(event_description, "Pair")      ~ "Team or pair",
    TRUE ~ "Unspecified"
  ))

schedule %>% 
  tabyl(gender)

schedule %>% 
  select(gender, event_description) %>% 
  filter(gender == "CHECK") %>% 
  View()


# Exploratory data analysis -----------------------------------------------

schedule %>% 
  summarise(n = n(), .by = event_description) %>% 
  ggplot() + 
  geom_col(aes(x = event_description,
               y = n)) + 
  coord_flip()


schedule %>% 
  summarise(n = n(), .by = gender) %>% 
  ggplot() + 
  geom_col(aes(x = reorder(gender, n),
               y = n))


# Pretty plot -------------------------------------------------------------

# Watermark

img <- png::readPNG(here::here("2026", "2026-02-10", "olympic_rings.png"))
# Optional: adjust transparency (alpha value from 0 to 1)

rast <-  grid::rasterGrob(img, interpolate = T)


schedule %>% 
  summarise(n = n(), .by = gender) %>% 
  ggplot() + 
  annotation_custom(rast,
                    ymin = 0, 
                    ymax = 900) + 
  geom_col(aes(x = reorder(gender, n),
               y = n),
           alpha = 0.8) + 
  labs(x = "", 
       y = "Frequency of events (n)",
       title = "Is there gender equity in Winter Olympics events?") + 
  theme_classic()


## Using cowplot

logo_file <- png::readPNG(here::here("2026", "2026-02-10", "olympic_rings.png"))

p <- schedule %>% 
  summarise(n = n(), .by = gender) %>% 
  ggplot() + 
  geom_col(aes(x = reorder(gender, n),
               y = n),
           alpha = 0.9,
           fill = "lightblue") + 
  labs(x = "", 
       y = "Frequency of events (n)",
       title = "Is there gender equity in Winter Olympics events?") +
  theme_cowplot()


final_p <- cowplot::ggdraw() + 
 cowplot::draw_image(logo_file, scale = 0.7) +
 draw_plot(p)  


# Save plot ---------------------------------------------------------------

ggsave(filename = here::here("2026", "2026-02-10", "20260210.png"),
       plot = final_p,
       dpi = 300)


## END



