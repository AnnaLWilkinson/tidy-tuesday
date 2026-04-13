
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
library(janitor)
library(gganimate)
library(gifski)


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
              alpha = 0.2) + 
  facet_wrap(~ category)


repairs %>% 
  filter(category %in% c("Bicyles", "Furniture", 
                         "Household applicances non-electric", "Jewelry", 
                         "Textile", "Tools non-electric",
                         "Toys non-electric")) %>% 
  mutate(repair_year = year(repair_date)) %>% 
  ggplot() + 
  geom_jitter(aes(y = repairability, 
                  x = factor(repair_year)),
              alpha = 0.3) + 
  facet_wrap(~ category)



# Pretty plot -------------------------------------------------------------

repairs %>% 
  filter(category %in% c("Bicyles", "Furniture", 
                         "Household applicances non-electric", "Jewelry", 
                         "Textile", "Tools non-electric",
                         "Toys non-electric")) %>% 
  filter(repairability >=1) %>% 
  mutate(repair_year = year(repair_date)) %>% 
  ggplot() + 
  geom_jitter(aes(y = repairability, 
                  x = factor(repair_year)),
              alpha = 0.3,
              colour = "white") + 
  labs(caption  = "Data: Repair Monitor; Repair Cafes Worldwide",
       title = "Repair of non-electric items over time",
       x = "", 
       y = "Rating of repair ease, from 1 (difficult) to 10 (easy)") +
  theme_bw() + 
  theme(
    axis.text.x = element_text(angle = 270, 
                               vjust = 0) ,
    panel.grid = element_blank(),
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0,
                                size = 12),
    plot.background = element_rect(fill = "grey30"),
    panel.background = element_rect(fill = "grey20"),
    text = element_text(colour = "white"),
    axis.text = element_text(colour = "white")
  ) + 
  facet_wrap(~ category)


## animate

p <- repairs %>% 
  filter(category %in% c("Bicyles", "Furniture", 
                         "Household applicances non-electric", "Jewelry", 
                         "Textile", "Tools non-electric",
                         "Toys non-electric")) %>% 
  filter(repairability >=1) %>% 
  mutate(repair_year = year(repair_date)) %>% 
  ggplot() + 
  geom_jitter(aes(y = repairability, 
                  x = factor(repair_year)),
              alpha = 0.3,
              colour = "white") + 
  labs(caption  = "Data: Repair Monitor; Repair Cafes Worldwide",
       title = "Repair of non-electric items over time",
       x = "", 
       y = "Rating of repair ease, from 1 (difficult) to 10 (easy)") +
  theme_bw() + 
  theme(
    axis.text.x = element_text(angle = 270, 
                               vjust = 0) ,
    panel.grid = element_blank(),
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0,
                                size = 12),
    plot.background = element_rect(fill = "grey30"),
    panel.background = element_rect(fill = "grey20"),
    text = element_text(colour = "white"),
    axis.text = element_text(colour = "white")
  ) + 
  facet_wrap(~ category) +
  gganimate::transition_time(repair_year) + 
  ease_aes('linear') + 
  shadow_mark(past = T)


p_animate <- animate(p,
        width = 1024,
        height = 768,
        nframes = 600,
        fps = 60,
        duration = 10,
        renderer = gifski_renderer(file = "20260407.gif"))  


animate(
  p,
  width = 1024,
  height = 768,
  nframes = 600,
  fps = 60,
  duration = 10,
  renderer = gifski_renderer(file = "avg_life_exp_cont_year.gif")
)



anim_save(here::here("2026", "2026-04-07", "20260407.gif"), p_animate)





