
# About this script -------------------------------------------------------

#  Project: Tidy Tuesday
#  Purpose: Roundabouts across the world
#  Author:  
#  Date started: 
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

library(conflicted)
conflicted::conflicts_prefer(dplyr::select)
conflicted::conflicts_prefer(dplyr::filter)

# Import data  ------------------------------------------------------------


# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2025-12-16')
## OR
#tuesdata <- tidytuesdayR::tt_load(2025, week = 50)

roundabouts_clean <- tuesdata$roundabouts_clean

# Option 2: Read directly from GitHub

#roundabouts_clean <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-12-16/roundabouts_clean.csv')



# Summary  ----------------------------------------------------------------

summary(roundabouts_clean)


# Exploratory data analysis -----------------------------------------------

tabyl(roundabouts_clean, country )
tabyl(roundabouts_clean, type)
tabyl(roundabouts_clean, year_completed)
tabyl(roundabouts_clean, approaches)
tabyl(roundabouts_clean, lane_type)

range(roundabouts_clean$year_completed)

roundabouts_clean |> 
  filter(year_completed >0) |> 
  ggplot(aes(x  = factor(year_completed))) + 
  geom_bar(stat = "count")

roundabouts_clean |> 
  ggplot(aes(x  = factor(lane_type))) + 
  geom_bar(stat = "count") + 
  coord_flip()


roundabouts_clean |> 
  ggplot(aes(x  = factor(approaches))) + 
  geom_bar(stat = "count") + 
  coord_flip()

roundabouts_clean |> 
  ggplot(aes(x  = country)) + 
  geom_bar(stat = "count") + 
  coord_flip()


roundabouts_clean |> 
  filter(country == "Australia") |> 
  tabyl(county_area)


roundabouts_clean |> 
  filter(country == "Australia") |> 
  tabyl(state_region)

roundabouts_clean |> 
  filter(country == "Australia") |> 
  filter(year_completed >0) |> 
  tabyl(year_completed)
  
roundabouts_clean |> 
  filter(country == "Australia") |> 
  tabyl(state_region, approaches)


roundabouts_clean |> 
  filter(country == "Australia") |> 
  tabyl(state_region, functional_class)


roundabouts_clean |> 
  filter(country == "Australia") |> 
  tabyl(state_region, control_type)


roundabouts_clean |> 
  filter(country == "Australia") |> 
  drop_na(state_region) |> 
  
  ggplot(aes(x = approaches)) + 
  geom_bar(stat = "count") +
  facet_wrap(~ state_region )


roundabouts_clean |> 
  tabyl(country, control_type)



roundabouts_clean |> 
  filter(year_completed >0) |> 
  ggplot(aes(x  = factor(year_completed))) + 
  geom_bar(stat = "count")


# Pretty plot -------------------------------------------------------------
range(roundabouts_clean$year_completed)

roundabouts_clean |> 
  filter(year_completed >=1900) |> 
  summarise(n = n(), .by = year_completed) |> 
  ggplot(aes(x  = year_completed,
             y = n)) + 
  geom_col(width = 0.8) +
  scale_x_continuous(limits = c(1900,2023),
                     breaks = seq(1903,2023,10),
                     expand = c(0.01,0.01)) + 
  scale_y_continuous(limits = c(0,700),
                     expand = c(0,0)) + 
  labs(y = "Roundabouts (frequency (n))",
       x = "",
       title = "Roundabouts across the world") + 
  theme_minimal() + 
  theme(
    
    panel.background = element_rect(fill = "grey90", colour = NA),
    plot.background = element_rect(fill = "lightblue3", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(colour = "linen"),
    axis.text.y = element_text(hjust = 1.2),
    axis.title.y = element_text(vjust = 3),
    axis.title = element_text(colour = "linen"),
    plot.title = element_text(colour = "linen", size = 16)
  )


p1 <- roundabouts_clean |> 
  filter(year_completed >=1993) |> 
  summarise(n = n(), .by = year_completed) |> 
  ggplot(aes(x  = year_completed,
             y = n)) + 
  geom_col(width = 0.8,
           alpha = 0.8) +
  annotate("text",
           x = 2000,
           y = 600,
           label = "Absolute count of roundabouts",
           colour = "grey20",
           size = 4) + 
  scale_x_continuous(limits = c(1990,2025),
                     breaks = seq(1993,2023,5),
                     expand = c(0.01,0.01)) + 
  scale_y_continuous(limits = c(0,700),
                     expand = c(0,0)) + 
  labs(y = "Roundabouts (frequency (n))",
       x = "") + 
  theme_minimal() + 
  theme(
    
    panel.background = element_rect(fill = "grey90", colour = NA),
    plot.background = element_rect(fill = "lightblue3", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(colour = "linen"),
    axis.text.y = element_text(hjust = 1.2),
    axis.title.y = element_text(vjust = 3),
    axis.title = element_text(colour = "linen"),
    plot.title = element_text(colour = "linen", size = 16),
    plot.margin = margin(6,6,8,12)
  )


p2 <- roundabouts_clean |> 
  filter(year_completed >=1993) |> 
  summarise(n = n(), .by = year_completed) |> 
  arrange(year_completed) |> 
  mutate(diff = n - dplyr::lag(n),
         perct_change = diff/n,
         diff_from1993 = n - n[year_completed ==1993] ,
         perct_from1993 = diff_from1993/n[year_completed ==1993]) |> 
  
  ggplot(aes(x = year_completed, 
             y = perct_from1993)) + 
  geom_col() +
  annotate("text",
           x = 2000,
           y = 150,
           label = "Relative increase in roundabouts",
           colour = "grey20",
           size = 4) + 
  scale_x_continuous(limits = c(1990,2025),
                     breaks = seq(1993,2023,5),
                     expand = c(0.01,0.01)) + 
  scale_y_continuous(limits = c(0,180),
                     expand = c(0,0)) + 
  labs(y = "Increase from 1993 (%)",
       x = "") + 
  theme_minimal() + 
  theme(
    
    panel.background = element_rect(fill = "grey90", colour = NA),
    plot.background = element_rect(fill = "#c8ad7f", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(colour = "linen"),
    axis.text.y = element_text(hjust = 1.2),
    axis.title.y = element_text(vjust = 3),
    axis.title = element_text(colour = "linen"),
    plot.title = element_text(colour = "linen", size = 16),
    plot.margin = margin(6,6,8,12)
  )

p1 + p2 + 
  patchwork::plot_annotation(title = "Roundabouts across the world",
                             caption = "1993 used a reference year for relative calculation")


ggsave(filename = here::here("2025", "2025-12-16", "20251216.png"),
       plot = last_plot())


## END





