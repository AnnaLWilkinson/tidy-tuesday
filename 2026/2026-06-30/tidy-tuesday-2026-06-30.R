
# About this script -------------------------------------------------------

#  Purpose: Tidy Tuesday
#  Project: Wreck Inventory of Ireland
#  Author: Anna Wilkinson; anna.wilkinson@burnet.edu.au
#  Date started: 30 June 2026
#  Date last changed:


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggtext)


# Load data ---------------------------------------------------------------
# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2026-06-30')
## OR
#tuesdata <- tidytuesdayR::tt_load(2026, week = 26)

wreck_inventory <- tuesdata$wreck_inventory

# Option 2: Read directly from GitHub

#wreck_inventory <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-30/wreck_inventory.csv')


# Summary of the data -----------------------------------------------------
summary(wreck_inventory)
head(wreck_inventory)

table(wreck_inventory$year)

# Exploratory data analysis -----------------------------------------------

wreck_inventory %>% 
  ggplot() + 
  geom_bar(aes(x = year),
           stat = "count")

wreck_inventory %>% 
  filter(!is.na(year)) %>% 
  count(year) %>% 
  arrange(-n)

wreck_inventory %>% 
  count(classification) %>% 
  filter(classification !="Unknown") %>% 
  mutate(prop = n/sum(n)) %>% 
  arrange(-prop)

wreck_inventory %>% 
  ggplot() + 
  geom_bar(aes(x = year),
           stat = "count") + 
  facet_wrap(~ classification,
             scales = "free_y")


wreck_inventory %>% 
  filter(classification !="Unknown" & !is.na(year)) %>% 
  summarise(n = n(), .by = c(year, classification)) %>% 
  arrange(-n) %>% 
  head(n = 100) %>% 
            
  ggplot() + 
  geom_bar(aes(x = year, 
               y = n),
           stat = "identity") + 
  facet_wrap(~ classification,
             scales = "free_y")
  

wreck_inventory %>% 
  filter(year == 1917) %>% 
  ggplot() + 
  geom_bar(aes(x = date),
           stat = "count") + 
  facet_wrap(~ classification)


wreck_inventory %>% 
  filter(year == 1917) %>% 
  ggplot() + 
  geom_bar(aes(x = date),
           stat = "count") 

wreck_inventory %>% 
  filter(year == 1917) %>% 
  summarise(n = n(), .by = date) %>% 
  ggplot() + 
  geom_point(aes(x = date, 
                 y = n)) 

wreck_inventory %>% 
  filter(year == 1917) %>% 
  summarise(n = n(), .by = date) %>% 
  ggplot() + 
  geom_bar(aes(x =date, 
                 y = n),
           stat = "identity") 

wreck_inventory %>% 
  filter(year == 1917) %>% 
  mutate(month = lubridate::month(date)) %>% 
  summarise(n = n(), .by = month) %>% 
  ggplot() + 
  geom_point(aes(x = month, 
                 y = n)) 



wreck_inventory %>% 
  filter(year %in% 1914:1919 & classification == "Steamship") %>% 
  ggplot() + 
  geom_bar(aes(x = date),
           stat = "count") + 
  facet_wrap(~ year, 
             scales = "free_x")

wreck_inventory %>% 
  filter(year == 1916 & classification == "Steamship") %>% 
  ggplot() + 
  geom_bar(aes(x = date),
           stat = "count") + 
  scale_y_continuous(limits = c(0,6))


wreck_inventory %>% 
  filter(year == 1917 & classification == "Steamship") %>% 
  ggplot() + 
  geom_bar(aes(x = date),
           stat = "count")


wreck_inventory %>% 
  filter(year == 1918 & classification == "Steamship") %>% 
  ggplot() + 
  geom_bar(aes(x = date),
           stat = "count")

wreck_inventory %>% 
  filter(year == 1919 & classification == "Steamship") %>% 
  ggplot() + 
  geom_bar(aes(x = date),
           stat = "count")



# Pretty plot -------------------------------------------------------------

total_wrecks16_17 <- wreck_inventory %>% 
  filter(year == 1917 | year == 1916) %>% 
  summarise(n = n(), .by = year)



wreck_inventory %>% 
  filter(year == 1917 | year == 1916) %>% 
  summarise(n = n(), .by = c(year,date)) %>% 
  ggplot() + 

  geom_segment(aes(x = date, 
                   xend = date, 
                   y = 0,
                   yend = n),
               colour = "grey80") +   
  
  geom_point(aes(x = date, 
               y = n), 
           stat = "identity", 
           shape = 21,
           colour = "black",
           fill = "navy",
           size = 2) +
  
  labs(x = "Date of the shipwreck event", 
       y = "Shipwreck events (n)",
       title = "1917 was the worst year for recorded shipwrecks in Ireland waterways") + 
  
  scale_y_continuous(limits = c(0,12),
                     breaks = seq(0, 10, 1),
                     expand = c(0,0)) + 

  facet_grid(~year, 
             scales = "free_x") +
  
    scale_x_date(date_breaks = "1 month", 
               date_labels = "%b") +
  
  theme_classic() + 
  theme(
    plot.margin = margin(t=10 , b=40 , r=10 , l=10 , unit = "pt"), 
    plot.title.position = "plot",
    plot.title = ggtext::element_textbox_simple(
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5),
      face = "bold",
      size = rel(1.5)
    ),
    
    panel.spacing = unit(2, "lines"),
    panel.grid.major.y = element_line(colour = "lightgrey"), 
   
    axis.title.x = element_text(vjust = -3)
  )
