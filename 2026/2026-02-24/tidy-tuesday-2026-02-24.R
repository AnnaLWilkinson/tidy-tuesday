
#  About this script ------------------------------------------------------

#  Project: Tidy Tuesday
#  Purpose: Science Foundation Ireland grants Commitments
#  Author:  Anna Wilkinson; anna.wilkinson@burnet.edu.au
#  Date started: 24 Feb 2026
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
library(scales)
conflicted::conflicts_prefer(dplyr::select)
conflicted::conflicts_prefer(dplyr::filter)

# Import data  ------------------------------------------------------------

# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2026-02-24')
## OR
#tuesdata <- tidytuesdayR::tt_load(2026, week = 8)

sfi_grants <- tuesdata$sfi_grants

# Option 2: Read directly from GitHub

#sfi_grants <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-02-24/sfi_grants.csv')



# Summary -----------------------------------------------------------------

summary(sfi_grants)


# Process -----------------------------------------------------------------

sfi_grants <-  sfi_grants %>% 
  mutate(start_year = year(start_date), 
         end_year = year(end_date),
         duration = floor(difftime(end_date, start_date, units = "weeks")))

sfi_grants$end_date - sfi_grants$start_date

# Exploratory Data Analysis -----------------------------------------------

sfi_grants %>% 
  tabyl(programme_name)


sfi_grants %>% 
  group_by(programme_name) %>% 
  count() %>% 
  arrange(-n)


sfi_grants %>% 
  tabyl(start_year)


sfi_grants %>% 
  summarise(n = n(), .by = start_year) %>% 
  arrange(-n)


sfi_grants %>% 
  ggplot() + 
  geom_histogram(aes(x = duration))

sfi_grants %>% 
  ggplot() + 
  geom_density(aes(x = duration))

sfi_grants %>% 
  ggplot() + 
  geom_histogram(aes(x = duration)) + 
  facet_wrap(~ research_body)


sfi_grants %>% 
  ggplot() + 
  geom_point(aes(x = duration,
                 y = current_total_commitment))



sfi_grants %>% 
  ggplot(aes(x = (duration/52),
             y = current_total_commitment)) + 
  geom_point(size = 2, 
             alpha = 0.3) + 
  scale_y_continuous(labels = scales::label_number(scale_cut = cut_short_scale()),
                     expand = expansion(mult = c(0.05, 0.05))) + 
  scale_x_continuous(breaks = seq(0, 10, 2),
                     expand = expansion(mult = c(0.05, 0.05))) + 
  theme_bw() +
  labs( y = "Current total commitment", 
        x = "Duration of grants in years",
        title = "Do larger science grants mean longer duration grants in Ireland?")



# Pretty plot -------------------------------------------------------------


sfi_grants %>% 
  ggplot(aes(x = (duration/52),
             y = current_total_commitment)) + 
  geom_point(size = 3, 
             shape = 21,
             colour = "navy",
             fill = "lightblue",
             alpha = 0.8,
             stroke = 1) + 
  scale_y_continuous(labels = scales::label_number(scale_cut = cut_short_scale()),
                     expand = expansion(mult = c(0.05, 0.05))) + 
  scale_x_continuous(breaks = seq(0, 10, 2),
                     expand = expansion(mult = c(0.05, 0.05))) + 
  theme_bw() +
  labs( y = "Current total commitment", 
        x = "Duration of grants in years",
        title = "Do larger science grants mean longer duration grants in Ireland?")



# Save plot ---------------------------------------------------------------

ggsave(filename = here::here("2026", "2026-02-24", "20260224.png"), 
       plot = last_plot(),
       dpi = 300)


## END


