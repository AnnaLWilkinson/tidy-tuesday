
# About this script -------------------------------------------------------

#  Project: Tidy Tuesday
#  Purpose: State of Crossref metatdata by member country
#  Author: Anna Wilkinson; anna.wilkinson@burnet.edu.au
#  Date started: 19 May 2026



# Load libraries ----------------------------------------------------------

library(rio)
library(here)
library(janitor)
library(lubridate)
library(tidyverse)
library(countrycode)


# Load data ---------------------------------------------------------------


# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2026-05-19')
## OR
#tuesdata <- tidytuesdayR::tt_load(2026, week = 20)

member_participation_stats_by_country <- tuesdata$member_participation_stats_by_country
metadata_coverage_stats_by_country <- tuesdata$metadata_coverage_stats_by_country

# # Option 2: Read directly from GitHub
# 
# member_participation_stats_by_country <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-05-19/member_participation_stats_by_country.csv')
# metadata_coverage_stats_by_country <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-05-19/metadata_coverage_stats_by_country.csv')



# Summary -----------------------------------------------------------------

range(member_participation_stats_by_country$current_up_to)
tabyl(member_participation_stats_by_country$region_id)
tabyl(member_participation_stats_by_country$iso3_code)




# Processing --------------------------------------------------------------

# World Bank Taxonomy for regions

# Geographic regions
# Visit the page on World Bank Units for more details on regional units.
# 
# Abbreviation	Description
# EAS	East Asia & Pacific
# ECS	Europe & Central Asia
# LCN	Latin America & the Caribbean
# MEA	Middle East, North Africa, Afghanistan & Pakistan
# NAC	North America
# SAS	South Asia
# SSF	Sub-Saharan Africa

member_participation_stats_by_country <-    member_participation_stats_by_country %>% 
  mutate(region_descr = case_when(
    region_id == "EAS" ~ "East Asia & Pacific", 
    region_id == "ECS" ~ "Europe & Central Asia", 
    region_id == "LCN" ~ "Latin America & the Caribbean",
    region_id == "MEA" ~ "Middle East, North Africa, Afghanistan & Pakistan",
    region_id == "NAC" ~ "North America",
    region_id == "SAS" ~ "South Asia",
    region_id == "SSF" ~ "Sub-Saharan Africa",
    TRUE ~ 'CHECK'
  ))
tabyl(member_participation_stats_by_country, region_id, region_descr)


# Exploratory analysis ----------------------------------------------------


member_participation_stats_by_country %>% 
  ggplot() + 
  geom_bar(aes(x = current_up_to, 
               y = total_members, 
               group = region_descr),
           stat = "identity") + 
  facet_wrap(~ region_descr)


member_participation_stats_by_country %>% 
  ggplot() + 
  geom_bar(aes(x = current_up_to, 
               y = deposits_ref, 
               group = region_descr),
           stat = "identity") + 
  facet_wrap(~ region_descr)

member_participation_stats_by_country %>% 
  ggplot() + 
  geom_bar(aes(x = current_up_to, 
               y = deposits_orcid, 
               group = region_descr),
           stat = "identity") + 
  facet_wrap(~ region_descr)


member_participation_stats_by_country %>% 
  filter(region_id == "EAS") %>% 
  ggplot() + 
  geom_point(aes(x = current_up_to, 
                 y = total_members,
                 colour= iso3_code), 
             stat = "identity")
  

member_participation_stats_by_country %>% 
  filter(region_id == "EAS") %>% 
  ggplot() + 
  geom_point(aes(x = current_up_to, 
                 y = total_members,
                 colour= iso3_code), 
             stat = "identity") + 
  facet_wrap(~ iso3_code, 
             scales = "free_y")






