
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

#ISO 3166-1 alpha-3 taxonomy

member_participation_stats_by_country <- member_participation_stats_by_country %>% 
  mutate(country_name = countrycode::countrycode(iso3_code, 
                        origin = "iso3c",
                        destination = "country.name"))

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
  facet_wrap(~ country_name, 
             scales = "free_y")


member_participation_stats_by_country %>% 
  filter(region_id == "EAS") %>% 
  ggplot() + 
  geom_point(aes(x = current_up_to, 
                 y = total_members,
                 colour= iso3_code), 
             stat = "identity") + 
  facet_wrap(~ country_name, 
             scales = "free_y")


# stratify by total members
eas_tot_members_quant <- member_participation_stats_by_country %>% 
 filter(region_id == "EAS") %>% 
  group_by(iso3_code) %>% 
  filter(current_up_to == max(current_up_to)) %>% 
  distinct(current_up_to, total_members) %>% 
  ungroup() %>% 
  arrange(-total_members) %>% 
  pull(total_members) %>% 
  quantile()


eas_tot_members_quant_grps <- member_participation_stats_by_country %>% 
  filter(region_id == "EAS") %>% 
  group_by(iso3_code) %>% 
  filter(current_up_to == max(current_up_to)) %>% 
  distinct(current_up_to, total_members) %>% 
  mutate(
      total_members_quant = case_when(
      total_members >=eas_tot_members_quant[1] & total_members <eas_tot_members_quant[2] ~   1 ,
      total_members >=eas_tot_members_quant[2] & total_members <eas_tot_members_quant[3] ~   2 ,
      total_members >=eas_tot_members_quant[3] & total_members <eas_tot_members_quant[4] ~   3 ,
      total_members >=eas_tot_members_quant[4] & !is.na(total_members) ~   4 ,
    
  ))

my_list <- list()
for (num in 1:4) {
  
    my_list[[num]]  <- eas_tot_members_quant_grps %>% 
      filter(total_members_quant == num) %>% 
      pull(iso3_code)
}

my_list[1]

# filter to first quantile total members in EAS only
member_participation_stats_by_country %>% 
  filter(iso3_code %in% my_list[[1]]) %>% 
  ggplot() + 
  geom_point(aes(x = current_up_to, 
                 y = total_members,
                 colour= iso3_code), 
             stat = "identity") + 
  facet_wrap(~ country_name, 
             scales = "free_y")
