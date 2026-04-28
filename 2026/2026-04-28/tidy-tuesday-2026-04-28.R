
# About this script -------------------------------------------------------

#  Purpose: Tidy Tuesday
#  Project: US Agricultural Tariffs
#  Author: Anna Wilkinson; anna.wilkinson@burnet.edu.au
#  Date started: 28 April 2026
#  Last dtae changed: 


# Load libraries ----------------------------------------------------------
library(rio)
library(here)
library(janitor)
library(lubridate)
library(tidyverse)



# Load data ---------------------------------------------------------------

# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2026-04-28')
## OR
#tuesdata <- tidytuesdayR::tt_load(2026, week = 17)

agreements <- tuesdata$agreements
quantity_codes <- tuesdata$quantity_codes
tariff_agricultural <- tuesdata$tariff_agricultural
tariff_codes <- tuesdata$tariff_codes

# Option 2: Read directly from GitHub

# agreements <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-28/agreements.csv')
# quantity_codes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-28/quantity_codes.csv')
# tariff_agricultural <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-28/tariff_agricultural.csv')
# tariff_codes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-28/tariff_codes.csv')
# 


# Summary -----------------------------------------------------------------

glimpse(agreements)
glimpse(quantity_codes)
glimpse(tariff_agricultural)
glimpse(tariff_codes)


# Join --------------------------------------------------------------------

tariff_agricultural <-  left_join(tariff_agricultural, agreements, by = "agreement")


# Exploratory data analysis -----------------------------------------------

tabyl(tariff_agricultural, agreement_full)

range(tariff_agricultural$begin_effective_date)
range(tariff_agricultural$end_effective_date)

tariff_agricultural %>% 
  filter(begin_effective_date >'2023-01-01') %>% 
  summarise(n = n(), .by = agreement_full)

tariff_agricultural %>% 
  filter(begin_effective_date >='2022-01-01') %>% 
  summarise(n = n(), .by = c(agreement_full, begin_effective_date)) %>% 
  ggplot() + 
  geom_point(aes(x = begin_effective_date, 
                 y = n))

tariff_agricultural %>% 
  filter(begin_effective_date >'2020-01-01') %>% 
  summarise(n = n(), .by = agreement_full)


tariff_agricultural %>% 
  drop_na(c(begin_effective_date, agreement_full)) %>% 
  filter(begin_effective_date >'1994-01-01') %>% 
  summarise(n = n(), .by = c(begin_effective_date,agreement_full)) %>% 
  ggplot() +
  geom_point(aes(x = begin_effective_date,
                 y = agreement_full,
                 size = n))


##

