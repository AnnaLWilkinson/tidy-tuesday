
# About this script -------------------------------------------------------

#  Purpose: Tidy Tuesday
#  Project: US Agricultural Tariffs
#  Author: Anna Wilkinson; anna.wilkinson@burnet.edu.au
#  Date started: 28 April 2026
#  Last date changed: 


# Load libraries ----------------------------------------------------------
library(rio)
library(here)
library(janitor)
library(lubridate)
library(tidyverse)
library(amerika)



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



# Create administration dataframe -----------------------------------------
us_administrations <- tibble(
  year = 1989:2024
) %>%
  mutate(
    administration = case_when(
      year >= 1989 & year <= 1992 ~ "George H. W. Bush",
      year >= 1993 & year <= 2000 ~ "Clinton",
      year >= 2001 & year <= 2008 ~ "George W. Bush",
      year >= 2009 & year <= 2016 ~ "Obama",
      year >= 2017 & year <= 2020 ~ "Trump",
      year >= 2021 & year <= 2024 ~ "Biden"
    ),
    party = case_when(
      administration %in% c("Clinton", "Obama", "Biden") ~ "Democratic",
      administration %in% c("George H. W. Bush", "George W. Bush", "Trump") ~ "Republican"
    )
  )

us_administrations


# Join administration to tariffs ------------------------------------------
tariff_agricultural <- tariff_agricultural %>% 
  mutate(year = year(begin_effective_date))
tabyl(tariff_agricultural, year)


tariff_agricultural <- left_join(tariff_agricultural, us_administrations, by = "year")

tariff_agricultural %>% 
  drop_na(c(begin_effective_date, agreement_full)) %>% 
  filter(begin_effective_date >'1994-01-01') %>% 
  summarise(n = n(), .by = c(party, begin_effective_date,agreement_full)) %>% 
  ggplot() +
  geom_point(aes(x = begin_effective_date,
                 y = agreement_full,
                 size = n,
                 colour = party)) + 
  scale_colour_manual(values = c("Republican" = "#E81B23",
                                 "Democratic" = "#00AEF3"))



# Pretty plot -------------------------------------------------------------

# organise agreements
agreement_order <- tariff_agricultural %>% 
  summarise(first_date = min(begin_effective_date), .by = agreement_full) %>%  
  arrange(first_date) %>% 
  distinct(agreement_full) %>%  
  pull(agreement_full) 


tariff_agricultural %>% 
  drop_na(c(begin_effective_date, agreement_full)) %>% 
  filter(begin_effective_date >'1994-01-01') %>% 
  mutate(agreement_full = fct_relevel(agreement_full, agreement_order)) %>% 
  summarise(n = n(), .by = c(party, begin_effective_date,agreement_full)) %>% 
  ggplot() +
  geom_point(aes(x = begin_effective_date,
                 y = agreement_full,
                 size = n,
                 colour = party),
             alpha = 0.5) + 
  scale_x_date(breaks = "3 years",
               date_labels = "%Y") + 
  scale_colour_manual(values = c("Republican" = "#E81B23",
                                 "Democratic" = "#00AEF3")) + 
  labs(y = "", 
       x  = "", 
       tag = str_wrap("Is there a flurry of tariff activity 
                      when administrations change?", 40),
       caption = 'Source: USITC Tariff Database') + 
  theme(
    
    axis.text = element_text(colour = "white",
                             face = "bold"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "lightgrey"),
    plot.margin = margin(t = 50,r = 10,b = 10,l = 10),
    plot.tag.position = c(0.55, 0.95),
    plot.tag = element_text(colour = "grey30", size = 18),
    legend.position = "none"
  )



# Save plot ---------------------------------------------------------------

ggsave(filename = "2026/2026-04-28/20260428.png",
       plot = last_plot())


## END









