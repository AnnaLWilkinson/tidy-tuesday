
# About this script -------------------------------------------------------

# Purpose: IELTS exam results
# Project: Tidy Tuesday
# Author: Anna Wilkinson; anna.wilkinson@burnet.edu.au
# Date started: 18 Aug 2026


# Load libraries ----------------------------------------------------------
library(tidyverse)


# Load data ---------------------------------------------------------------

# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2026-08-18')
## OR
#tuesdata <- tidytuesdayR::tt_load(2026, week = 33)

demo_by_first_language <- tuesdata$demo_by_first_language
demo_by_nationality <- tuesdata$demo_by_nationality
demo_by_reasons <- tuesdata$demo_by_reasons
performance_by_first_language <- tuesdata$performance_by_first_language
performance_by_nationality <- tuesdata$performance_by_nationality

# # Option 2: Read directly from GitHub
# 
# demo_by_first_language <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-08-18/demo_by_first_language.csv')
# demo_by_nationality <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-08-18/demo_by_nationality.csv')
# demo_by_reasons <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-08-18/demo_by_reasons.csv')
# performance_by_first_language <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-08-18/performance_by_first_language.csv')
# performance_by_nationality <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-08-18/performance_by_nationality.csv')



# Exploratory data analysis -----------------------------------------------

performance_by_first_language %>% 
  count(type)

performance_by_first_language %>% 
  count(language) %>% 
  arrange(-n)

performance_by_first_language %>% 
  count(part)

performance_by_first_language %>%
  group_by(part) %>%
  summarise(mean(score))

performance_by_first_language %>% 
  ggplot() + 
  geom_jitter(aes(x = part, 
                  y = score))


performance_by_first_language %>% 
  filter(part != "overall") %>% 
  ggplot() + 
  geom_jitter(aes(x = part, 
                  y = score))



demo_by_first_language %>% 
  ggplot() + 
  geom_histogram(aes(x = percent))


demo_by_first_language %>% 
  ggplot() + 
  geom_histogram(aes(x = percent)) + 
  facet_wrap(~language)

demo_by_first_language %>% 
  count(band)

demo_by_first_language %>% 
  summarise(n = n(), .by = c(language, band))


demo_by_first_language %>% 
  ggplot() + 
  geom_bar(aes(x = band),
           stat = "count")

demo_by_first_language %>% 
  ggplot() + 
  geom_jitter(aes(x = band, 
                  y = percent)) + 
  geom_jitter(data = subset(demo_by_first_language, language == "English"), 
              aes(x = band, 
                  y = percent), 
              colour = 'red')



demo_by_first_language %>% 
  ggplot(aes(x = band, 
             y = percent)) + 
  geom_point() + 
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3), se = FALSE)





