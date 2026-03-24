
# About this script -------------------------------------------------------

#  Project:  Tidy Tuesday
#  Purpose:  One Million Digits of Pi
#  Author:   Anna Wilkinson; anna.wilkinson@burnet.edu.au
#  Date started: 24 March 2026



# Libraries ---------------------------------------------------------------

pacman::p_load(rio, 
               here, 
               lubridate, 
               janitor,
               tidyverse)


# Import data -------------------------------------------------------------

# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2026-03-24')
## OR
#tuesdata <- tidytuesdayR::tt_load(2026, week = 12)

pi_digits <- tuesdata$pi_digits

# Option 2: Read directly from GitHub

#pi_digits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-03-24/pi_digits.csv')



# Summary ------------------------------------------------------------------

summary(pi_digits)



# Processing --------------------------------------------------------------

pi_digits <-  pi_digits %>% 
  arrange(digit_position) %>% 
  group_by(digit) %>% 
  mutate(digit_seq = seq_along(digit_position)) %>% 
  ungroup()



# Exploratory data analysis -----------------------------------------------

pi_digits %>% 
  summarise(n = n(), .by = digit)


pi_digits %>% 
  ggplot() + 
  geom_histogram(aes(x = digit))

pi_digits %>% 
  filter(digit_position <=100) %>% 
  
  ggplot() +
  geom_bar(aes(x = digit_position, 
               y = digit_seq, 
               fill = factor(digit)),
           stat = "identity")

pi_digits %>% 
  filter(digit_seq == 1) %>% 
  ggplot() + 
  geom_point(aes(y = digit_position, 
                 x = reorder(factor(digit), digit_position))) + 
  coord_flip()


