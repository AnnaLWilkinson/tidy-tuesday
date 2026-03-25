
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


# Pretty plot -------------------------------------------------------------


pi_digits %>% 
  filter(digit_seq == 1) %>% 
  ggplot() + 
  geom_point(aes(y = digit_position, 
                 x = reorder(factor(digit), -digit_position)),
             size = 8,
             shape = "\u03C0",
             colour = "navy") + 
  geom_segment(aes(xend = reorder(factor(digit), -digit_position),
                   x    = reorder(factor(digit), -digit_position), 
                   yend = digit_position - 0.2, 
                   y    = 0 ),
               linetype = 2, 
               colour = "navy",
               size = 0.5) + 
  scale_y_continuous(limits = c(0,35),
                     expand = expansion(add = 0.5),
                     breaks = c(0, 5, 10, 15, 20, 25, 30, 35)) + 
  theme_bw() + 
  theme(
    
    panel.grid.major.y  = element_blank(),
    panel.grid.minor.y = element_blank(),
    text = element_text(size = 16)
    
  ) + 

  labs(title = "When does a number first appear in Pi",
       x = "",
       y = "Position when digit first appears") + 
  coord_flip()



# Save plot ---------------------------------------------------------------

ggsave(filename = here::here("2026", "2026-03-24.png"),
       plot = last_plot())


## END







