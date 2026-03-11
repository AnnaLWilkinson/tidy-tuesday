
# About this script -------------------------------------------------------

#  Project:  Tidy Tuesday
#  Purpose:  How likely is 'likely'?
#  Author:   Anna Wilkinson; anna.wilkinson@burnet.edu.au
#  Date started: 10 March 2026

# Notes on the data:

# Probability Quiz by A Kucharski - selecting phrases that represent the **higher** probability of something occurring

# Libraries ---------------------------------------------------------------

pacman::p_load(rio,
               here,
               tidyverse,
               janitor,
               patchwork)


# Import data -------------------------------------------------------------


# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2026-03-10')
## OR
#tuesdata <- tidytuesdayR::tt_load(2026, week = 10)

absolute_judgements <- tuesdata$absolute_judgements
pairwise_comparisons <- tuesdata$pairwise_comparisons
respondent_metadata <- tuesdata$respondent_metadata


# Summary -----------------------------------------------------------------

tabyl(absolute_judgements, term)


# Exploratory data analysis -----------------------------------------------

absolute_judgements %>% 
  ggplot() + 
  geom_density(aes(x = probability)) + 
  facet_wrap(~ term)


absolute_judgements %>% 
  ggplot() + 
  geom_histogram(aes(x = probability)) + 
  facet_wrap(~ term)


absolute_judgements %>% 
  ggplot() + 
  geom_histogram(aes(x = probability)) + 
  facet_wrap(~ term,
             scales = "free_y")


## spread of responses
my_summary <- absolute_judgements %>% 
  summarise(mean = mean(probability, na.rm = TRUE), 
            sd   = sd(probability, na.rm = TRUE), .by = term)

my_summary %>% 
  ggplot(aes(x = reorder(term, sd),
                 y = sd)) + 
  geom_point() + 
  geom_segment(aes(y = 0,
                   yend = sd, 
                   x = term, 
                   xend = term)) + 
  coord_flip()


# Pretty plot -------------------------------------------------------------


## Plot A
plot_a <-  my_summary %>% 
  ggplot(aes(x = reorder(term, sd),
             y = sd)) + 
  geom_point() + 
  geom_segment(aes(y = 0,
                   yend = sd, 
                   x = term, 
                   xend = term)) + 
  scale_y_continuous(limits = c(0,50)) + 
  labs(subtitle = "Phrases with more to less variation \nin the numeric estimate respondents gave",
       x = "", 
       y = "Standard deviation",
       caption = "Respondents gave each phrase a numerical estimate (0 to 100). 
       The standard deviation measure the spread of responses around the average; a larger standard deviation
       means more variation in the responses were given for the same phrase") + 
  coord_flip() + 
  theme_bw()



## Plot B
plot_b <- absolute_judgements %>% 
  filter(term == "Realistic Possibility") %>% 
  ggplot() + 
  geom_histogram(aes(x = probability),
                 colour = "navy",
                 fill = "lightblue") + 
  labs(x = "Numerical estimate respondents gave", 
       y = "Frequency (n)", 
       subtitle = "Respondents interpret the phrase \nRealistic Possibility inconsistently") + 
  theme_bw()

## Plot C
plot_c <- absolute_judgements %>% 
  filter(term == "Could Happen") %>% 
  ggplot() + 
  geom_histogram(aes(x = probability),
                 colour = "darkgreen",
                 fill = "green") + 
  labs(x = "Numerical estimate respondents gave", 
       y = "Frequency (n)", 
       subtitle = "Respondents interpret the phrase \nCould Happen inconsistently") + 
  theme_bw()


patch <- plot_b / plot_c
plot_a + patch



# Save plot ---------------------------------------------------------------

ggsave(filename = here::here("2026", "2026-03-10", "20260310.png"),
       plot = last_plot())

## END



