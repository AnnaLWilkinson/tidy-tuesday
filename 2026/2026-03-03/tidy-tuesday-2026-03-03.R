
# About this script -------------------------------------------------------

#  Project: Tidy Tuesday 
#  Purpose: Golem Grad Tortoise Data
#  Author: Anna Wilkinson; anna.wilkinson@burnet.edu.au
#  Date started: 4 March 2026


# Libraries ---------------------------------------------------------------
library(tidyverse)
library(rio)
library(here)
library(janitor, include.only = c("tabyl", "clean_names"))
library(lubridate)
library(naniar)
library(visdat)
library(patchwork)

# Import data -------------------------------------------------------------

# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2026-03-03')
## OR
#tuesdata <- tidytuesdayR::tt_load(2026, week = 9)

clutch_size_cleaned <- tuesdata$clutch_size_cleaned
tortoise_body_condition_cleaned <- tuesdata$tortoise_body_condition_cleaned


# Exploratory data analysis -----------------------------------------------

summary(clutch_size_cleaned)
summary(tortoise_body_condition_cleaned)

tortoise_body_condition_cleaned %>% 
  distinct(individual) %>% 
  nrow()  ## 2139 unique tortoise

tortoise_body_condition_cleaned %>% 
  distinct(individual, .keep_all = TRUE) %>% 
  janitor::tabyl(sex)  ## 646 f, 1493 m


tortoise_body_condition_cleaned %>% 
  distinct(individual, .keep_all = TRUE) %>% 
  janitor::tabyl(locality)  ## 129 beach, 838 Konjsko, 1172 plateau

# Process -----------------------------------------------------------------

# expand data to every combination (to see missed recapture)

# use complete to expand all combinations within individual of year and season
my_expand_df <-  tortoise_body_condition_cleaned %>% 
  complete(
    individual,
    nesting(year, season),
    fill = list(year_recode = NA_real_, locality = NA_character_, sex = NA_character_),
    explicit = FALSE) 

# fill in sex and locality w observed data
my_expand_df <- my_expand_df %>% 
  group_by(individual) %>% 
  fill(c(sex,locality), .direction = "downup")


my_expand_df <-  my_expand_df %>% 
  mutate(year_chr = as.character(year),
         year_season = paste0(year_chr, "-", season))

# Exploratory plotting ----------------------------------------------------

my_expand_df %>% 
  group_by(year_season) %>% 
  count()


my_expand_df %>% 
  group_by(year_season, locality) %>% 
  count() %>% 
  ggplot() + 
  geom_bar(aes(x = year_season, 
               y = n,
               fill = locality), 
           stat = "identity")



visdat::vis_dat(my_expand_df)
vis_miss(my_expand_df)


my_expand_df %>% 
  ggplot(aes(x = body_mass_grams, 
           y = straight_carapace_length_mm)) + 
  geom_miss_point()

my_expand_df %>% 
  ggplot(aes(x = body_mass_grams, 
             y = straight_carapace_length_mm)) + 
  geom_miss_point() + 
  facet_wrap(~ locality)


my_expand_df %>% 
  filter(is.na(year_recode)) %>% 
  group_by(year_season) %>% 
  count()

my_expand_df %>% 
  filter(is.na(year_recode)) %>% 
  group_by(year_season, sex) %>% 
  count() %>% 
  
  ggplot() + 
  geom_bar(aes(x = year_season,
               y = n), 
           stat = "identity") + 
  facet_wrap(~ sex)


my_expand_df %>% 
  filter(is.na(year_recode)) %>% 
  group_by(year_season, locality) %>% 
  count() %>% 
  
  ggplot() + 
  geom_bar(aes(x = year_season,
               y = n), 
           stat = "identity") + 
  facet_wrap(~ locality)



# Pretty plot -------------------------------------------------------------

my_expand_df %>% 
  filter(is.na(year_recode)) %>% 
  group_by(year_season, locality) %>% 
  count() %>% 
  
  ggplot() + 
  geom_bar(aes(x = year_season,
               y = n), 
           stat = "identity") + 
  facet_wrap(~ locality, 
             scales = "free_y")


## Beach # 129 Beach tortoise
p_beach <- my_expand_df %>% 
  filter(is.na(year_recode)) %>% 
  filter(locality == "Beach") %>% 
  group_by(year_season, locality) %>% 
  count() %>% 
  
  ggplot() + 
  geom_bar(aes(x = year_season,
               y = n), 
           stat = "identity",
           fill = "grey60") +
  geom_hline(yintercept = 129, 
             colour = "darkgreen",
             linetype = 2,
             size = 1.5) +
  annotate(geom = "text", 
           x = "2010-Spring",
           y = 145,
           label = "Total of 129 tortoises to recapture") + 
  scale_y_continuous(limits = c(0, 170)) + 
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 270, vjust = 0.5)  
  ) +
  labs(title = "Beach location",
       y = "Number not recaptured (n)", 
       x = "") 

# Konjsko N=838 tortoises
p_konjsko <-  my_expand_df %>% 
  filter(is.na(year_recode)) %>% 
  filter(locality == "Konjsko") %>% 
  group_by(year_season, locality) %>% 
  count() %>% 
  
  ggplot() + 
  geom_bar(aes(x = year_season,
               y = n), 
           stat = "identity",
           fill = "grey60") +
  geom_hline(yintercept = 838,
             colour = "darkgreen",
             linetype = 2,
             size = 1.5) +
  annotate(geom = "text", 
           x = "2010-Spring",
           y = 1000,
           label = "Total of 838 tortoises to recapture") + 
  scale_y_continuous(limits = c(0, 1200)) + 
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 270, vjust = 0.5)  
  ) +
  labs(title = "Konjsko location",
       y = "Number not recaptured (n)", 
       x = "") 


# Plateau N=1172 plateau
p_plateau <- my_expand_df %>% 
  filter(is.na(year_recode)) %>% 
  filter(locality == "Plateau") %>% 
  group_by(year_season, locality) %>% 
  count() %>% 
  
  ggplot() + 
  geom_bar(aes(x = year_season,
               y = n), 
           stat = "identity",
           fill = "grey60") +
  geom_hline(yintercept = 1172,
             colour = "darkgreen",
             linetype = 2,
             size = 1.5) +
  annotate(geom = "text", 
           x = "2010-Spring",
           y = 1400,
           label = "Total of 1,172 tortoises to recapture") + 
  scale_y_continuous(limits = c(0, 1500)) + 
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 270, vjust = 0.5)  
  ) +
  labs(title = "Plateau location",
       y = "Number not recaptured (n)", 
       x = "") 


# patchwork
p_beach / p_konjsko / p_plateau + 
  patchwork::plot_layout(axis_titles = "collect",
                         axes = "collect_x") + 
  patchwork::plot_annotation(title = "Each year-season did they reach the line and recapture all tortoises possible?")



# Save plot ---------------------------------------------------------------
ggsave(filename = here::here("2026", "2026-03-03", "20260303.png"), 
       plot = last_plot(),
       dpi = 300)


## END






















