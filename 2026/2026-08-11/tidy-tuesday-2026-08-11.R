
# About this script -------------------------------------------------------

#  Purpose: the Palomar Spectroscopic Survey of Nearby Galaxies
#  Project: Tidy Tuesday  
#  Author:  Anna Wilkinson; anna.wilkinson@burnet.edu.au
#  Date started: 11 August 2026


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(scales)

# Load data ---------------------------------------------------------------

# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2026-08-11')
## OR
#tuesdata <- tidytuesdayR::tt_load(2026, week = 32)

palomar_emission_lines <- tuesdata$palomar_emission_lines
palomar_survey <- tuesdata$palomar_survey

# Option 2: Read directly from GitHub

# palomar_emission_lines <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-08-11/palomar_emission_lines.csv')
# palomar_survey <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-08-11/palomar_survey.csv')
# 

# Summary -----------------------------------------------------------------

summary(palomar_emission_lines)
summary(palomar_survey)

palomar_survey %>% 
  count(galaxy_name)

palomar_survey %>% 
  count(activity_type)

palomar_survey %>% 
  count(classification_confidence)

palomar_survey %>% 
  count(classification_confidence, activity_type)

palomar_survey %>% 
  count(hubble_type) %>% 
  arrange(-n)



# Join data ---------------------------------------------------------------

df2 <- palomar_survey %>% 
  select(galaxy_name, 
         activity_type, 
         activity_subtype)

palomar_emission_lines <- left_join(palomar_emission_lines, df2, by = "galaxy_name")


# BPT diagram -------------------------------------------------------------

palomar_emission_lines %>% 
  ggplot() + 
  geom_point(aes(x = nii_6583, 
                 y = oiii_5007)) + 
  scale_x_log10(breaks = c(0.001, 0.1, 1, 10),
                labels = c(0.001, 0.1, 1, 10),
                expand = expansion(0.8)) + 
  scale_y_log10(breaks = c(0.1, 1, 10)) + 
  annotation_logticks(base = 10)

palomar_emission_lines %>% 
  ggplot() + 
  geom_point(aes(x = nii_6583, 
                 y = oiii_5007,
                 shape = activity_type)) + 
  scale_x_log10(breaks = c(0.001, 0.1, 1, 10),
                labels = c(0.001, 0.1, 1, 10),
                expand = expansion(0.8)) + 
  scale_y_log10(breaks = c(0.1, 1, 10)) + 
  annotation_logticks(base = 10)

# use provided log transformed values
palomar_survey %>% 
  ggplot() + 
  geom_point(aes(x = log_nii_ha, 
                 y = log_oiii_hb)) +
  scale_y_log10() + 
  scale_x_log10()

palomar_survey %>% 
  ggplot() + 
  geom_point(aes(x = log_nii_ha, 
                 y = log_oiii_hb,
                 shape = activity_type)) +
  scale_y_log10() + 
  scale_x_log10()


# restrict to H11, Seyfert and LINER

palomar_survey %>% 
  filter(activity_type %in% c("H II", "LINER", "Seyfert")) %>% 
  ggplot() + 
  geom_point(aes(x = log_nii_ha, 
                 y = log_oiii_hb,
                 shape = activity_type)) +
  scale_y_log10() + 
  scale_x_log10()



# Other exploratory analysis ----------------------------------------------

palomar_survey %>% 
  ggplot() + 
  geom_jitter(aes(x  = activity_type, 
                  y = b_magnitude))


palomar_survey %>% 
  ggplot() + 
  geom_jitter(aes(x = activity_type,
                  y = velocity_dispersion_km_s))



palomar_survey %>% 
  ggplot() + 
  geom_jitter(aes(x = hubble_type,
                  y = activity_type))












