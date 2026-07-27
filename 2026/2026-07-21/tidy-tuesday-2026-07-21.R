
# About this script -------------------------------------------------------

#  Purpose: Near-Death Experiences (NDERF) Near Death Experience Research Foundation
#  Project: Tidy Tuesday 
#  Author:  Anna Wilkinson; anna.wilkinson@burnet.edu.au
#  Date started: 21 July 2026
#  Date last revised: 


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(scales)


# Load data ---------------------------------------------------------------

# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2026-07-21')
## OR
#tuesdata <- tidytuesdayR::tt_load(2026, week = 29)

nde_experiences <- tuesdata$nde_experiences

# Option 2: Read directly from GitHub
#nde_experiences <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-07-21/nde_experiences.csv')


# Summary -----------------------------------------------------------------
glimpse(nde_experiences)

nde_experiences %>% 
  count(gender)

nde_experiences %>% 
  count(country) %>% 
  arrange(-n)

# A score of 7 or higher indicates a validated near-death experience. 
nde_experiences %>% 
  ggplot() + 
  geom_histogram(aes(x = greyson_score))

# obe          - out-of-body experience
# unity        - feeling of unity or oneness
# hellish      - distressing or hellish imagery
# clinical     - confirmed clinical death
# esp          - extrasensory perception or seeing distant events
# past_lives   - recalling past lives
# world_future - visions of the world's future
# aliens       - detected alien of extraterrestrial encounters

nde_experiences %>% 
  summarise(across(starts_with("ai_"), sum))

# character count
nde_experiences %>% 
  ggplot() + 
  geom_histogram(aes(x = narrative_length)) + 
  scale_x_continuous(labels = label_number(scale_cut = cut_short_scale()) )



# Split data --------------------------------------------------------------

ai_narrative_detection <-  nde_experiences %>% 
  select(entry_id, 
         starts_with("ai_"))

nde_experiences_charac <- nde_experiences %>% 
  select(entry_id, 
         !starts_with("ai_"))
  
  
# Pivot -------------------------------------------------------------------

ai_narrative_long <- ai_narrative_detection %>% 
  pivot_longer(-entry_id, 
               names_to  = "experience",
               values_to = "detected") %>% 
  mutate(detected = if_else(detected == TRUE, "yes", "no"))


# Join --------------------------------------------------------------------

# join characteristics with long df of experiences detected by AI in the narrative

## left join - keeps all rows in the first dataframe - the characteristics is the baseline data frame
nde_experiences_long <-  left_join(nde_experiences_charac, ai_narrative_long, by = "entry_id")

# clean up experiences string
nde_experiences_long <-  nde_experiences_long %>% 
  mutate(experience = str_replace_all(experience, "ai_", ""))

## right join - keeps all rows in second dataframe
nde_experiences_long_subset <-  nde_experiences_long %>% 
  filter(entry_id <=20)

nde_experiences_rj <- right_join(nde_experiences_charac, nde_experiences_long_subset, by = "entry_id")



# Exploratory data analysis -----------------------------------------------


nde_experiences_long %>% 
  filter(detected == "yes") %>% 
  group_by(experience, detected) %>% 
  count() %>% 
  ggplot() + 
  geom_col(aes(x = reorder(experience,-n), 
               y = n)) 





