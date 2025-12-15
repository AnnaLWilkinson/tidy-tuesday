# About this script -------------------------------------------------------

## Project: Tidy Tuesday
## Purpose: Euroleague basketball
## Author: Anna Wilkinson; anna.wilkinson@burnet.edu.au
## Date: 7th October 2025



## https://github.com/rfordatascience/tidytuesday/blob/main/data/2025/2025-10-07/readme.md

## EuroLeague Basketball

# Libraries ---------------------------------------------------------------
pacman::p_load(rio, 
               here, 
               tidyverse,
               janitor,
               lubridate,
               gtsummary,
               scales,
               GGally,
               nullabor,
               ggdist
)


# Import data -------------------------------------------------------------

# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2025-10-07')
## OR
#tuesdata <- tidytuesdayR::tt_load(2025, week = 40)

euroleague_basketball <- tuesdata$euroleague_basketball

# Option 2: Read directly from GitHub

#euroleague_basketball <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-10-07/euroleague_basketball.csv')



# Exploratory analysis ----------------------------------------------------

summary(euroleague_basketball)
skimr::skim(euroleague_basketball)



# Data processing  --------------------------------------------------------

euroleague_basketball <-  euroleague_basketball |> 
  clean_names()

# euroleague_basketball <-  euroleague_basketball |> 
#   mutate(split = str_split_fixed(years_of_final_four_appearances, "," , n = Inf))

euroleague_basketball_split <-  euroleague_basketball |> 
  separate_wider_delim(years_of_final_four_appearances, 
                     delim = ",",
                     names_sep = "_finals_appearance_year", 
                     too_few = "align_start") 


euroleague_basketball_split <-  euroleague_basketball_split |> 
  rename_with(~stringr::str_remove_all(., "years_of_final_four_appearances_"))


# pivot

long_euroleague <-  euroleague_basketball_split |> 
  pivot_longer(cols = starts_with("finals_appearance"),
               names_to = "names", 
               values_to = "values")



# Pretty plots -------------------------------------------------------

long_euroleague <- long_euroleague |> 
  mutate(tohighlight = if_else(team == "Real Madrid", "yes", "no"))
  
long_euroleague |> 
  drop_na(values) |> 
  ggplot() + 
  geom_bar(aes(x = factor(as.numeric(values)),
               fill = tohighlight),
           stat = "count") + 
  scale_fill_manual(values = c("yes" = "#ee324e", "no" = "lightgrey")) + 
  
  labs(title = 'Real Madrid getting better at making finals',
      x = "",
      y = "Count of teams  (n)") + 
  
  theme_bw() + 
  theme(
    legend.position = "none"
  ) + 
  
  coord_flip()


ggsave(filename = here::here("figs", "euroleague_basketball.svg"),
        plot = last_plot())

# note - edited in canva for final plot


## END
