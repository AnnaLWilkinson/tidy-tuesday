# About this script -------------------------------------------------------

## Project: Tidy Tuesday
## Purpose: Crane Observations Swedeon 
## Author: Anna Wilkinson; anna.wilkinson@burnet.edu.au
## Date: 30 September 2025



## https://github.com/rfordatascience/tidytuesday/blob/main/data/2025/2025-09-30/readme.md

## Crane Observations at Lake Hornborgasjön, Sweden (1994–2024)

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

tuesdata <- tidytuesdayR::tt_load('2025-09-30')
## OR
#tuesdata <- tidytuesdayR::tt_load(2025, week = 39)

cranes <- tuesdata$cranes

# Option 2: Read directly from GitHub

#cranes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-30/cranes.csv')



# Process dates -----------------------------------------------------------

cranes$day_of_week <-  lubridate::wday(cranes$date, label = TRUE, abbr = FALSE, 
                                       week_start = 1)
cranes$year  <-  lubridate::year(cranes$date)

# Exploratory data analysis -----------------------------------------------

summary(cranes)

cranes |> 
ggplot() + 
  geom_point(aes(x = date, 
                 y = observations))

cranes |> 
  ggplot() +
  geom_bar(aes( x= day_of_week, 
                y = observations),
           stat = "identity")


cranes |> 
  ggplot() +
  geom_bar(aes( x= day_of_week, 
                y = observations),
           stat = "identity") + 
  facet_wrap(~ year)


cranes |> 
  ggplot() + 
  geom_point(aes(x = date, 
                 y = observations)) + 
  facet_wrap(~ day_of_week)


cranes |> 
  ggplot() + 
  geom_point(aes(x = date,
                 y = observations,
                 colour = day_of_week)) + 
  gghighlight::gghighlight() + 
  facet_wrap(~day_of_week) + 
  scale_colour_grey()



cranes |> 
  ggplot() + 
  geom_point(aes(x = date,
                 y = observations,
                 colour = day_of_week)) + 
  gghighlight::gghighlight() + 
  facet_wrap(~day_of_week) + 
  scale_colour_manual(values = c("black",
                                 "black",
                                 "black",
                                 "black",
                                 "black",
                                 "black",
                                 "black"))


# Pretty plot -------------------------------------------------------------


cranes |> 
  ggplot() + 
  geom_point(aes(x = date,
                 y = observations,
                 colour = day_of_week)) + 
  gghighlight::gghighlight() + 
  facet_wrap(~day_of_week) + 
  scale_colour_manual(values = c("black",
                                 "black",
                                 "black",
                                 "black",
                                 "black",
                                 "black",
                                 "black")) +
  
  scale_x_date(breaks = "5 year",
               date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 30000)) + 
  
  labs(title = "Cranes in Sweden observed more on Monday and Thursday",
       x = "",
       y = "Number of observations (n)") + 
  
  theme_bw() + 
  
  theme(
    panel.grid.major.y = element_line(colour = "lightgrey"),
    panel.grid.major.x = element_line(colour = "lightgrey"),
  )

ggsave(filename = here::here("figs", "cranes_sweden.jpeg"),
       plot = last_plot())


## END




