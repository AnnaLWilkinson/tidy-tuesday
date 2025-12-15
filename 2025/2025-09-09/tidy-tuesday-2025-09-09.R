
# About this script -------------------------------------------------------

## Project: Tidy Tuesday
## Purpose: Henley Passport Index Data
## Author: Anna Wilkinson; anna.wilkinson@burnet.edu.au
## Date: 9 September 2025




# Libraries ---------------------------------------------------------------
pacman::p_load(rio, 
               here, 
               tidyverse,
               janitor,
               lubridate,
               gtsummary,
               scales
               )




# Import data -------------------------------------------------------------

# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2025-09-09')
## OR
#tuesdata <- tidytuesdayR::tt_load(2025, week = 36)

country_lists <- tuesdata$country_lists
rank_by_year <- tuesdata$rank_by_year

# Option 2: Read directly from GitHub

# country_lists <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-09/country_lists.csv')
# rank_by_year <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-09/rank_by_year.csv')


# Summary -----------------------------------------------------------------
summary(rank_by_year)



# Exploratory plots ----------------------------------------------------

rank_by_year |> 
  group_by(year) |> 
  summarise(n_count = sum(visa_free_count)) |> 

  ggplot() + 
  geom_bar(aes(x = year,
               y = n_count),
           stat = "identity")

rank_by_year |> 
  group_by(year, region) |> 
  summarise(n_count = sum(visa_free_count)) |> 

  ggplot() + 
  geom_bar(aes(x = year,
               y = n_count),
           stat = "identity") + 
  
  facet_wrap(~region, scales = "free_y")

rank_by_year |> 
  group_by(year, region) |> 
  summarise(n_count = sum(visa_free_count)) |> 
  
  ggplot() + 
  geom_line(aes(x = year,
                y = n_count,
                groups=region))

rank_by_year |> 
  filter(year>=2010) |> 
  group_by(year, region) |> 
  summarise(n_count = sum(visa_free_count)) |> 
  
  ggplot() + 
  geom_line(aes(x = year,
                y = n_count,
                groups=region))



rank_by_year |> 
  group_by(year, region) |> 
  summarise(n_count = sum(visa_free_count)) |> 
  
  ggplot() + 
  geom_line(aes(x = year,
               y = n_count)) + 
  
  facet_wrap(~region, scales = "free_y")


rank_by_year |> 
  filter(year>=2010) |> 
  filter(region == "EUROPE") |> 
  group_by(country, year) |> 
  summarise(n_count = sum(visa_free_count))  |> 

  ggplot() + 
  geom_line(aes(x = factor(year),
                y = n_count,
                group = country,
                colour = country))


rank_by_year |> 
  filter(region == "EUROPE") |> 
  distinct(country) |> 
  nrow()


rank_by_year |> 
  filter(year>=2010) |> 
  filter(region == "EUROPE") |> 
  group_by(country, year) |> 
  summarise(n_count = sum(visa_free_count))  |> 
  
  ggplot() + 
  geom_point(aes(x = factor(year),
                y = n_count,
                group = country,
                colour = country,
                size = n_count))


rank_by_year |> 
  filter(year >=2010) |> 
  summarise(n_count = sum(visa_free_count), .by = c(region, year)) |> 
  ggplot(aes(x = factor(year), 
             y = n_count,
             colour = region,
             group = region)) + 
  geom_point() + 
  geom_line() 


rank_by_year |> 
  filter(year >=2010) |> 
  summarise(n_count = sum(visa_free_count), .by = c(region, year)) |> 
  ggplot(aes(x = factor(year), 
             y = n_count,
             colour = region,
             group = region)) + 
  geom_point() + 
  geom_smooth(stat = "smooth") 


rank_by_year |> 
  filter(year >=2010) |> 
  filter(region == "EUROPE") |> 
  summarise(n_count = sum(visa_free_count), .by = c(country, year)) |> 
  ggplot(aes(x = factor(year), 
             y = n_count,
             colour = country,
             group = country)) + 
  geom_point()  



# Pretty plots -------------------------------------------------------


p <- rank_by_year |> 
  filter(year >=2010) |> 
  summarise(n_count = sum(visa_free_count), .by = c(region, year)) |> 
  ggplot(aes(x = factor(year), 
             y = n_count,
             colour = region,
             group = region)) + 
  
  geom_point() + 
  geom_smooth(stat = "smooth") + 
  
  scale_y_continuous(limits = c(0, 10000),
                     labels = label_number(scale_cut = scales::cut_short_scale()),
                     expand = c(0,0)) + 
  scale_x_discrete(expand = expansion(add = c(0.5,0.5))) +
  
  labs(title = "Henley Passport Index Data", 
       subtitle = "Count of visa free by region by time",
       x = "", 
       y = "Count of visa free (n)",
       colour = "") + 
  
  theme_bw()


ggsave(filename = here::here("figs", "henley_passport.jpeg"),
       plot = p)


## END