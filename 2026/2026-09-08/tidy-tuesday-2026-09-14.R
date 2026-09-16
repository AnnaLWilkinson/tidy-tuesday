
# About this script -------------------------------------------------------

#  Purpose: The Cappuccino Index
#  Project: Tidy Tuesday 
#  Author: Anna Wilkinson; anna.wilkinson@burnet.edu.au
#  Date started: 14 September 2026


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(janitor)


# Load data ---------------------------------------------------------------

# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2026-09-08')
## OR
#tuesdata <- tidytuesdayR::tt_load(2026, week = 36)

cafe <- tuesdata$cafe
cappuccino_index <- tuesdata$cappuccino_index

# Option 2: Read directly from GitHub

# cafe <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-09-08/cafe.csv')
# cappuccino_index <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-09-08/cappuccino_index.csv')

# With either option, replace the non-breaking spaces in `country`

cafe$country <- gsub("\u00a0", " ", cafe$country)
cappuccino_index$country <- gsub("\u00a0", " ", cappuccino_index$country)




# Exploratory data analysis -----------------------------------------------

cafe |> 
  ggplot() + 
  geom_density(aes(x = hourly_wage_gbp)) + 
  geom_density(aes(x = price_gbp))


cafe |> 
  ggplot() + 
  geom_density(aes(x = hourly_wage_gbp)) + 
  geom_density(aes(x = price_gbp)) + 
  facet_wrap(~country)


cafe |> 
  ggplot() + 
  geom_point(aes(x = hourly_wage_gbp, 
                 y =price_gbp))



#  Price to wage ratio  ---------------------------------------------------


cafe |> 
  mutate(ratio_wage_price = price_gbp/hourly_wage_gbp, 
         ratio_direction = if_else(ratio_wage_price <1, "neg", "pos")) |> 
  
  group_by(country) |> 
  mutate(obs = n()) |> 
  ungroup() |> 
  
  ggplot() + 
  geom_point(aes(x = ratio_wage_price, 
                 y = reorder(country, obs), 
                 colour = ratio_direction)) + 
  geom_vline(aes(xintercept = 1)) + 
  labs(title = "Cappucino price and barista wages", 
       y = "", 
       x = "") + 

  theme_bw() + 
  theme(
    
    panel.grid = element_blank()
  )


facets <- cafe |> 
  summarise(nobs = n(), .by=country) |> 
  arrange(-nobs) |> 
  mutate(seq   = seq_along(country), 
         group = ntile(row_number(), 4))

cafe |> 
  summarise(avg_hourly_wage_gbp = mean(hourly_wage_gbp, na.rm = TRUE), 
            avg_price_gbp = mean(price_gbp, na.rm = TRUE), 
            nobs = n(), .by = country
  ) |> 
  mutate(ratio_wage_price = avg_price_gbp/avg_hourly_wage_gbp, 
         ratio_direction = if_else(ratio_wage_price <1, "neg", "pos"))  |> 
  
  arrange(-nobs) |> 
  
  mutate(facet = ntile(row_number(), 4)) |> 
  
  ggplot() + 
  geom_point(aes(x = ratio_wage_price, 
                 y = reorder(country, nobs), 
                 colour = ratio_direction,
                 size = nobs)) + 
  geom_vline(aes(xintercept = 1)) + 
  scale_color_grey() + 
  scale_fill_grey() + 
  facet_wrap(~ facet, scales = "free_y") + 
  labs(y = "", 
       x = "") + 
  theme_bw() +
  theme(
  )



df_plot <- cafe |> 
  summarise(avg_hourly_wage_gbp = mean(hourly_wage_gbp, na.rm = TRUE), 
            avg_price_gbp = mean(price_gbp, na.rm = TRUE), 
            nobs = n(), .by = country
  ) |> 
  mutate(ratio_wage_price = avg_price_gbp/avg_hourly_wage_gbp, 
         ratio_direction = if_else(ratio_wage_price <1, "neg", "pos"))  |> 
  
  arrange(-nobs) |> 
  
  mutate(facet = ntile(row_number(), 4)) 




  
ggplot(data = df_plot) + 
  
  geom_rect(data = rect_grid, 
            mapping = aes(ymin = -Inf, 
                          ymax = Inf, 
                          xmin = 0, 
                          xmax = 1,
                          group = facet),
            fill = "grey7") +
  
  geom_rect(
            mapping = aes(ymin = -Inf, 
                          ymax = Inf, 
                          xmin = 1, 
                          xmax = 6,
                          group = facet),
            fill = "white") +
  
  geom_vline(aes(xintercept = 1)) + 
  
  
  geom_count(mapping = aes(x = ratio_wage_price, 
                 y = reorder(country, nobs), 
                 colour = ratio_direction,
                 size = nobs)) + 
  
  scale_size_area(max_size = 5.5,
                  guide = "none") + 
  
  scale_y_discrete(expand = c(0.05,0.05)) +
  scale_x_continuous(expand = c(0.0)) +

  scale_color_manual(values = c("white", "black"), guide = "none") +

  facet_wrap(~ facet, 
             scales = "free_y") + 
  labs(y = "", 
       x = "") + 
  theme_bw() +
  theme(
    
    plot.margin = margin(t =20, r =20 , b =20 , l =20 , unit = "pt"),
    strip.background = element_blank(),
    strip.text = element_blank(),
    plot.background = element_rect(fill = "#f5eee7")
    
  )

