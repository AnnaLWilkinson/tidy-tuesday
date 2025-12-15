
# About this script -------------------------------------------------------

#  Project: 
#  Purpose: 
#  Author: 
#  Date started:


# Libraries ---------------------------------------------------------------
library(tidyverse)
library(rio)
library(here)
library(janitor)
library(lubridate)
library(GGally)


# Import data -------------------------------------------------------------

# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2025-12-02')
## OR
#tuesdata <- tidytuesdayR::tt_load(2025, week = 48)

sechselaeuten <- tuesdata$sechselaeuten

# Option 2: Read directly from GitHub

#sechselaeuten <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-12-02/sechselaeuten.csv')



# Summary -----------------------------------------------------------------

summary(sechselaeuten)


# Exploratory data analysis -----------------------------------------------

summary(sechselaeuten$duration)  # time to explosion in minutes

# Exploratory plots -------------------------------------------------------

GGally::ggpairs(sechselaeuten, cols = tre200m0:rre150m0)


sechselaeuten |> 
  ggplot(aes( x = year, 
              y = duration)) + 
  geom_point()
  
sechselaeuten |> 
  mutate(count = 1) |> 
  ggplot(aes( x = year, 
              y = duration,
              group = count)) + 
  geom_col()

sechselaeuten |> 
  mutate(count = 1) |> 
  ggplot(aes( x = year, 
              y = duration,
              group = count,
              fill = record)) + 
  geom_col()


sechselaeuten |> 
  ggplot(aes(x = year)) + 
  geom_line(aes(y = tre200m0)) +   # av air temp
  geom_line(aes(y = tre200mn)) +  # min air temp
  geom_line(aes(y = tre200mx))    # maz air temp


sechselaeuten |> 
  ggplot(aes(x = year, 
             y = sre000m0)) +   # total sunshine in hours
  geom_line()
  
sechselaeuten |> 
  ggplot(aes(x = year, 
             y = sremaxmv)) +   # total sunshine as % of possible max
  geom_line()

sechselaeuten |> 
  ggplot(aes(x = year, 
             y = rre150m0)) +   # total rain (mm)
  geom_line()


sechselaeuten |> 
  ggplot(aes(x = tre200mn,
             y = rre150m0)) + 
  geom_point()

tabyl(sechselaeuten, record)



sechselaeuten |> 
  filter(year >1925) |>  
  mutate(count = 1) |> 
  ggplot(aes(x = year, 
             y = tre200m0,
             group = count,
             fill= record)) + 
  geom_col()

sechselaeuten |> 
  filter(year >1925) |>  
  ggplot(aes(x = year, 
             y = tre200m0,
             size = duration,
             colour = tre200m0)) + 
  geom_point() + 
  scale_size(range = c(7, 1), # Reverse the range: large size for small values, small size for large values
             guide = guide_legend(reverse = TRUE)) # Reverse the legend order



# Pretty plot -------------------------------------------------------------

colorRampPalette(c("yellow", "red"))(8)

sechselaeuten |> 
  filter(year >1925) |>  
  ggplot(aes(x = year, 
             y = tre200m0,
             size = duration,
             colour = tre200m0)) + 
  geom_point() + 
  scale_size(range = c(7, 1),   # Reverse the range: large size for small values, small size for large values
             guide = guide_legend(reverse = TRUE))  +  # Reverse the legend order
  scale_colour_distiller(palette = "Oranges", direction = 1) + 
  scale_y_continuous(limits = c(0, 25))


p1 <- sechselaeuten |> 
  filter(year >1925) |>  
  ggplot(aes(x = year, 
             y = tre200m0,
             size = tre200m0)) + 
             #colour = tre200m0)) + 
  geom_point(position = position_jitter(width = 1, height = 1),
             alpha = 0.5) + 
  scale_size_continuous(guide = "none") + 
  
  # scale_size(range = c(7, 1),   # Reverse the range: large size for small values, small size for large values
  #            guide = guide_legend(reverse = TRUE))  +  # Reverse the legend order
  
  #scale_colour_distiller(palette = "Oranges", direction = 1) + 
  scale_y_continuous(limits = c(0, 25)) + 
  scale_x_continuous(minor_breaks = seq(1950,2025,by=5),
                     breaks = seq(1950,2025,by=25),
                     limits = c(1950,2025),
                     expand = c(0.05,0),
                     guide = guide_axis(minor.ticks = TRUE)) +
  labs(y = "Average air temperature (C)",
       x = "") + 
  theme_bw() +
  theme(
    aspect.ratio = 1
  )

p2 <- sechselaeuten |> 
  filter(year >1925) |>  
  ggplot(aes(x = year, 
             y = duration,
             size = duration)) +
             #colour = duration)) + 
  geom_point(alpha = 0.5) + 
  scale_y_reverse() + 
  scale_size_continuous(guide = "none") + 
  
  # scale_size(range = c(7, 1),   # Reverse the range: large size for small values, small size for large values
  #            guide = guide_legend(reverse = TRUE))  +  # Reverse the legend order
  
  scale_colour_distiller(palette = "Blues", direction = 1) + 
  #scale_y_continuous(limits = c(0, 70)) + 
  scale_x_continuous(minor_breaks = seq(1950,2025,by=5),
                     breaks = seq(1950,2025,by=25),
                     limits = c(1950,2025),
                     expand = c(0.05,0),
                     guide = guide_axis(minor.ticks = TRUE)) +
  labs(y = "Time to Boeoeg explosion (mins)",
       x = "") + 
  theme_bw()

p1 + p2 + 
  patchwork::plot_annotation( title = "It's getting warmer over time, 
but recently its taken longer to explode the Boeoeg (Snowman)", 
                              caption = "The Boeoegg is a snowman effigy made of cotton wool and stuffed with fireworks, 
created every year for Zurich's Sechselaeuten spring festival. The saying 
goes that the quicker the Boeoeg's head explodes, the finer the summer will be"
    
  )


ggsave(filename = here::here("figs", "snowman_weather.jpeg"),
       plot = last_plot())






