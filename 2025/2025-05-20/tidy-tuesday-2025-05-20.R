
# About this script -------------------------------------------------------

#  Purpose: Water Quality at Sydney Beaches
#  Project: Data Viz Whizzes examples plots
#  Author: Anna Wilkinson; anna.wilkinson@burnet.edu.au
#  Date started: 19 August 2026
#  Date last changed: 



# Load libraries ----------------------------------------------------------

library(tidyverse)

# Load data ---------------------------------------------------------------

# Option 2: Read directly from GitHub

water_quality <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-05-20/water_quality.csv')
weather <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-05-20/weather.csv')


# Process -----------------------------------------------------------------

#derive year
water_quality <- water_quality %>% 
  mutate(year = lubridate::year(date))

water_quality %>% 
  count(year)


# Time series -------------------------------------------------------------

water_quality %>% 
  filter(year ==2024) %>% 
  ggplot(aes(x = date,
                 y = enterococci_cfu_100ml)) + 
  geom_jitter(alpha = 0.2,
              colour = "navy", 
              size = 2) +
  geom_smooth(formula = y ~ splines::bs(x, 100), se = FALSE,
              colour = "red",
              linewidth = 1) + 
  scale_y_continuous(limits = c(0, 2000),
                     labels = scales::label_number(scale = 1e-3, 
                                                   suffix = "K")) + 
  scale_x_date(breaks = seq(as.Date("2024-01-01"), 
                            as.Date("2024-12-31"), 
                            by = "1 month"), 
               date_labels = "%b") + 
  theme_bw(base_size = 10) + 
  theme(
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title.x = element_text(vjust = -3)
    
  ) + 
  labs(title = "Water Quality at Sydney Beaches",
       x = "2024",
       y = "Enterococci bateria/100ml",
       caption = "Enterococci bacteria levels in \ncolony forming units (CFU) \nper 100 millilitres of water")



## expand font

water_quality %>% 
  filter(year ==2024) %>% 
  ggplot(aes(x = date,
             y = enterococci_cfu_100ml)) + 
  geom_jitter(alpha = 0.2,
              colour = "navy", 
              size = 4) +
  geom_smooth(formula = y ~ splines::bs(x, 100), se = FALSE,
              colour = "red",
              linewidth = 1.75) + 
  scale_y_continuous(limits = c(0, 2000),
                     labels = scales::label_number(scale = 1e-3, 
                                                   suffix = "K"),
                     expand = c(0.1,0)) + 
  scale_x_date(breaks = seq(as.Date("2024-01-01"), 
                            as.Date("2024-12-31"), 
                            by = "1 month"), 
               date_labels = "%b") + 
  theme_bw(base_size = 18) + 
  theme(
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title.x = element_text(vjust = -3),
    axis.title.y = element_text(vjust = 3),
    plot.margin = margin(t=40 , l=40 , b=40, r=40, unit="pt")
    
  ) + 
  labs(title = "Water Quality at Sydney Beaches",
       x = "2024",
       y = "Enterococci bateria/100ml",
       caption = "Enterococci bacteria levels in \ncolony forming units (CFU) \nper 100 millilitres of water")










