
# About this script -------------------------------------------------------

#  Purpose: Tidy Tuesday 
#  Project: Global Health Spending
#  Author: Anna Wilkinson; anna.wilkinson@burnet.edu.au
#  Date started: 27th April 2026
#  Date last changed: 


# Load libraries ----------------------------------------------------------

library(rio)
library(here)
library(janitor)
library(scales)
library(cowplot)
library(grid)
library(ggtext)
library(tidyverse)


# Load data ---------------------------------------------------------------

# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2026-04-21')
## OR
#tuesdata <- tidytuesdayR::tt_load(2026, week = 16)

financing_schemes <- tuesdata$financing_schemes
health_spending <- tuesdata$health_spending
spending_purpose <- tuesdata$spending_purpose

# Option 2: Read directly from GitHub

# financing_schemes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-21/financing_schemes.csv')
# health_spending <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-21/health_spending.csv')
# spending_purpose <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-21/spending_purpose.csv')


# Summary -----------------------------------------------------------------
glimpse(financing_schemes)
glimpse(health_spending)
glimpse(spending_purpose)

# Exploratory data analysis -----------------------------------------------

tabyl(spending_purpose, indicator_code)
tabyl(spending_purpose, country_name)
tabyl(spending_purpose, year)
tabyl(spending_purpose, spending_purpose)

tabyl(financing_schemes, year)
tabyl(financing_schemes, country_name)



## Australia ---------------------------------------------------------------

# financing scheme
my_australia <- financing_schemes %>% 
  filter(country_name == "Australia")

my_australia %>% 
  filter(unit == "% of current health expenditure") %>% 
  ggplot(aes(x = year, 
                y = value,
                group = financing_scheme,
                colour = financing_scheme)) + 
  geom_point() +
  geom_line() 

my_australia %>% 
filter(unit == "constant 2023 US$") %>% 
  ggplot(aes(x = year, 
             y = value,
             group = financing_scheme,
             colour = financing_scheme)) + 
  geom_point() +
  geom_line() 

my_australia %>% 
  filter(unit == "constant 2023 US$") %>% 
  filter(!str_detect(financing_scheme, "Rest")) %>% 
  ggplot(aes(x = factor(year), 
             y = value,
             group = financing_scheme)) + 
  geom_col()  +
  facet_wrap(~financing_scheme, nrow = 2) + 
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()))


my_australia %>% 
  filter(unit == "constant 2023 US$", 
         str_detect(financing_scheme, "Household") | str_detect(financing_scheme, "Voluntary"))  %>% 
  ggplot(aes(x = factor(year), 
             y = value,
             group = financing_scheme)) + 
  geom_col()  +
  facet_wrap(~financing_scheme) + 
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()))


#health spending
my_aus_health_spending <- health_spending %>% 
  filter(country_name == "Australia")


my_aus_health_spending %>% 
  filter(indicator_code == "che_usd2023") %>% 
  ggplot(aes(x = year, 
             y = value))  +
  geom_col() + 
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()))




# Pretty plot -------------------------------------------------------------


p <- my_australia %>% 
  filter(unit == "constant 2023 US$", 
         str_detect(financing_scheme, "Household") | str_detect(financing_scheme, "Voluntary"))  %>% 
  ggplot(aes(x = factor(year), 
             y = value,
             group = financing_scheme)) + 
  geom_col()  +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()), 
                     expand = expansion(mult = c(0, 0.1))) + 
  scale_x_discrete(breaks = seq(2000, 2023, 3), 
                   expand = expansion(mult = c(0, 0))) + 
  labs(y = "Spending in 2023 USD billions", 
       x = "", 
       caption = "Source: WHO Global Health Expenditure" , 
       tag = str_wrap("The cost of healthcare to individuals continues to rise", 17)) + 
  theme(
    
    plot.margin = margin(30,30,30,30),
    panel.spacing = unit(2, "lines"), 
    plot.background = element_rect(fill = "white"), 
    panel.background = element_rect(fill = "white"),
    panel.grid.major.y = element_line(colour = "grey77"), 
    axis.title.y =  element_text(vjust = 4),
    strip.text.x = element_text(face = "bold",
                                size = 12),
    strip.background.x = element_rect(fill = "white",
                                      colour = "darkgrey"),
    plot.tag.position = c(0.2, 0.8)

  ) + 
  facet_wrap(~financing_scheme) 

p

## means test rebate introduced in 1 July 2012
## medicare levy surcharge income tiers also changed

ggdraw(p) + 
  draw_text(
    x = 0.6, 
    y = 0.62, 
    size = 12,
    hjust = 0,
    text = str_wrap("Policy changes occurred 1 July 2012 to incentise uptake of private health insurance", 17),
    colour= "navy"
  ) +
  
  draw_grob(
    curveGrob(
      x1 = 0.62, y1 = 0.54,
      x2 = 0.72, y2 = 0.48,
      curvature = 0.3,
      gp = gpar(lwd = 1.8, fill = "navy", colour = "navy"),
      arrow = arrow(type = "closed", length = unit(0.07, "inches"))
    ))












