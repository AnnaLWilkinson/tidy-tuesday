

# About this script -------------------------------------------------------

#  Project:  Tidy Tuesday
#  Purpose:  Salmonid Mortality Data
#  Author:   Anna Wilkinson; anna.wilkinson@burnet.edu.au
#  Date started: 17 March 2026

# Notes on the data:

# Fish Health Report on health and welfare of farmed fished. The Norwegian government goal is to push for lower mortality.
# Libraries ---------------------------------------------------------------

pacman::p_load(rio,
               here,
               tidyverse,
               janitor,
               patchwork,
               scales,
               paletteer)


# Import data -------------------------------------------------------------

# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2026-03-17')
## OR
tuesdata <- tidytuesdayR::tt_load(2026, week = 11)

monthly_losses_data <- tuesdata$monthly_losses_data
monthly_mortality_data <- tuesdata$monthly_mortality_data

# Option 2: Read directly from GitHub

#monthly_losses_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-03-17/monthly_losses_data.csv')
#monthly_mortality_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-03-17/monthly_mortality_data.csv')



# Summary -----------------------------------------------------------------

summary(monthly_losses_data)
summary(monthly_mortality_data)



# Processing --------------------------------------------------------------
range(monthly_losses_data$date)

monthly_losses_data <-  monthly_losses_data %>% 
  mutate(month   = lubridate::month(date),
         year    = lubridate::year(date), 
         monyear = ym(paste(year, "-", month)))

tabyl(monthly_losses_data, month)
tabyl(monthly_losses_data, year)

# looks like geo_group duplicates data - so split

sum(duplicated(c(monthly_losses_data$date, monthly_losses_data$losses)))

list_of_dfs <- split(monthly_losses_data, monthly_losses_data$geo_group)

monthly_losses_data_area   <-  list_of_dfs$area
monthly_losses_data_county <-  list_of_dfs$county


# Exploratory data analysis -----------------------------------------------

monthly_losses_data_area %>% 
  summarise(across(dead:other, sum), .by = monyear) %>% 
  pivot_longer(-monyear,
               names_to = "loss_cat",
               values_to = "number") %>% 
  
  ggplot() + 
  geom_bar(aes(x = monyear, 
               y = number), 
           stat = "identity") + 
  facet_wrap(~ loss_cat, scales = "free_y")




# Pretty plot -------------------------------------------------------------

labels <- c("dead"      = "Dead", 
            "discarded" = "Discarded",
            "escaped"   = "Escaped", 
            "other"     = "Other")

monthly_losses_data_area %>% 
  summarise(across(dead:other, sum), .by = monyear) %>% 
  pivot_longer(-monyear,
               names_to = "loss_cat",
               values_to = "number") %>% 
  
  ggplot() + 
  geom_bar(aes(x = monyear, 
               y = number,
               fill = fct_relevel(loss_cat, "dead", "other", "discarded", "escaped")), 
           stat = "identity") + 
  
  scale_x_date(date_breaks = "6 months",
               date_labels = "%b %Y") + 
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) + 
  scale_fill_paletteer_d("nbapalettes::supersonics_holiday") +
  labs(x = "", 
       y = "Frequency (n)", 
       title = "Norwegian Salmonid losses are mostly mortality \nbut also discarded, escaped, and a lot of 'other'") + 
  
  theme_bw() + 
  
  theme(
    
    axis.text.x = element_text(angle = 310, hjust = 0),
    legend.position = "none"
  ) + 
  
  facet_wrap(~ fct_relevel(loss_cat, "dead", "other", "discarded", "escaped"), 
             scales = "free_y",
             labeller = as_labeller(labels))





 
 
 



















