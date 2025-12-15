
# About this script -------------------------------------------------------

# Project: Tidy Tuesday 
# Purpose: Lead concentration in Flint water samples in 2015
# Author: Anna Wilkinson; anna.wilkinson@burnet.edu.au
# Date started: 05 Nov 2025


# https://github.com/rfordatascience/tidytuesday/blob/main/data/2025/2025-11-04/readme.md

# Libraries ---------------------------------------------------------------

pacman::p_load(
  rio, 
  here,
  tidyverse,
  lubridate,
  gtsummary
  
)


# Data --------------------------------------------------------------------

# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2025-11-04')
## OR
#tuesdata <- tidytuesdayR::tt_load(2025, week = 44)

flint_mdeq <- tuesdata$flint_mdeq
flint_vt <- tuesdata$flint_vt

# Option 2: Read directly from GitHub

#flint_mdeq <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-11-04/flint_mdeq.csv')
#flint_vt <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-11-04/flint_vt.csv')


# Summary -----------------------------------------------------------------

# Michigan Dept Environment (MDEQ)
# Viriginia Tech

summary(flint_mdeq)
summary(flint_vt)

# Exploratory data analysis ----------------------------------------------

flint_mdeq |> 
  ggplot() + 
  geom_histogram(aes(x = lead))

flint_vt |> 
  ggplot() + 
  geom_histogram(aes(x = lead))


flint_mdeq |> 
  ggplot() + 
  geom_density(aes(x = lead))

flint_vt |> 
  ggplot() + 
  geom_density(aes(x = lead))

flint_mdeq |> 
  summarise(mean_lead   = mean(lead, na.rm = TRUE),
            sd_lead     = sd(lead, na.rm = TRUE),
            median_lead = median(lead, na.rm = TRUE),
            IQR         = IQR(lead, na.rm = TRUE)
            )

flint_mdeq |> 
  summarise(mean_lead   = mean(lead2, na.rm = TRUE),
            sd_lead     = sd(lead2, na.rm = TRUE),
            median_lead = median(lead2, na.rm = TRUE),
            IQR         = IQR(lead2, na.rm = TRUE)
  )



flint_vt |> 
  summarise(mean_lead   = mean(lead, na.rm = TRUE),
            sd_lead     = sd(lead, na.rm = TRUE),
            median_lead = median(lead, na.rm = TRUE),
            IQR         = IQR(lead, na.rm = TRUE)
  )




# Append data -------------------------------------------------------------


flint_mdeq <-  flint_mdeq |> 
  mutate(study = "Michigan Department of Environment")

flint_vt <- flint_vt |> 
  mutate(study  = "Virginia Tech (citizen science)")

flint_mdeq_vt <-  bind_rows(flint_mdeq, flint_vt) 

flint_mdeq_vt <- flint_mdeq_vt |> 
  mutate(lead2 = if_else(is.na(lead2) & study == "Virginia Tech (citizen science)", lead, lead2))


flint_mdeq_vt |> 
  ggplot() + 
  geom_histogram(aes(x = lead2,
                     fill = study))



# Pretty plot -------------------------------------------------------------


flint_mdeq_vt |> 
  ggplot() + 
  geom_histogram(aes(x = lead2,
                     fill = study),
                 alpha = 0.6) + 
  labs(x = "Lead level in parts per billion",
       y = "Frequency (n)", 
       fill = "",
       title = "A citizen science had more samples with higher lead levels") + 
  scale_y_continuous(limits = c(0,150), 
                     breaks = seq(0, 150, 50)) + 
  scale_fill_manual(values = c("darkblue", "darkorange")) + 
  
  theme_bw() + 
  theme(
    
    aspect.ratio = 1
  )


ggsave(filename = here::here("figs", "lead_michigan.jpeg"),
       plot = last_plot())
## END





