
# About this script -------------------------------------------------------

# Purpose: Tidy Tuesday 
# Project: European Parenting Leave Policies
# Author: Anna Wilkinson; anna.wilkinson@burnet.edu.au
# Date started: 24 September 2026



# Load libraries ----------------------------------------------------------

library(tidyverse)
library(countrycode)
library(showtext)
library(glue)
library(ggview)
library(ggtext)


# Load data ---------------------------------------------------------------

# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2026-06-02')
## OR
#tuesdata <- tidytuesdayR::tt_load(2026, week = 22)

eplp <- tuesdata$eplp

# Option 2: Read directly from GitHub

#eplp <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-02/eplp.csv')


# Load fonts --------------------------------------------------------------




# Define colours ----------------------------------------------------------

bg_col <- "#F2F4F8"
text_col <- "#151C28"
geom_palette <-  c("Not applicable" = "#BDBDBD","mothers" = "#E69F00", "either" = "#0072B2")


# Define text -------------------------------------------------------------

title = "European Parenting Leave Policies"
st    = "The European Parenting Leave Policies (EPLP) Dataset provides harmonised data on maternity, co-parent, paid parental, and job-protected leave regulations across 21 European countries from 1970 to 2024."
cap   = "Source:"


# Data wrangling -----------------------------------------------


eplp$country_name <- countrycode(eplp$country, "iso2c", "country.name", custom_match = c("UK" = "UK"))

table(eplp$par1_for_whom)
table(eplp$country, eplp$par1_for_whom)

eplp_whom <- eplp |> 
  select(country, 
         country_name,
         year, 
         par1_for_whom)

eplp_whom <- eplp_whom |> 
  group_by(country, par1_for_whom) |> 
  mutate(seq = seq_along(year),
         intro_yr_either = if_else(seq ==1 & par1_for_whom=="either", year, NA)) |> 
  ungroup()

country_order <- eplp_whom |> 
  select(country_name, intro_yr_either) |> 
  group_by(country_name) |> 
  fill(intro_yr_either, .direction = "updown") |> 
  distinct(country_name, .keep_all = TRUE) |> 
  arrange(intro_yr_either) |> 
  select(country_name) |> 
  unlist() 

eplp_whom$country_name_fct = factor(eplp_whom$country_name, levels = c(country_order))  


# Plot --------------------------------------------------------------------


eplp_whom |> 
  ggplot() + 
  geom_point(aes(x = year, 
                 y = country_name_fct, 
                 group = 1,
                 fill= par1_for_whom),
             size = 4.5,
             shape = 21) + 
  scale_fill_manual(values = geom_palette) + 
  scale_y_discrete(limits = rev) + 
  theme_minimal(base_size = 10) + 
  theme(
    
    panel.grid = element_blank(), 
    plot.title = element_textbox_simple()
  ) + 
  labs(x = "", 
       y = "", 
       fill = "",
       title = title)









