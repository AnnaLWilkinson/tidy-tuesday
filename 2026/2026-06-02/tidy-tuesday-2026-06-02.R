
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
library(sysfonts)
library(gganimate)


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

font_add_google("Oswald")
font_add_google("Nunito")
font_add_google("Montserrat")
font_add_google("Roboto")
font_add_google("Atkinson Hyperlegible Next")
sysfonts::font_families()
showtext_auto()
showtext_opts(dpi = 300)
title_font <- "sans"
body_font <- "sans"




# Define colours ----------------------------------------------------------

bg_col <- "#F2F4F8"
text_col <- "#151C28"


# Define text -------------------------------------------------------------

# anything in curly braces is interpreted as R code

title = glue('<span style="font-family:{title_font}; font-size:17pt;">**European Parenting Leave Policies**</span>')
st    = "The European Parenting Leave Policies (EPLP) Dataset provides harmonised data on maternity, co-parent, paid parental, and job-protected leave regulations across 21 European countries from 1970 to 2024."
cap   = "**Source**: The European Parenting Leave Policies (EPLP) Dataset"


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

## Static

eplp_whom |> 
  mutate(par1_for_whom = stringr::str_to_sentence(par1_for_whom)) |> 
  ggplot() + 
  geom_point(aes(x = year, 
                 y = country_name_fct, 
                 group = 1,
                 fill= par1_for_whom),
             size = 3,
             shape = 21) + 
  scale_fill_manual(values =  c("Not applicable" = "#BDBDBD",
                                "Mothers" = "#E69F00", 
                                "Either" = "#0072B2")) +

  scale_y_discrete(limits = rev) + 
  labs(x = "", 
       y = "", 
       fill = "",
       title = title,
       subtitle = st,
       caption = cap) +
  theme_minimal(base_size = 11) + 
  theme(
    
    plot.margin = margin(0,5,5,5),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    panel.grid = element_blank(), 

    plot.title = element_textbox_simple(
      colour = text_col,
      size = rel(1),
      margin = margin(b = 5, t = 5)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col, 
      size = rel(0.9), 
      hjust = 0, 
      halign = 0, 
      margin = margin (b = 5, t = 5),
      family = body_font
    ), 
    plot.caption = element_textbox_simple(
      colour = text_col, 
      size = rel(0.9), 
      hjust = 0, 
      halign = 0, 
      margin = margin (b = 0, t = 10),
      family = body_font
    )
  ) + 
  coord_cartesian(ylim = c(22, 0.5)) + 
  canvas(
    width = 9, height = 9,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> p

# Save static--------------------------------------------------------------------

save_ggplot(
  plot = p,
  file = file.path("2026", "2026-06-02", paste0("20260602", ".png"))
)



# Animate -----------------------------------------------------------------

title = glue('<span style="font-family:{title_font}; font-size:8pt;">**European Parenting Leave Policies**</span>')
st    = "The European Parenting Leave Policies (EPLP) Dataset provides harmonised data on maternity, co-parent, paid parental, and job-protected leave regulations across 21 European countries from 1970 to 2024."
cap   = "**Source**: The European Parenting Leave Policies (EPLP) Dataset"



eplp_whom |> 
  mutate(par1_for_whom = stringr::str_to_sentence(par1_for_whom)) |> 
  ggplot() + 
  geom_point(aes(x = year, 
                 y = country_name_fct, 
                 group = 1,
                 fill= par1_for_whom),
             size = 2,
             shape = 21) + 
  scale_fill_manual(values =  c("Not applicable" = "#BDBDBD",
                                "Mothers" = "#E69F00", 
                                "Either" = "#0072B2")) +
  
  scale_y_discrete(limits = rev) + 
  labs(x = "", 
       y = "", 
       fill = "", 
       title = "European parental leave policies") +
  theme_minimal(base_size = 7) +
  theme(
    plot.margin = margin(20,60,40,40),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    panel.grid = element_blank()) +
    
  transition_time(year) +
  shadow_mark() -> p

p_anim <- animate(
  p,
  width = 1024,
  height = 768,
  res = 150,
  nframes = 600,
  fps = 40,
  renderer = gifski_renderer(file = "eplp.gif")
)

p_anim 
anim_save("2026/2026-06-02/eplp.gif", animation = p_anim)


## END



