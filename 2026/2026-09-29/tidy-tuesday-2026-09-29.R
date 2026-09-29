
# About this script -------------------------------------------------------

# Project: Tidy Tuesday
# Purpose: Health metrics in urban centres worldwide
# Author: Anna Wilkinson; anna.wilkinson@burnet.edu.au
# Date started: 29 September 2026


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(showtext)
library(glue)
library(ggview)
library(ggtext)


# Load data ---------------------------------------------------------------
# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2026-09-29')
## OR
#tuesdata <- tidytuesdayR::tt_load(2026, week = 39)

health <- tuesdata$health

# Option 2: Read directly from GitHub
#health <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-09-29/health.csv')



# Load fonts --------------------------------------------------------------
font_add_google("Oswald")
font_add_google("Nunito")
font_add_google("Inter")
showtext_auto()
showtext_opts(dpi = 300)
title_font <- "Inter"
body_font <- "Inter"


# Define text -------------------------------------------------------------
title <- glue('<span style="font-family:{title_font}; font-size:17pt;">**XXXXXX**</span><br>XXXXXXXXXXXX.')
st    <- "XXXXXX"
cap   <- glue("**Source**: Global Human Settlement Urban Centre Database Produced by the Joint Research Centre (JRC) of the European Commission")



# Define colours ----------------------------------------------------------

bg_col <- "#F2F4F8"
text_col <- "#151C28"
highlight_col <- "#519623"



# Exploratory data analysis -----------------------------------------------

health |> 
  distinct(GC_UCN_MAI_2025) |> 
  nrow()

health |> 
  distinct(GC_CNT_GAD_2025) |> 
  nrow()

health |> 
  count(GC_DEV_WIG_2025)

health |> 
  count(GC_DEV_USR_2025)

health |> 
  filter(GC_CNT_GAD_2025 == "Australia") |> 
  count(GC_UCN_MAI_2025)

health |> 
  summarise(n = n(), .by = GC_CNT_GAD_2025) |> 
  arrange(-n) |> 
  print(n = 50)

health |> 
  summarise(n = n(), .by = GC_CNT_GAD_2025) |> 
  arrange(n) |> 
  print(n = 30)


health |> 
  summarise(n = n(), .by = GC_CNT_GAD_2025) |>
  filter(n >10 & n <30) |> 
  arrange(-n)|> 
  print(n = 30)

health |> 
  arrange(-GC_UCA_KM2_2025) |> 
  select(GC_UCN_MAI_2025)

top_10_area <- health |> 
  arrange(-GC_UCA_KM2_2025) |> 
  head(10)


top_10_area |> 
  ggplot(aes(x = GC_UCN_MAI_2025, 
             y = HL_SHP_HOS_2025)) + 
  geom_point(aes(size = GC_UCA_KM2_2025)) + 
  labs(x = "", 
       y = "Living within 1km of a hospital (%)", 
       size = "") + 
  coord_flip()


top_10_area |> 
  ggplot(aes(x = GC_UCN_MAI_2025, 
             y = HL_FCL_HOS_2024)) + 
  geom_point(aes(size = GC_UCA_KM2_2025)) + 
  labs(x = "", 
       y = "Number of hospitals", 
       size = "") + 
  coord_flip()


top_10_area |> 
  ggplot(aes(x = GC_UCN_MAI_2025, 
             y = HL_SHP_PHA_2025)) + 
  geom_point(aes(size = GC_UCA_KM2_2025)) + 
  labs(x = "", 
       y = "Living within 1km of a pharmacy (%)", 
       size = "") + 
  coord_flip() 

top_10_area |> 
  ggplot(aes(x = GC_UCN_MAI_2025, 
             y = HL_FCL_PHA_2024)) + 
  geom_point(aes(size = GC_UCA_KM2_2025)) + 
  labs(x = "", 
       y = "Number of pharmacies", 
       size = "") + 
  coord_flip() 
