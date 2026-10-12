
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
library(ggrepel)
library(patchwork)


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
title <- glue('<span style="font-family:{title_font}; font-size:17pt;">Global Human Settlement Urban Centre Database</span><br>Top 20 largest urban centres (geographical size) by World Bank Income Group.')
st    <- "The urban centre name is followed by (area) in km2. The number of hopitals is displayed on the right of each panel. Example is Osaka has an area of 2,212 km2, with 50% of population living within 1km of a hospital, and a 774 hospitals. "
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


top_20_area <- health |> 
  arrange(-GC_UCA_KM2_2025) |> 
  head(20)



top_20_area |> 
  ggplot(aes(x = reorder(GC_UCN_MAI_2025, HL_SHP_HOS_2025),
             y = HL_SHP_HOS_2025,
             size = GC_UCA_KM2_2025,
             shape = GC_DEV_WIG_2025)) + 
  geom_point() + 
  labs(x = "", 
       y = "Living within 1km of a hospital (%)", 
       size = "") + 
  coord_flip()

top_20_area |> 
  ggplot(aes(x = reorder(GC_UCN_MAI_2025, HL_SHP_HOS_2025),
             y = HL_SHP_HOS_2025,
             size = GC_UCA_KM2_2025,
             shape = GC_DEV_WIG_2025)) + 
  geom_count() + 
  scale_size_area(max_size = 10) +
  labs(x = "", 
       y = "Living within 1km of a hospital (%)", 
       size = "") + 
  coord_flip()


top_20_area |> 
  ggplot(aes(x = reorder(GC_UCN_MAI_2025, HL_SHP_HOS_2025),
             y = HL_SHP_HOS_2025,
             size = GC_UCA_KM2_2025)) + 
  geom_count() + 
  scale_size_area(max_size = 10) +
  labs(x = "", 
       y = "Living within 1km of a hospital (%)", 
       size = "") + 
  coord_flip() + 
  facet_grid(~GC_DEV_WIG_2025)

## High income
p_high <- top_20_area |> 
  filter(GC_DEV_WIG_2025 == "High income") |> 
  ggplot(aes(x = reorder(GC_UCN_MAI_2025, HL_SHP_HOS_2025),
             y = HL_SHP_HOS_2025)) + 
  geom_point(shape = 15,
             size = 5) + 
  scale_y_continuous(limits = c(0,100)) +
  scale_size_continuous(limits = c(0, 10000)) +
  labs(x = "", 
       y = "Living within 1km of a hospital (%)") + 
  coord_flip() + 
  theme(
    legend.position = "none",
    axis.text = element_text(size = 4)
  )

p_upper <- top_20_area |> 
  filter(GC_DEV_WIG_2025 == "Upper Middle") |> 
  ggplot(aes(x = reorder(GC_UCN_MAI_2025, HL_SHP_HOS_2025),
             y = HL_SHP_HOS_2025)) + 
  geom_point(shape = 16,
             size = 5) + 
  scale_y_continuous(limits = c(0,100)) +
  scale_size_continuous(limits = c(0, 10000)) +
  labs(x = "", 
       y = "Living within 1km of a hospital (%)") + 
  coord_flip() + 
  theme(
    legend.position = "none"
  )


p_lower <- top_20_area |> 
  filter(GC_DEV_WIG_2025 == "Lower Middle") |> 
  ggplot(aes(x = reorder(GC_UCN_MAI_2025, HL_SHP_HOS_2025),
             y = HL_SHP_HOS_2025)) + 
  geom_point(shape = 17,
             size = 5) + 
  scale_y_continuous(limits = c(0,100)) +
  scale_size_continuous(limits = c(0, 10000)) +
  labs(x = "", 
       y = "Living within 1km of a hospital (%)") + 
  coord_flip() + 
  theme(
    legend.position = "none"
  )


p_high / p_upper / p_lower + 
  patchwork::plot_layout(axis_titles  = "collect",
                         axes = "collect_x") &
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = bg_col, 
                                     colour = bg_col),
      panel.background = element_rect(fill = "white", 
                                      colour = "white"),
      panel.border = element_rect(colour = "grey",
                                  fill= NA)) + 
  canvas(
    width = 8, height = 8,
    units = "in", bg = bg_col,
    dpi = 300
  ) 



# Pretty plot -------------------------------------------------------------

top_20_area$GC_DEV_WIG_2025 <- stringr::str_to_sentence(top_20_area$GC_DEV_WIG_2025)
top_20_area$GC_DEV_WIG_2025 <- factor(top_20_area$GC_DEV_WIG_2025,
                                      levels = c("High income", 
                                                 "Upper middle",
                                                 "Lower middle"))

top_20_area <- top_20_area |> 
  mutate(km = as.character(GC_UCA_KM2_2025),
        city_km = paste0(GC_UCN_MAI_2025, " (", km, ")"))

hosp_num <- top_20_area |> 
  select(GC_UCN_MAI_2025, HL_FCL_HOS_2024, GC_DEV_WIG_2025, city_km) |> 
  mutate(HL_SHP_HOS_2025 = 120,
         HL_FCL_HOS_2024 = as.character(HL_FCL_HOS_2024),
         HL_FCL_HOS_2024 = stringr::str_trim(HL_FCL_HOS_2024),
         )

top_20_area |> 
  ggplot(aes(x = reorder(city_km, HL_SHP_HOS_2025),
             y = HL_SHP_HOS_2025,
             shape = GC_DEV_WIG_2025)) + 
  geom_point(size = 5) + 
  geom_point(data = hosp_num, 
             aes(x = reorder(city_km, HL_SHP_HOS_2025),
                 y = HL_SHP_HOS_2025,
                 shape = GC_DEV_WIG_2025)) +
  geom_label_repel(data = hosp_num, 
                   aes(x = reorder(city_km, HL_SHP_HOS_2025),
                       y = HL_SHP_HOS_2025, 
                       label = HL_FCL_HOS_2024),
                   box.padding   = 0.3, 
                   point.padding = 0.5,
                   segment.color = 'grey50',
                   seed = 1234,
                   direction = "x", 
                   point.size = NA) +
  scale_y_continuous(limits = c(0,125),
                     breaks = seq(0,100, 25),
                     expand = c(0.1, 0.1)) +
  scale_size_continuous(limits = c(0, 10000)) +
  labs(x = "", 
       y = "Living within 1km of a hospital (%)") + 
  coord_flip() + 
  facet_wrap(~ GC_DEV_WIG_2025, scales = "free_y")  + 
  labs(title = title,
       subtitle = st, 
       caption = cap) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "none", 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", colour = bg_col),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    plot.margin = margin(t=20,b=20,l=5,r=5),
    axis.title.x = element_text(vjust = -2),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 12),
    plot.title = element_textbox_simple(
      margin = margin(t = 5, b= 15),
      size = rel(1)
    ),
    plot.subtitle = element_textbox_simple(
      margin = margin(t = 5, b= 20),
      size = rel(1)
    ),
    plot.caption = element_textbox_simple(
      margin = margin(t = 20, b= 0),
      size = rel(0.9)
    ),
  ) + 
  canvas(
    width = 9, height = 9,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> p



# Save plot ---------------------------------------------------------------

save_ggplot(
  plot = p,
  file = file.path("2026", "2026-09-29", paste0("20260929", ".png"))
)

## END






