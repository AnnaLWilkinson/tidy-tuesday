
# About this script -------------------------------------------------------

# Purpose: Tidy Tuesday
# Project: UFC Athletes and Fight Data
# Author: Anna Wilkinson
# Date started: 7th July 2026


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(cowplot)
library(patchwork)
library(grid)
library(ggview)


# Load data ---------------------------------------------------------------

# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2026-07-07')
## OR
#tuesdata <- tidytuesdayR::tt_load(2026, week = 27)

ufc_athletes <- tuesdata$ufc_athletes
ufc_fights <- tuesdata$ufc_fights
ufc_rankings_dataset <- tuesdata$ufc_rankings_dataset
ufcstats_data <- tuesdata$ufcstats_data
ultimate_ufc_dataset <- tuesdata$ultimate_ufc_dataset

# Option 2: Read directly from GitHub

ufc_athletes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-07-07/ufc_athletes.csv')
ufc_fights <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-07-07/ufc_fights.csv')
ufc_rankings_dataset <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-07-07/ufc_rankings_dataset.csv')
ufcstats_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-07-07/ufcstats_data.csv')
ultimate_ufc_dataset <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-07-07/ultimate_ufc_dataset.csv')



# Summary of data  --------------------------------------------------------

head(ufc_athletes)
head(ufc_fights)
head(ufc_rankings_dataset)
head(ultimate_ufc_dataset)


# Exploratory data analysis -----------------------------------------------
summary(ufc_athletes$height)  ## height is in inches
summary(ufc_athletes$weight)  ## weight is in pounds
table(ufc_athletes$weight_class)

ufc_athletes %>% 
  distinct(weight_class) %>% 
  unlist()


# height & weight
ufc_athletes %>% 
  filter(!is.na(height) & !is.na(weight) & height >0 & weight >0) %>% 
  ggplot() + 
  geom_point(aes(x = weight, 
                 y = height))


# height and weight by weight class
ufc_athletes %>% 
  filter(!is.na(height) & !is.na(weight) & height >0 & weight >0 & !is.na(weight_class)) %>% 
  ggplot() + 
  geom_point(aes(x = weight, 
                 y = height)) + 
  facet_wrap(~weight_class)


# reach
summary(ufc_athletes$reach)  # in inches
summary(ufc_athletes$leg_reach)  # in inches


ufc_athletes %>% 
  ggplot() + 
  geom_point(aes(x = reach, 
                 y = leg_reach))

ufc_athletes %>% 
  ggplot() + 
  geom_point(aes(x = reach, 
                 y = leg_reach,
                 size = height))


ufc_athletes %>% 
  ggplot() + 
  geom_point(aes(x = reach, 
                 y = leg_reach,
                 size = weight))


ufc_athletes %>% 
  select(weight_class, age, height, weight, reach, leg_reach) %>% 
  filter(!is.na(weight_class)) %>% 
  summarise(across(c(weight, height, reach, leg_reach), list(mean = mean, sd = sd), na.rm = TRUE), .by = weight_class
  )


ufc_athletes %>% 
  filter(!is.na(height) & !is.na(weight) & height >0 & weight >0 & !is.na(weight_class)) %>% 
  ggplot() + 
  geom_jitter(aes(y = weight_class, 
                  x = weight))


## ordering weight class

weight_levels <- c(
  "Flyweight",
  "Bantamweight",
  "Featherweight",
  "Lightweight",
  "Welterweight",
  "Middleweight",
  "Light Heavyweight",
  "Heavyweight",
  "Women's Strawweight",
  "Women's Flyweight",
  "Women's Bantamweight",
  "Women's Featherweight"
)

ufc_athletes %>% 
  filter(!is.na(height) & !is.na(weight) & height >0 & weight >0 & !is.na(weight_class)) %>% 
  mutate(weight_class = factor(weight_class, 
                               levels = weight_levels, 
                               ordered = TRUE)) %>% 
  ggplot() + 
  geom_jitter(aes(y = weight_class, 
                  x = weight))


ufc_athletes %>% 
  filter(!is.na(height) & !is.na(weight) & height >0 & weight >0 & !is.na(weight_class)) %>% 
  mutate(weight_class = factor(weight_class, 
                               levels = weight_levels, 
                               ordered = TRUE)) %>% 
  ggplot() + 
  geom_jitter(aes(y = weight_class, 
                  x = height))

summary_stats <- ufc_athletes %>% 
  select(weight_class, age, height, weight, reach, leg_reach) %>% 
  filter(!is.na(weight_class)) %>% 
  summarise(across(c(weight, height, reach, leg_reach), list(mean = mean, sd = sd), na.rm = TRUE), .by = weight_class
  ) %>% 
  mutate(weight_class = factor(weight_class, 
                               levels = weight_levels, 
                               ordered = TRUE))

summary_stats <-  summary_stats %>% 
  mutate(height = 87,
         weight = 280)


ufc_athletes %>% 
  filter(!is.na(height) & !is.na(weight) & height >0 & weight >0 & !is.na(weight_class)) %>% 
  mutate(weight_class = factor(weight_class, 
                               levels = weight_levels, 
                               ordered = TRUE)) %>% 
  ggplot() + 
  geom_jitter(aes(y = weight_class, 
                  x = height)) + 
  geom_point(data = summary_stats, 
             mapping = aes(x = height, 
                           y = weight_class),
             alpha = 0) + 
  geom_text(data = summary_stats, 
            mapping = aes(x = height, 
                          y = weight_class,
                          label = sprintf("%.1f", height_mean))) + 
  labs(x = "Height in inches")


ufc_athletes %>% 
  filter(!is.na(height) & !is.na(weight) & height >0 & weight >0 & !is.na(weight_class)) %>% 
  mutate(weight_class = factor(weight_class, 
                               levels = weight_levels, 
                               ordered = TRUE)) %>% 
  ggplot() + 
  geom_jitter(aes(y = weight_class, 
                  x = weight)) + 
  geom_point(data = summary_stats, 
             mapping = aes(x = weight, 
                           y = weight_class),
             alpha = 0) + 
  geom_text(data = summary_stats, 
            mapping = aes(x = weight, 
                          y = weight_class,
                          label = sprintf("%.1f", weight_mean))) + 
  labs(x = "Weight in pounds")



# Pretty plot -------------------------------------------------------------

# Load fonts --------------------------------------------------------------

font_add_google("Oswald")
font_add_google("Nunito")
showtext_auto()
showtext_opts(dpi = 300)
title_font <- "Oswald"
body_font <- "Nunito"


# Define colours and fonts ------------------------------------------------

bg_col <- "#F2F4F8"
text_col <- "#151C28"
highlight_col <- "#7F055F"


# Plot --------------------------------------------------------------------


## Height
g <- ufc_athletes %>% 
  filter(!is.na(height) & !is.na(weight) & height >0 & weight >0 & !is.na(weight_class)) %>% 
  mutate(weight_class = factor(weight_class, 
                               levels = weight_levels, 
                               ordered = TRUE)) %>% 
  ggplot() + 
  geom_jitter(aes(y = weight_class, 
                  x = height),
              alpha = 0.5,
              width = 0.4,
              height = 0.3,
              size = 2,
              colour = highlight_col) + 
  
  scale_x_continuous(limits = c(55, 90)) + 
  
  geom_point(data = summary_stats, 
             mapping = aes(x = height, 
                           y = weight_class),
             alpha = 0) + 
  
  geom_text(data = summary_stats, 
            mapping = aes(x = height, 
                          y = weight_class,
                          label = sprintf("%.1f", height_mean)),
            size = 3,
            colour = text_col) + 
  
  labs(x = "Height in inches",
       y = "") + 
  
  coord_cartesian(clip = "off") + 
  
  theme_minimal(base_family = body_font, base_size = 11.5) + 
  
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.margin = margin(t=5,r=20,b=40,l=5),
    legend.position = "none",
    axis.title.x = element_text(vjust = -5)
  ) 

g

g_int <- ggdraw(g) +
  draw_grob(
    rectGrob(
      gp = gpar(fill = bg_col, col = NA)
    )
  ) +
  draw_plot(p) +
  draw_text(
    x = 0.6, y = 0.9,
    size =11,
    hjust = -0.2,
    colour = text_col,
    family = body_font,
    text = str_wrap("Mean height for each weight class", 22)
  ) +
  draw_grob(
    curveGrob(
      x1 = 0.72, y1 = 0.85,
      x2 = 0.82, y2 = 0.78,
      curvature = 0.5,
      gp = gpar(col = text_col, lwd = 1.5, fill = text_col),
      arrow = arrow(type = "closed", length = unit(0.07, "inches"))
    )
  )
g_int

ggsave(
  "2026/2026-07-07/20260707.png",
  g_int,
  width = 7,
  height = 7,
  units = "in",
  dpi = 300,
  bg = bg_col
)



## END

## Weight
ufc_athletes %>% 
  filter(!is.na(height) & !is.na(weight) & height >0 & weight >0 & !is.na(weight_class)) %>% 
  mutate(weight_class = factor(weight_class, 
                               levels = weight_levels, 
                               ordered = TRUE)) %>% 
  ggplot() + 
  geom_jitter(aes(y = weight_class, 
                  x = weight)) + 
  geom_point(data = summary_stats, 
             mapping = aes(x = weight, 
                           y = weight_class),
             alpha = 0) + 
  geom_text(data = summary_stats, 
            mapping = aes(x = weight, 
                          y = weight_class,
                          label = sprintf("%.1f", weight_mean))) + 
  labs(x = "Weight in pounds")

























