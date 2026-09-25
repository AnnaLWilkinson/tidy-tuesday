
# About this script -------------------------------------------------------

#  Project: Tidy Tuesday 
#  Purpose: Average share of green areas across cities
#  Author: Anna Wilkinson; anna.wilkinson@burnet.edu.au
#  Date: 23 Sep 2026


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(countrycode)
library(brolgar)
library(gghighlight)
library(showtext)
library(glue)
library(ggview)
library(ggtext)


# Load data ---------------------------------------------------------------

# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2026-09-22')
## OR
#tuesdata <- tidytuesdayR::tt_load(2026, week = 38)

urban <- tuesdata$urban

# Option 2: Read directly from GitHub

#urban <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-09-22/urban.csv')



# Load fonts --------------------------------------------------------------

font_add_google("Oswald")
font_add_google("Nunito")
showtext_auto()
showtext_opts(dpi = 300)
title_font <- "Oswald"
body_font <- "Nunito"


# Define colours and fonts-------------------------------------------------

bg_col <- "#F2F4F8"
text_col <- "#151C28"
highlight_col <- "#519623"


# Define text -------------------------------------------------------------

title <- glue('<span style="font-family:{title_font}; font-size:17pt;">**Losing green space**</span><br>Many cities have reduced the size of green spaces over time.')
st    <- "Green space per capita in meter squared. 1990 - 2025."
cap   <- glue("**Source**: UN Habitat Urban Indicators Database")



# Summary  ----------------------------------------------------------------

urban |> 
  count(cityName) |> 
  arrange(-n)

urban |> 
  distinct(cityName) |> 
  nrow()

urban |> 
  distinct(countryOrTerritoryName) |> 
  nrow()

urban |> 
  distinct(sdgSubRegion) |> 
  nrow()

urban |> 
  distinct(sdgRegion) |> 
  nrow()

urban |> 
  distinct(sdgRegion) 


# Data wrangling ----------------------------------------------------------


# Exploratory data analysis -----------------------------------------------

urban |> 
  ggplot() + 
  geom_density(aes(x = averageShareOfGreenAreaInCityUrbanAreaPct))


urban |> 
  ggplot() + 
  geom_density(aes(x = greenAreaPerCapitaM2))


urban |> 
  filter(sdgRegion == "Australia and New Zealand") |> 
  drop_na(cityName) |> 
  ggplot(aes(x = year,
             y = greenAreaPerCapitaM2,
             group = cityName, 
             colour = cityName)) + 
  geom_point() +
  geom_line()
  
urban |> 
  filter(sdgRegion == "Eastern Asia and South-eastern Asia") |> 
  drop_na(cityName) |> 
  ggplot(aes(x = year,
             y = greenAreaPerCapitaM2,
             group = cityName, 
             colour = cityName)) + 
  geom_point() +
  theme(
    
    legend.position = "none"
  )

## Brolgar

missing_2025 <- urban |> 
 filter(year == 2025 & is.na(greenAreaPerCapitaM2)) |> 
 select(cityCode) 
  
urban_allyrs <- urban |> 
  anti_join(missing_2025) 


urban_ts <- urban_allyrs |> 
  select(cityCode, 
         cityName,
         year, 
         greenAreaPerCapitaM2) |> 
  drop_na(cityCode) |> 
  as_tsibble(key = cityCode,
             index = year, 
             regular = TRUE)

urban_ts |> 
  brolgar::features(greenAreaPerCapitaM2, feat_monotonic) |> 
  left_join(urban_ts, by = "cityCode") |> 
  
  ggplot(aes(x = year, 
             y = greenAreaPerCapitaM2, 
             group = cityCode)) + 
  geom_line() + 
  gghighlight(increase)


urban_ts |> 
  brolgar::features(greenAreaPerCapitaM2, feat_monotonic) |> 
  left_join(urban_ts, by = "cityCode") |> 
  
  filter(decrease == TRUE) |> 
  
  ggplot(aes(x = year, 
             y = greenAreaPerCapitaM2, 
             group = cityCode)) + 
  geom_line() + 
  gghighlight(decrease,
              label_key = cityName)

urban_ts |> 
  brolgar::features(greenAreaPerCapitaM2, feat_monotonic) |> 
  left_join(urban_ts, by = "cityCode") |> 
  
  filter(decrease == TRUE) |> 
  
  ggplot(aes(x = year, 
             y = greenAreaPerCapitaM2, 
             group = cityName)) + 
  geom_line() +
  gghighlight() +
  facet_wrap(~cityName)



# Final plot --------------------------------------------------------------

## Dumbbell plot

urban_dec <- urban_ts |> 
  brolgar::features(greenAreaPerCapitaM2, feat_monotonic) |> 
  left_join(urban_ts, by = "cityCode") |> 
  filter(decrease == TRUE & (year == 1990 | year ==2025)) |> 
  select(cityCode, 
         cityName, 
         year,
         greenAreaPerCapitaM2) |> 
  mutate(year = as_factor(year), 
         year = fct_relevel(year, 
                            "1990", "2025")) 

urban_dec |> 
  group_by(cityCode) |> 
  arrange(cityCode, year) |> 
  mutate(diff = greenAreaPerCapitaM2 - greenAreaPerCapitaM2[1], 
         diff = replace(diff, row_number() == 1, diff[2])) |>
  ungroup() |> 

  ggplot(aes(x = greenAreaPerCapitaM2,
             y = reorder(cityName, -diff))) +
  
  geom_line(aes(group = cityName), 
            color = "#E7E7E7", 
            linewidth = 3.5) + 
  geom_point(aes(color = year), 
                 size = 5) +
  
  scale_colour_manual(values = c("darkgreen", "lightgreen")) + 
  scale_x_continuous(limits = c(0, 70),
                     breaks = seq(0,70,10)) +
  
  geom_text(data = urban_dec  |> 
              group_by(cityCode) |> 
              arrange(cityCode, year) |> 
              mutate(diff = greenAreaPerCapitaM2 - greenAreaPerCapitaM2[1], 
                     diff = replace(diff, row_number() == 1, diff[2])) |>
              ungroup() |>   
              filter(diff==min(diff)),
            aes(label = year, 
                colour = year), 
            nudge_y = .8,
            fontface = "bold") + 
              
  labs(x = "Green area per capita (m2)", 
       y = "",
       colour = "",
       title    = title,
       subtitle =   st,
       caption  = cap) + 
  
   theme_minimal(base_size = 12, base_family = body_font) +
  
   theme(
    legend.position = "none",
    plot.margin = margin(5,5,5,5),
    panel.grid = element_blank(),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5),
      family = body_font,
      size = rel(1)
    ), 
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5),
      family = body_font,
      size = rel(0.9)
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 10),
      family = body_font,
      size = rel(0.9)
  )
  ) + 
  coord_cartesian(ylim = c(25, 0.5)) + 
  canvas(
    width = 9, height = 9,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> p

p
# Save --------------------------------------------------------------------

save_ggplot(
  plot = p,
  file = file.path("2026", "2026-09-22", paste0("20260922", ".png"))
)
  
## END
