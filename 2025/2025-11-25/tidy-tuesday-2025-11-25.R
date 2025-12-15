
# About this script -------------------------------------------------------

#  Purpose: Tidy Tuesday
#  Project: Statistical Performance Indicators
#  Author: Anna Wilkinson; anna.wilkinson@burnet.edu.au
#  Date: 24 Nov 2025
#  Last update: 


#  Libraries --------------------------------------------------------------

pacman::p_load(rio, 
               here, 
               tidyverse,
               janitor,
               lubridate,
               gtsummary,
               ggdist,
               fishualize,
               ggrepel
               )

# Import data -------------------------------------------------------------

# World Bank Statistical Performance Indicators
# https://github.com/rfordatascience/tidytuesday/blob/main/data/2025/2025-11-25/readme.md

# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2025-11-25')
## OR
#tuesdata <- tidytuesdayR::tt_load(2025, week = 47)

spi_indicators <- tuesdata$spi_indicators

# Option 2: Read directly from GitHub

#spi_indicators <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-11-25/spi_indicators.csv')



# Summary -----------------------------------------------------------------

summary(spi_indicators)

tabyl(spi_indicators$country)
tabyl(spi_indicators$region)
tabyl(spi_indicators$income)
tabyl(spi_indicators$year)


# EDA ---------------------------------------------------------------------

spi_indicators |> 
  filter(year >=2016) |> 
  ggplot() + 
  geom_line(aes(x = year, 
                y = overall_score,
                group = country)) + 
  facet_wrap(~ region)

spi_indicators |> 
  ggplot() + 
  geom_line(aes(x = year, 
                y = data_use_score,
                group = country)) + 
  facet_wrap(~ region)


spi_indicators |> 
  filter(year >=2016) |> 
  ggplot() + 
  geom_line(aes(x = year, 
                y = data_services_score,
                group = country)) + 
  facet_wrap(~ region)

spi_indicators |> 
  ggplot() + 
  geom_line(aes(x = year, 
                y = data_products_score,
                group = country)) + 
  facet_wrap(~ region)


spi_indicators |> 
  filter(year >=2016) |> 
  ggplot() + 
  geom_line(aes(x = year, 
                y = data_sources_score,
                group = country)) + 
  facet_wrap(~ region)


spi_indicators |> 
  filter(year >=2016) |> 
  ggplot() + 
  geom_line(aes(x = year, 
                y = data_infrastructure_score,
                group = country)) + 
  facet_wrap(~ region)



spi_indicators |> 
  filter(year >=2016) |> 
  ggplot() + 
  geom_jitter(aes(x = year, 
                 y = overall_score)) 



spi_indicators |> 
  filter(year >=2016) |> 
  ggplot() + 
  geom_dots(aes(x = year, 
                  y = overall_score),
            side = "both") 


spi_indicators |> 
  filter(year >=2016) |> 
  ggplot() + 
  geom_dots(aes(x = year, 
                y = overall_score),
            side = "left") 


spi_indicators |> 
  filter(year >=2016) |> 
  ggplot() + 
  geom_dots(aes(x = year, 
                y = overall_score),
            side = "right") 

spi_indicators |> 
  filter(year >=2016) |> 
  ggplot() + 
  geom_boxplot(aes(x = factor(year), 
                y = overall_score)) 


spi_indicators |> 
  filter(year >=2016) |> 
  ggplot() + 
  stat_slabinterval(aes(x = factor(year), 
                   y = overall_score))


spi_indicators |> 
  filter(year >=2016) |> 
  ggplot() + 
  stat_interval(aes(x = factor(year), 
                        y = overall_score)) +
  scale_colour_grey() +
  coord_flip()


spi_indicators |> 
  filter(year >=2016) |> 
  ggplot() + 
  stat_gradientinterval(aes(x = factor(year), 
                    y = overall_score)) +
  scale_colour_grey() 

top_ten_overall <-  spi_indicators |> 
  filter(year == 2023) |> 
  arrange(desc(overall_score)) |> 
  head(10)

low_ten_overall <- spi_indicators |> 
  filter(year == 2023) |> 
  arrange(overall_score) |> 
  head(10)

top_low_ten <-  bind_rows(top_ten_overall, low_ten_overall)

top_low_ten |> 
  ggplot(aes(x = reorder(country, overall_score), 
             y = overall_score)) + 
  geom_point() + 
  geom_segment(aes(xend = reorder(country, overall_score),  
                   yend = 0)) + 
  coord_flip()




top_ten_countries <- spi_indicators |> 
  summarise(mean_dataproducts = mean(data_products_score, na.rm = TRUE), .by = country) |> 
  arrange(desc(mean_dataproducts)) |> 
  head(10) |> 
  select(country) |> 
  unlist()

low_ten_countries <- spi_indicators |> 
  summarise(mean_dataproducts = mean(data_products_score, na.rm = TRUE), .by = country) |> 
  arrange(desc(mean_dataproducts)) |> 
  tail(10) |> 
  select(country) |> 
  unlist()


### Pretty smooth plot

spi_indicators |> 
  filter(country %in% top_ten_countries | country %in% low_ten_countries) |> 
  filter(year >=2015) |> 

  mutate(group = if_else(country %in% top_ten_countries, "Top ten", "Bottom ten"),
         group = fct_relevel(group, 
                             "Top ten",
                             "Bottom ten")) |> 
  
  ggplot(aes(x = factor(year),
             y = data_products_score    # note - only complete var for top ten and low ten countries
             )) + 
  geom_line(aes(group = country,
                colour = group)) +
  geom_smooth(aes(group = group,
                  colour = group),
              method = "lm", 
              formula = y ~ splines::bs(x, 6)) + 

  scale_y_continuous(limits = c(0,100)) + 
  fishualize::scale_colour_fish_d(option = "Coris_gaimard") + 
  
  labs(title = "Statistical Performance Indicators",
       subtitle = "Disparity in Data Products Score between \n the top ten vs the lowest ten countries",
       y = "Data products score",
       x = "", 
       colour = "") + 

  theme_minimal() + 
  theme(
    
    legend.position = "bottom"
  )


ggsave(filename = here::here("figs", "who_stat_perf_2.jpeg"), 
                             plot = last_plot())



# Pretty plot -------------------------------------------------------------
tabyl(spi_indicators$income)
spi_indicators <-  spi_indicators |> 
  mutate(income = fct_relevel(income, 
                              "High income",
                              "Upper middle income",
                              "Lower middle income",
                              "Low income",
                              "Not classified"))

spi_indicators |> 
  filter(year >=2016) |> 
  ggplot() + 
  geom_dots(aes(x = factor(year), 
                y = overall_score,
                fill = income,
                colour = income),
            side = "both",
            layout = "hex") +
  scale_y_continuous(limits = c(0, 100)) + 
  scale_fill_grey(start = 0.2, end = 0.9) + 
  scale_colour_grey(start = 0.2, end = 0.9) + 
  labs(title    = "World Bank Statistical Performance Indicators",
       subtitle = "Is there clustering of overall score by country income",
       y        = "Overall score",
       x = "",
       fill = "",
       colour = "") + 
  theme_grey() + 
  theme(
    aspect.ratio = 1
  )

ggsave(filename = here::here("figs", "world_bank_stats_perf.jpeg"),
       plot = last_plot())








## END











