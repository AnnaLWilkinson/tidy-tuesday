
# About this script -------------------------------------------------------

# Tidy Tuesday
# Anna Wilknson
# 2025-10-14
# World Food Day

# https://github.com/rfordatascience/tidytuesday/blob/main/data/2025/2025-10-14/readme.md

# Libraries ---------------------------------------------------------------

pacman::p_load(rio, 
               here, 
               tidyverse,
               janitor,
               lubridate,
               gtsummary,
               tsibble,
               brolgar
               )




# Import data -------------------------------------------------------------

# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2025-10-14')
## OR
#tuesdata <- tidytuesdayR::tt_load(2025, week = 41)

food_security <- tuesdata$food_security

# Option 2: Read directly from GitHub

#food_security <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-10-14/food_security.csv')




# Summary -----------------------------------------------------------------

food_security <-  food_security |> 
  clean_names()

summary(food_security)
tabyl(food_security, item )
unique(food_security$item)
tabyl(food_security$unit)

food_security |> 
  summarise(n = n(), .by = item) |> 
  arrange(desc(n))

food_security |> 
  summarise(n = n(), .by = c(item, area)) |> 
  arrange(desc(n))

tabyl(food_security$area)

# Exploratory data analysis -----------------------------------------------

food_security |> 
  filter(item == "Prevalence of undernourishment (percent) (annual value)") |> 
  ggplot() +
  geom_line(aes(x      = year_start,
                y      = value,
                group  =  area)) + 

  facet_wrap(~ area)




# Explore high and low income ---------------------------------------------

food_security_sub <-  food_security |> 
  filter(area == "High-income economies" | area == "Low-income economies" )

food_security_sub |> 
  filter(unit == "%") |> 
  ggplot() + 
  geom_line(aes(x = year_start, 
                y = value, 
                colour = area)) + 
  facet_wrap(~ item)


food_security_sub |> 
  filter(unit == "%") |> 
  ggplot() + 
  geom_bar(aes(x = year_start, 
                y = value, 
                fill = area),
           stat = "identity",
           position = "dodge") + 
  facet_wrap(~ item)
 
   

# Stunting ----------------------------------------------------------------

stunting <-  food_security |> 
  filter(item == "Percentage of children under 5 years of age who are stunted (modelled estimates) (percent)")    
stunting  

stunting |> 
  ggplot() + 
  geom_line(aes(x = year_start, 
                y = value,
                colour = area)) + 
  theme(
    legend.position = "none"
  )

ts_stunting <- as_tsibble(x = stunting, 
                         key = area,
                         index = year_start, 
                         regular = FALSE)
class(ts_stunting)


set.seed(2025-10-15-12-55)
ts_stunting %>%
  sample_n_keys(size = 5) %>%
  ggplot(aes(x = year_start,
             y = value,
             group = area)) + 
  geom_line()


# features
ts_stunting |> 
  features(value, feat_monotonic) |> 
  left_join(ts_stunting, by = "area") |> 
  ggplot(aes(x = year_start, 
             y = value, 
             group = area)) + 
  geom_line() + 
  gghighlight::gghighlight(increase)


ts_stunting |> 
  features(value, feat_monotonic) |> 
  filter(increase)


ts_stunting |> 
  features(value, feat_monotonic) |> 
  filter(decrease)


ts_stunting |> 
  features(value, feat_monotonic) |> 
  left_join(ts_stunting, by = "area") |> 
  ggplot(aes(x = year_start, 
             y = value, 
             group = area)) + 
  geom_line() + 
  gghighlight::gghighlight(decrease)


ts_stunting |> 
  features(value, feat_monotonic) |> 
  filter(decrease) |> 
  left_join(ts_stunting, by = "area") |> 
  ggplot(aes(x = year_start, 
             y = value, 
             group = area)) + 
  geom_line() + 
  gghighlight::gghighlight(area ==  "Democratic People's Republic of Korea")



ts_stunting |> 
  features(value, feat_monotonic) |> 
  left_join(ts_stunting, by = "area") |> 
  ggplot(aes(x = year_start, 
             y = value, 
             group = area)) + 
  geom_line() + 
  gghighlight::gghighlight(monotonic)


ts_stunting |> 
  features(value, feat_monotonic) |> 
  left_join(ts_stunting, by = "area") |> 
  ggplot(aes(x = year_start, 
             y = value, 
             group = area)) + 
  geom_line() + 
  gghighlight::gghighlight(unvary)


ts_stunting |> 
  features(value, feat_five_num) |> 
  mutate(diff = max - min) |> 
  arrange(desc(diff))

ts_stunting |> 
  filter(area ==  "Democratic People's Republic of Korea")




# Pretty plot -------------------------------------------------------------
range(ts_stunting$year_start)

ts_stunting |> 
  features(value, feat_monotonic) |> 
  filter(decrease) |> 
  
  left_join(ts_stunting, by = "area") |> 
  
  ggplot(aes(x = factor(year_start), 
             y = value, 
             group = area,
             colour = area)) + 
  geom_line() + 
  
  gghighlight::gghighlight(area ==  "Democratic People's Republic of Korea",
                           label_params = list(size = 4),
                           keep_scales = FALSE,
                           unhighlighted_params = list(linewidth = 0.3,
                                                       colour = alpha("grey50", 0.7),
                                                       linetype = "solid")) +
  scale_colour_manual(values = "darkblue") + 

  scale_y_continuous(limits = c(0,50)) + 
  scale_x_discrete(breaks = c(2005, 2007, 2009, 2011, 2013, 
                              2015, 2017, 2019, 2021, 2023, 2025)) + 
  labs(y = "Percent (%)",
       x = "",
       title = "Children under 5 stunted") + 
  
  theme_minimal()


ggsave(filename = here::here("figs", "foodsecurity.jpeg"),
       plot = last_plot())






