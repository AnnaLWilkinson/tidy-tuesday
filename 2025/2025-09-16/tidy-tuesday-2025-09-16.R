
# About this script -------------------------------------------------------

## Project: Tidy Tuesday
## Purpose: All Recipes
## Author: Anna Wilkinson; anna.wilkinson@burnet.edu.au
## Date: 16 September 2025




# Libraries ---------------------------------------------------------------
pacman::p_load(rio, 
               here, 
               tidyverse,
               janitor,
               lubridate,
               gtsummary,
               scales,
               GGally,
               nullabor,
               ggdist
)




# Import data -------------------------------------------------------------

# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2025-09-16')
## OR
#tuesdata <- tidytuesdayR::tt_load(2025, week = 37)

all_recipes <- tuesdata$all_recipes
cuisines <- tuesdata$cuisines

# Option 2: Read directly from GitHub

#all_recipes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-16/all_recipes.csv')
#cuisines <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-16/cuisines.csv')


# Summary -----------------------------------------------------------------
summary(all_recipes)



# Exploratory plots -------------------------------------------------------


all_recipes |> 
  select(calories, 
         fat, 
         carbs,
         protein) |> 
  GGally::ggpairs()




all_recipes |> 
  ggplot() + 
  geom_point(aes(x = calories,
                 y = total_time))


cuisines |> 
  ggplot() +
  geom_point(aes(x = total_time, 
                 y = calories,
                 colour = country))


# ? total is in minutes
cuisines |> 
  filter(total_time <=60) |>  
  ggplot() + 
  geom_point(aes(x = total_time, 
                y = calories,
                colour = country))


all_recipes |>
  filter(total_time <=60) |> 
  filter(calories <8000) |>  
  ggplot() + 
  geom_point(aes(x = total_time,
                 y = calories))


cuisines |> 
  filter(total_time <60) |> 
  summarise(n = n(), .by = country) |> 
  arrange(desc(n))

cuisines |>
  filter(total_time <=30) |> 
  filter(calories <8000) |>  
  ggplot() + 
  geom_point(aes(x = total_time,
                 y = calories,
                 colour = country))
  

cuisines |> 
  filter(total_time == 30) |> 
  ggplot() + 
  geom_point(aes(x = calories, 
             y = avg_rating,
             colour = author))



all_recipes |> 
  ggplot() + 
  geom_point(aes(x = total_time, 
                 y = avg_rating))


all_recipes |> 
  ggplot() + 
  geom_point(aes(x = total_time, 
                 y = avg_rating,
                 size = calories))


all_recipes |> 
  filter(total_time <30) |> 
  ggplot() + 
  geom_point(aes(x = total_time, 
                 y = avg_rating,
                 size = calories))


all_recipes |> 
  filter(total_time >0 & total_time <30) |> 
  filter(calories <1000) |> 
  ggplot() + 
  geom_jitter(aes(x = total_time, 
                 y = calories,
                 size = total_ratings,
                 colour = avg_rating),
              alpha = 0.3)




# Pretty plot -------------------------------------------------------------

length(unique(all_recipes$avg_rating))  ## 31

all_recipes |> 
  filter(total_time >0 & total_time <=60) |> 
  filter(calories <=1000) |> 
  drop_na(avg_rating) |> 
  ggplot() + 
  geom_jitter(aes(x = total_time, 
                  y = calories),
              alpha = 0.2,
              size = 3,
              width = 1, 
              colour = "maroon") + 
  scale_x_continuous(breaks = seq(0, 60, 10)) + 
  #scale_colour_gradient(low = "lightpink", high = "maroon") +
  
  labs(title    = "Allrecipes",
       subtitle = "Can I get high ratings and high calories quickly?",
       caption  = "Excludes recipes of total time of 0 and above 30 mins,
                   with calories >1000, and with no rating.",
       x = "Total time (mins)",
       y = "Calories"
       ) + 
  
  theme_bw() + 
  theme(
    aspect.ratio = .5
  )




# Exploratory -------------------------------------------------------------

all_recipes |> 
  summarise(n_cals = sum(calories), .by = author) |> 
  arrange(desc(n_cals))

all_recipes |> 
  select(name, author, calories) |> 
  arrange(desc(calories))


all_recipes |> 
  ggplot() +
  geom_point(aes(x = date_published,
                 y = calories))

all_recipes |> 
  ggplot() +
  geom_point(aes(x = date_published,
                 y = fat))


all_recipes |> 
  ggplot() +
  geom_point(aes(x = date_published,
                 y = calories),
             alpha = 0.3)




# Null plots --------------------------------------------------------------

d <- nullabor::lineup(null_permute("allrecipes"), all_recipes)

# Position of the actual data
attr(d, "pos")  ## 9


ggplot(data = d) +
  geom_point(aes(x = date_published,
                 y = calories),
             alpha = 0.3) + 
  facet_wrap(~ .sample)



# Distribution  -----------------------------------------------------------

all_recipes |> 
  ggplot() + 
  geom_dots(aes(x = calories))


cuisines |> 
  ggplot(aes(x = total_time, 
             y = country)) + 
  stat_interval()


cuisines |> 
  filter(total_time <60) |> 
  ggplot(aes(x = total_time, 
             y = country)) + 
  stat_interval()

cuisines |> 
  filter(total_time <60) |> 
  ggplot(aes(x = total_time, 
             y = country)) + 
  ggdist::stat_gradientinterval(
    geom = "slabinterval"
  )


cuisines |> 
  filter(total_time <60) |> 
  summarise(mean = mean(total_time, na.rm = TRUE), .by = country) |> 
  arrange(desc(mean))

country_order <-  cuisines |> 
  filter(total_time <60) |> 
  summarise(mean = mean(total_time, na.rm = TRUE), .by = country) |> 
  arrange(desc(mean)) |> 
  select(country) |> 
  unlist()
country_order

cuisines <- cuisines |> 
  mutate(country_fct = factor(country, 
                       levels = country_order))

levels(cuisines$country_fct)


# re-plot with country relevelled

cuisines |> 
  filter(total_time <60) |> 
  ggplot(aes(x = total_time,country_fct, 
             y = country_fct)) + 
  ggdist::stat_gradientinterval(
    point_interval = "mean_qi",
    geom = "slabinterval",
    position = "dodge", 
    fill_type = "gradient"
  )



cuisines |> 
  filter(total_time <60) |> 
  ggplot(aes(x = total_time,country_fct, 
             y = country_fct)) + 
  ggdist::stat_interval(alpha = 0.3,
                        colour = "lightblue", 
                        .width = c(0.2, 0.8)) + 
  ggdist::stat_pointinterval(point_interval = "mean_qi",
                             point_size = 2,
                             point_colour = "darkgrey", 
                             interval_colour = "lightgrey") +
  
  scale_x_continuous(limits = c(0,60),
                     breaks = seq(0, 60, 10)) +
  
  labs(title    = "All Recipes",
       subtitle = "Distribution of time by country. Recipes 60mins or less",
       x = "Total time (mins)",
       y = "") + 
  
  theme_classic() + 
  theme(
    
    panel.grid.major.x = element_line(colour = "lightgrey")
  )

ggsave(filename = here::here("figs", "all_recipes.jpeg"),
       plot = last_plot(),
       width = 8, 
       height = 10 )


cuisines |> 
  filter(total_time <60) |> 
  ggplot(aes(x = total_time,country_fct, 
             y = country_fct)) + 
  geom_boxplot() + 
  #geom_jitter() + 
  coord_flip()

## END


