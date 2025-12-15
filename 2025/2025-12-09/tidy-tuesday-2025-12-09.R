
#  About this script ------------------------------------------------------

#  Project: Tidy Tuesday 
#  Purpose: Cars in Qatar
#  Author: Anna Wilkinson; anna.wilkinson@burnet.edu.au
#  Date started:  10 Dec 2025


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(rio)
library(here)
library(lubridate)
library(janitor)
library(scales)
library(GGally)
library(patchwork)
library(conflicted)
conflicted::conflicts_prefer(dplyr::filter)
conflicted::conflicts_prefer(dplyr::select)

# Import data -------------------------------------------------------------

# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2025-12-09')
## OR
#tuesdata <- tidytuesdayR::tt_load(2025, week = 49)

qatarcars <- tuesdata$qatarcars

# Option 2: Read directly from GitHub

#qatarcars <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-12-09/qatarcars.csv')


# Summary -----------------------------------------------------------------
summary(qatarcars)
tabyl(qatarcars, origin)
tabyl(qatarcars, make)
tabyl(qatarcars, model)
qatarcars |> 
  distinct(model) |> 
  count()


# Exploratory data analysis -----------------------------------------------

GGally::ggpairs(qatarcars, columns = 4:13)

qatarcars |> 
  ggplot(aes(x = price)) + 
  geom_histogram() + 
  scale_x_continuous(labels = label_number(scale_cut = cut_short_scale()))

qatarcars |> 
  ggplot(aes(x = log(price))) + 
  geom_histogram()


qatarcars |> 
  ggplot() + 
  geom_point(aes(x = horsepower,
                 y = price))

qatarcars |> 
  ggplot() + 
  geom_point(aes(x = horsepower,
                 y = log(price)))


qatarcars |> 
  ggplot() + 
  geom_point(aes(x = horsepower,
                 y = log10(price)))


qatarcars |> 
  ggplot() + 
  geom_point(aes(x = horsepower,
                 y = price)) + 
  scale_y_log10(labels = label_number(scale_cut = cut_short_scale()))
  
qatarcars |> 
  ggplot(aes(x = economy, 
             y = horsepower)) + 
  geom_point()

qatarcars |> 
  ggplot(aes(x = mass, 
             y = horsepower)) + 
  geom_point()

qatarcars |> 
  ggplot(aes(x = performance, 
             y = mass)) + 
  geom_point()

qatarcars |> 
  ggplot(aes(x = performance,   # performance is time 0 to 100
             y = horsepower)) + 
  geom_point()


## price and horsepower

# raw
qatarcars |> 
  ggplot() + 
  geom_point(aes(x = horsepower,
                 y = price))

# nl
qatarcars |> 
  ggplot() + 
  geom_point(aes(x = horsepower,
                 y = log(price)))  


# log base 10
qatarcars |> 
  ggplot() + 
  geom_point(aes(x = horsepower,
                 y = log10(price)))  

qatarcars |> 
  ggplot() + 
  geom_point(aes(x = log10(horsepower),
                 y = log10(price)))  


# log base ten changing the y scale
qatarcars |> 
  ggplot() + 
  geom_point(aes(x = horsepower,
                 y = price)) + 
  scale_y_log10(labels = label_number(scale_cut = cut_short_scale()))




# Pretty plot -------------------------------------------------------------

# calculate mid points for the x-axis and y-axis
# x_mid <-  mean(c(max(qatarcars$horsepower, na.rm = TRUE), min(qatarcars$horsepower, na.rm = TRUE)))
# x_mid

# arthimetic mean
# y_mid <- mean(c(max(qatarcars$price, na.rm = TRUE), min(qatarcars$price, na.rm = TRUE)))
# y_mid

# geometric mean
# multiply min and max and take the square root
y_max <- max(log10(qatarcars$price), na.rm = TRUE)
y_min <- min(log10(qatarcars$price), na.rm = TRUE)

y_mid <- sqrt(y_max * y_min)
y_mid
10^y_mid

x_max <- max(log10(qatarcars$horsepower), na.rm = TRUE)
x_min <- min(log10(qatarcars$horsepower), na.rm = TRUE)

x_mid <- sqrt(x_max * x_min)
x_max
x_min
x_mid
10^x_min
10^x_max
10^x_mid



# create a quadrant column
qatarcars <-  qatarcars |> 
  mutate(quadrant = case_when(
    
    log10(horsepower) >x_mid   & log10(price) >y_mid    ~ 'High price High horsepower', 
    log10(horsepower) <=x_mid  & log10(price) >y_mid    ~ "High price Low horsepower",
    log10(horsepower) <=x_mid  & log10(price) <=y_mid   ~ "Low price Low horesepower",
    TRUE ~ 'Low price High horsepower'
  ))


tabyl(qatarcars, quadrant)

# quadrants right but labels not nice
qatarcars |> 
  ggplot(aes(x = horsepower,
             y = log10(price))) + 
  geom_vline(xintercept = x_mid) + 
  geom_hline(yintercept = y_mid) + 
  geom_point() 


## nice labels but quadrants wrong
qatarcars |> 
  ggplot(aes(x = horsepower,
             y = price)) + 
  geom_vline(xintercept = x_mid) + 
  geom_hline(yintercept = y_mid) + 
  geom_point() + 
  scale_y_log10(labels = label_number(scale_cut = cut_short_scale()))

qatarcars |> 
  ggplot(aes(x = horsepower,
             y = price)) + 
  # geom_vline(xintercept = x_mid) + 
  # geom_hline(yintercept = y_mid) + 
  geom_point() + 
  scale_y_log10(labels = label_number(scale_cut = cut_short_scale())) + 
  theme(
    
    aspect.ratio = 1
  )

# transform y_mid
qatarcars |> 
  ggplot(aes(x = horsepower,
             y = price)) + 
  geom_vline(xintercept = x_mid) + 
  geom_hline(yintercept = (10^y_mid)) + 
  geom_point() + 
  scale_y_log10(labels = label_number(scale_cut = cut_short_scale()))


# transform horsepower
# transform y_mid
p1 <- qatarcars |> 
  
  ggplot(aes(x = horsepower,
             y = price,
             shape = quadrant,
             fill = quadrant,
             colour = quadrant)) + 
  geom_rect(aes(xmin = 10^x_mid, 
                xmax = 10^x_max, 
                ymin = 0, 
                ymax = 10^y_mid), 
            fill = "lightgrey", 
            alpha = 0.2) + 
  
  geom_vline(xintercept = (10^x_mid),
             colour = "grey60") + 
  geom_hline(yintercept = (10^y_mid),
             colour = "grey60") + 
  geom_point(size = 2) + 
  scale_colour_manual(values = c("grey60", "navy", "grey60")) + 
  scale_y_log10(labels = label_number(scale_cut = cut_short_scale())) +
  scale_x_log10(labels = label_number(scale_cut = cut_short_scale())) + 
  labs(y = "Price in 2025 Qatari riyals",
       x = "Horsepower",
       title = "Qatar Car Data",
       subtitle = "What should I buy when I win powerball",
       colour = "") +
  theme_minimal() +
  theme(
    
    aspect.ratio = 1,
    legend.position = "none"
  )

p2 <- qatarcars |> 
  filter(quadrant =='Low price High horsepower' ) |> 
  arrange(price) |>
  select(price, horsepower, make, model) |> 
  
  # create make-model col
  mutate(make_model = paste0(make,"-",model)) |> 
  
  head(n=10) |> 
  
  distinct(make_model, .keep_all = TRUE) |> 
  
  ggplot(aes(x = reorder(make_model, price),
             y = price)) + 
  geom_bar(stat = "identity",
           fill = "navy") +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) + 
  labs(x = "",
       y = "Price in Qatari riyals",
       title = "Which is the best value among the low(er) price high horsepower cars?") + 
  theme_minimal() +
  coord_flip()

p3 <- qatarcars |> 
  filter(quadrant =='Low price High horsepower' ) |> 
  arrange(desc(horsepower)) |>
  select(horsepower, make, model) |> 
  
  # create make-model col
  mutate(make_model = paste0(make,"-",model)) |> 
  
  head(n=10) |> 
  distinct(make_model, .keep_all = TRUE) |> 
  
  ggplot(aes(x = reorder( make_model,-horsepower),
             y = horsepower)) + 
  geom_bar(stat = "identity",
           fill = "navy") +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) + 
  labs(x = "", 
       y = "Horsepower") + 
  theme_minimal() + 
  coord_flip()


(p1 + plot_spacer())/ 
  (p2 | p3) +
  patchwork::plot_layout(nrow = 2)

p1 / 
  (p2 | p3) +
  patchwork::plot_layout(widths = c(2,1,1))


ggsave(filename = here::here("2025", "2025-12-09", "qatar_cars.jpeg"), 
       plot = last_plot())


# horsepower/mass by price

qatarcars <-  qatarcars |> 
  mutate(power_to_weight = round(horsepower/mass, digits = 3), 
         make_model = paste0(make,"-",model)) 

summary(qatarcars$power_to_weight)


qatarcars |>  
  arrange(desc(power_to_weight)) |> 
  head(n = 10)

qatarcars |> 
  arrange(desc(power_to_weight)) |> 
  head(n = 20) |> 
  ggplot(aes(y = power_to_weight, 
             x = reorder(make_model, price))) + 
  geom_point() +
  geom_segment(aes(y =0 , 
               yend = power_to_weight,
               x = make_model, 
               xend = make_model) ) + 
  labs(x = "Make and model, ordered by price",
       y = "Power to mass ratio",
       title = "Qatar Car Data", 
       subtitle = "What car gives me best power for buck?") + 
  theme_minimal() + 
  coord_flip() 
  

## END

