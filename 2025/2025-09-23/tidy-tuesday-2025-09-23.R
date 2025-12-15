
# About this script -------------------------------------------------------

##  Purpose: Tidy Tuesday
##  Project: FIDE Chess Player Ratings
##  Author: Anna Wilkinson; anna.wilkinson@burnet.edu.au
##  Date: 23 September 2025
##  Date last changed:




# Libraries ---------------------------------------------------------------

pacman::p_load(rio, 
               here, 
               tidyverse,
               janitor,
               lubridate,
               gtsummary,
               scales, 
               patchwork
               )


# Import data -------------------------------------------------------------

# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2025-09-23')
## OR
#tuesdata <- tidytuesdayR::tt_load(2025, week = 38)

fide_ratings_august <- tuesdata$fide_ratings_august
fide_ratings_september <- tuesdata$fide_ratings_september

# Option 2: Read directly from GitHub

#fide_ratings_august <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-23/fide_ratings_august.csv')
#fide_ratings_september <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-23/fide_ratings_september.csv')



# Exploratory analysis ----------------------------------------------------

summary(fide_ratings_august)


# Exploratory plots -------------------------------------------------------

fide_ratings_august |> 
  ggplot() + 
  geom_boxplot(aes(x = sex,
               y = k))

fide_ratings_august |> 
  ggplot() + 
  geom_bar(aes(x = bday), 
           stat = "count")

fide_ratings_august |> 
  ggplot() + 
  geom_bar(aes(x = bday,
               fill = fed), 
           stat = "count")


fide_ratings_august |> 
  ggplot() + 
  geom_bar(aes(x = bday,
               fill = sex), 
           stat = "count")

fide_ratings_august |> 
  ggplot() + 
  geom_bar(aes(x = bday,
               fill = title), 
           stat = "count")




# Pretty plot -------------------------------------------------------------

# Make a plot ordering F then M
p1 <- fide_ratings_august |> 
  ggplot() + 
  geom_bar(aes(x = bday,
               fill = sex), 
           stat = "count",
           width = 1) + 
  
  labs(
       x     = "Year of birth of player",
       y     =  "Frequency (n)",
       fill  = "Sex of player") + 
  
  scale_y_continuous(limits = c(0, 10000),
                     expand = c(0,0),
                     labels = scales::label_number(scale_cut = scales::cut_short_scale())) + 
  
  scale_fill_manual(values = c("darkorange", "darkgreen"),
                    labels = c("Female", "Male")) + 
  
  theme_bw() 
  

# Make a plot using M then F
p2 <- fide_ratings_august |> 
  mutate(sex = forcats::fct_relevel(sex, 
                                    "M", 
                                    "F")) |> 
  ggplot() + 
  geom_bar(aes(x = bday,
               fill = sex), 
           stat = "count",
           width = 1) + 
  
  labs(
       x     = "Year of birth of player",
       y     =  "Frequency (n)",
       fill  = "Sex of player") + 
  
  scale_y_continuous(limits = c(0, 10000),
                     expand = c(0,0),
                     labels = scales::label_number(scale_cut = scales::cut_short_scale())) + 
  
  scale_fill_manual(values = c("darkorange", "darkgreen"),
                     labels = c("Male", "Female")) + 
   
  theme_bw() 


# Make the plot in grey scale
p3 <- fide_ratings_august |> 
  ggplot() + 
  geom_bar(aes(x = bday,
               fill = sex), 
           stat = "count",
           width = 1) + 
  
  labs(
    x     = "Year of birth of player",
    y     =  "Frequency (n)",
    fill  = "Sex of player") + 
  
  scale_y_continuous(limits = c(0, 10000),
                     expand = c(0,0),
                     labels = scales::label_number(scale_cut = scales::cut_short_scale())) + 
  
  scale_fill_grey(labels = c("Female", "Male")) +
  
  theme_bw() 



# Reverse the male and female in grey scale
p4 <- fide_ratings_august |> 
  
  mutate(sex = forcats::fct_relevel(sex, 
                                    "M", 
                                    "F")) |> 
  
  ggplot() + 
  geom_bar(aes(x = bday,
               fill = sex), 
           stat = "count",
           width = 1) + 
  
  labs(
    x     = "Year of birth of player",
    y     =  "Frequency (n)",
    fill  = "Sex of player") + 
  
  scale_y_continuous(limits = c(0, 10000),
                     expand = c(0,0),
                     labels = scales::label_number(scale_cut = scales::cut_short_scale())) + 
  
  scale_fill_grey(labels = c("Male", "Female")) +
  
  theme_bw() 


# Make the plot in pink and blue
p5 <- fide_ratings_august |> 
  ggplot() + 
  geom_bar(aes(x = bday,
               fill = sex), 
           stat = "count",
           width = 1) + 
  
  labs(
    x     = "Year of birth of player",
    y     =  "Frequency (n)",
    fill  = "Sex of player") + 
  
  scale_y_continuous(limits = c(0, 10000),
                     expand = c(0,0),
                     labels = scales::label_number(scale_cut = scales::cut_short_scale())) + 
  
  scale_fill_manual(values = c("pink", "blue"),
                    labels = c("Female", "Male")) + 
  
  theme_bw() 


# Make a plot using M then F
p6 <- fide_ratings_august |> 
  mutate(sex = forcats::fct_relevel(sex, 
                                    "M", 
                                    "F")) |> 
  ggplot() + 
  geom_bar(aes(x = bday,
               fill = sex), 
           stat = "count",
           width = 1) + 
  
  labs(
    x     = "Year of birth of player",
    y     =  "Frequency (n)",
    fill  = "Sex of player", 
    caption = "Gender associations with colour started around the 20th century: 
    https://en.wikipedia.org/wiki/Gendered_associations_of_pink_and_blue") + 
  
  scale_y_continuous(limits = c(0, 10000),
                     expand = c(0,0),
                     labels = scales::label_number(scale_cut = scales::cut_short_scale())) + 
  
  scale_fill_manual(values = c("blue", "pink"),
                    labels = c("Male", "Female")) + 
  
  theme_bw() 




# patchwork

p7 <- (p1 + p2) / (p3 + p4) / (p5 + p6) +
  patchwork::plot_layout(axes = "collect") + 
  patchwork::plot_annotation(title = "FIDE Chess Player Ratings",
                             subtitle = "Is there a best way to show sex difference?")
p7

ggsave(filename = here::here("figs", "fide_chessplayers.jpeg"),
       plot = p7)


# Make a pyramid plot w suffragette colours

suff_g1 <- "#18981a"
suff_p1 <- "#880990"
suff_g2 <- "#0fd430"
suff_p2 <- "#4a2051"

p8 <- fide_ratings_august |> 
  summarise(n_count = n(), .by = c(bday, sex)) |> 
  ggplot(aes(y = ifelse( test = sex == "M", yes = -n_count, no = n_count),
             x = bday,
             fill = sex)) + 
  geom_bar(stat = "identity",
           width = 0.9) + 
  scale_y_continuous(labels = abs,
                     limits = c(-10000, 3000),
                     expand = c(0,0)) + 
    labs(
    title = "Number of FIDE Chess Players by birth year by sex",
    x     = "Year of birth of player",
    y     =  "Frequency (n)",
    fill  = "Sex of player") + 
  
  scale_fill_manual(values = c(suff_p1, suff_g1),
                    labels = c("Female", "Male")) + 
  theme_bw() +
  theme(
    aspect.ratio = 1
  ) + 
  coord_flip()
p8 

ggsave(filename = here::here("figs", "fide_chessplayers_pyramid.jpeg"),
       plot = p8)




## END


