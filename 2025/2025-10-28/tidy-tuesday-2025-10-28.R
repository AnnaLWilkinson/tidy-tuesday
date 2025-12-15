
# About this script -------------------------------------------------------

# Project: Tidy Tuesday 
# Purpose: Selected British Literary Prizes
# Author: Anna Wilkinson; anna.wilkinson@burnet.edu.au
# Date started: 5 Nov 2025

# https://github.com/rfordatascience/tidytuesday/blob/main/data/2025/2025-10-28/readme.md



# Libraries ---------------------------------------------------------------

pacman::p_load(rio, 
               here, 
               tidyverse,
               janitor,
               lubridate,
               gtsummary,
               ggstream,
               khroma,
               randomcoloR,
               ggrepel
               )

devtools::install_github("hrbrmstr/streamgraph")

# Import data -------------------------------------------------------------

# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2025-10-28')
## OR
#tuesdata <- tidytuesdayR::tt_load(2025, week = 43)

prizes <- tuesdata$prizes

# Option 2: Read directly from GitHub

#prizes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-10-28/prizes.csv')


# Summary -----------------------------------------------------------------

summary(prizes)



# Exploratory analysis ----------------------------------------------------

tabyl(prizes, prize_name )



prizes |> 
  ggplot() + 
  geom_bar(aes(x = ethnicity_macro))

prizes |> 
  ggplot() + 
  geom_bar(aes(x = ethnicity))

prizes |> 
  ggplot() + 
  geom_bar(aes(x = gender))

prizes |> 
  ggplot() + 
  geom_bar(aes(x = prize_year,
               fill = gender))


prizes |> 
  ggplot() + 
  geom_bar(aes(x = prize_year,
               fill = ethnicity_macro))

# streamgraph 

stream_p <-  prizes |> 
  summarise(n = n(), .by = c(prize_year,ethnicity_macro)) 


streamgraph::streamgraph(stream_p, key = "ethnicity_macro",
                           value = "n", 
                           date = "prize_year") + 
  labs(y = "People shortlisted (n)")


## ggstream


stream_p |> 
  ggplot(aes(x = prize_year,
             y = n, 
             fill = ethnicity_macro)) + 
  geom_stream(geom = "contour",
              colour = "white") + 
  geom_stream(geom = "polygon") + 
  theme_minimal()


stream_p |> 
  ggplot(aes(x = prize_year,
             y = n, 
             fill = ethnicity_macro)) + 

  geom_stream(
   geom = "contour",
    color = "white",
    size = 1.25,
    bw = .5 # Controls smoothness  - bandwidth of kernel density estimation
  ) +
  geom_stream(
    geom = "polygon",
    bw = .5,
    size = 0
  ) 


stream_p |> 
  ggplot(aes(x = prize_year,
             y = n, 
             fill = ethnicity_macro)) + 
  
  geom_stream(
    geom = "polygon",
    type = "proportional")


stream_p |> 
  ggplot(aes(x = prize_year,
             y = n, 
             fill = ethnicity_macro)) + 
  
  geom_stream(
    geom = "polygon",
    type = "mirror")


stream_p |> 
  ggplot(aes(x = prize_year,
             y = n, 
             fill = ethnicity_macro)) + 
  
  geom_stream(
    geom = "polygon",
    type = "ridge")



stream_p |> 
  ggplot(aes(x = prize_year,
             y = n, 
             fill = ethnicity_macro)) + 
  
  geom_stream(
    geom = "polygon",
    type = "ridge") + 
  scale_fill_bright()

set.seed(2025-11-07)
randomcoloR::distinctColorPalette(k = 9)

stream_p |> 
  ggplot(aes(x = prize_year,
             y = n, 
             fill = ethnicity_macro)) + 
  
  geom_stream(
    geom = "polygon",
    type = "ridge") + 
  scale_fill_manual(values = randomcoloR::distinctColorPalette(k = 9))



stream_p |> 
  ggplot(aes(x = prize_year,
             y = n, 
             fill = ethnicity_macro)) + 
  
  geom_stream(
    geom = "polygon",
    type = "mirror") + 
  scale_fill_manual(values = randomcoloR::distinctColorPalette(k = 9))


stream_p |> 
  ggplot(aes(x = prize_year,
             y = n, 
             fill = ethnicity_macro)) + 
  
  geom_stream(
    geom = "polygon",
    type = "proportional") + 
  scale_fill_manual(values = randomcoloR::distinctColorPalette(k = 9))



# Pretty plot -------------------------------------------------------------

set.seed(2025-11-07)

range(stream_p$prize_year)
unique(stream_p$ethnicity_macro)  # 9
tabyl(stream_p, ethnicity_macro, prize_year)

stream_p |> 
  filter(prize_year == 2022)

stream_p |> 
  ggplot(aes(x = prize_year,
             y = n, 
             fill = ethnicity_macro,
             label = ethnicity_macro)) + 
  
  geom_stream(
    geom = "polygon",
    type = "ridge") + 
  geom_stream_label(
    geom = "text",
    type = "ridge") + 

  scale_fill_manual(values = randomcoloR::distinctColorPalette(k = 9)) + 
  theme_minimal()



stream_p |> 
  ggplot(aes(x = prize_year,
             y = n, 
             fill = ethnicity_macro,
             label = ethnicity_macro)) + 
  
  geom_stream(
    geom = "polygon",
    type = "ridge") + 
  geom_text_repel(data = subset(stream_p, prize_year == 2021), 
                  aes(y = n, 
                      x = Inf)) +  
  
  scale_fill_manual(values = randomcoloR::distinctColorPalette(k = 9)) + 
  theme_minimal()




## Pretty plot proportional

prizes |> 
  filter(prize_name == "Women's Prize for Fiction") |> 
  arrange(-desc(prize_year)) |> 
  head()

prizes |> 
  summarise(min_year = min(prize_year), .by = prize_name) |> 
  arrange(min_year) |> 
  print(n = 29)

prizes |> 
  filter(person_role == 'winner') |> 
  summarise(min_year = min(prize_year), .by = c(gender,prize_name)) |> 
  arrange(min_year) 

prizes |> 
  filter(person_role == 'winner') |> 
  filter(gender == "woman") |> 
  summarise(min_year = min(prize_year), .by = c(prize_name)) |> 
  arrange(min_year) 

prizes |> 
  filter(person_role == 'winner') |> 
  filter(gender == "woman") |> 
  filter(str_detect(prize_name, "Booker")) |> 
  summarise(min_year = min(prize_year), .by = c(prize_name)) |> 
  arrange(min_year) 


stream_p |> 
  filter(prize_year == 2022) |> 
  arrange(desc(ethnicity_macro)) |> 
  mutate(prop = n/sum(n), 
         cum_sum = cumsum(n),
         cum_prop = cum_sum/sum(n)) 

stream_p |> 
  filter(prize_year == 2021) |> 
  arrange(desc(ethnicity_macro)) |> 
  mutate(prop = n/sum(n), 
         cum_sum = cumsum(n),
         cum_prop = cum_sum/sum(n)) 

stream_p |> 
  filter(prize_year == 2020) |> 
  arrange(desc(ethnicity_macro)) |> 
  mutate(prop = n/sum(n), 
         cum_sum = cumsum(n),
         cum_prop = cum_sum/sum(n)) 


stream_p |> 
  filter(prize_year == 2019) |> 
  arrange(desc(ethnicity_macro)) |> 
  mutate(prop = n/sum(n), 
         cum_sum = cumsum(n),
         cum_prop = cum_sum/sum(n)) 


stream_p |> 
  filter(prize_year == 1996) |> 
  arrange(desc(ethnicity_macro)) |> 
  mutate(prop = n/sum(n), 
         cum_sum = cumsum(n),
         cum_prop = cum_sum/sum(n)) 




set.seed(2025-11-07)
stream_p |> 
  ggplot(aes(x = prize_year,
             y = n, 
             fill = ethnicity_macro,
             group = ethnicity_macro)) + 
  
  geom_stream(
    geom = "polygon",
    type = "proportional") + 
  geom_stream_label(aes(label = ethnicity_macro),
                    type = "proportional",
                    hjust = 1,
                    vjust = 0,
                    colour = "white",
                    size = 4,
                    position = "identity",
                    sorting = "none") + 
  scale_fill_manual(values = randomcoloR::distinctColorPalette(k = 9)) + 
  scale_x_continuous(breaks = seq(1991, 2022, 5)) + 
  labs(
    y = "", 
    x = "",
    title = "Ethnic diversity over time in British Literary Prizes"
  ) + 
  theme_minimal() + 
  theme(
    
    legend.position = "none",
    plot.background = element_rect(fill = "grey88"),
   # panel.grid = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(vjust = 6),
   # axis.ticks.x = element_line(colour = "black"),
    plot.title = element_text(
     size = 15,
     face = "bold",
     hjust = .5,
     margin = margin(10, 0, 10, 0)
   )
  )


# try to make label on far right side y axis

ethnic_labels <- stream_p |> 
  filter(prize_year == 2022) |> 
  arrange(desc(ethnicity_macro)) |> 
  mutate(prop = n/sum(n), 
         cum_sum = cumsum(n),
         cum_prop = cum_sum/sum(n)) |> 
  select(prize_year, ethnicity_macro, cum_prop)

# fudge the position a little
ethnic_labels <- ethnic_labels |> 
  mutate(cum_prop = if_else(ethnicity_macro == "White British", cum_prop - 0.1, cum_prop),
         cum_prop = if_else(ethnicity_macro == "Non-UK White", cum_prop - 0.1, cum_prop),
         cum_prop = if_else(ethnicity_macro == "Jewish", cum_prop - 0.01, cum_prop),
         cum_prop = if_else(ethnicity_macro == "Caribbean", cum_prop + 0.01, cum_prop),
         cum_prop = if_else(ethnicity_macro == "Asian", cum_prop - 0.05, cum_prop),
         cum_prop = if_else(ethnicity_macro == "African", cum_prop - 0.02, cum_prop),
         )

# add in Black British - just above Jewish cumul prop
black_british <-  data.frame(
    prize_year = 2022,
    ethnicity_macro = "Black British",
    cum_prop = NA_real_)

ethnic_labels <-  bind_rows(ethnic_labels, black_british)

ethnic_labels$cum_prop[ethnic_labels$ethnicity_macro == "Jewish"]

ethnic_labels <- ethnic_labels |> 
  arrange(desc(ethnicity_macro)) |> 
  mutate(cum_prop = if_else(ethnicity_macro == "Black British", ethnic_labels$cum_prop[ethnic_labels$ethnicity_macro == "Jewish"] + 0.05, cum_prop))


set.seed(2025-11-07)
stream_p |> 
  ggplot(aes(x = prize_year,
             y = n, 
             fill = ethnicity_macro,
             group = ethnicity_macro,
             label = ethnicity_macro)) + 
  
  geom_stream(
    geom = "polygon",
    type = "proportional") + 
  # geom_point(data = ethnic_labels, 
  #            aes(x = prize_year, 
  #                y = cum_prop
  #                )) + 
  geom_text_repel(data = ethnic_labels, 
                  aes(x = prize_year, 
                      y = cum_prop),
                  nudge_x = 6, 
                  segment.curvature = -1,
                  segment.ncp = 3, 
                  segment.angle = 20,
                  segment.linetype = "dashed",
                  hjust = 1) + 

  scale_fill_manual(values = randomcoloR::distinctColorPalette(k = 9)) + 
  scale_x_continuous(breaks = seq(1991, 2022, 5)) + 
  labs(
    y = "", 
    x = "",
    title = "Ethnic diversity over time in British Literary Prizes"
  ) + 
  theme_minimal() + 
  theme(
    
    legend.position = "none",
    plot.background = element_rect(fill = "grey90"),
    # panel.grid = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(vjust = 6),
    # axis.ticks.x = element_line(colour = "black"),
    plot.title = element_text(
      size = 15,
      face = "bold",
      hjust = .5,
      margin = margin(10, 0, 10, 0)
    )
  )

ggsave(filename = here::here("figs", "british_lit_prizes.jpeg"),
       plot = last_plot())



## END

