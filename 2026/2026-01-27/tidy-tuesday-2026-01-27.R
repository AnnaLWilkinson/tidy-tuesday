
# About this script -------------------------------------------------------

#  Project: Tidy Tuesday
#  Purpose: Brazilian Companies
#  Author:  Anna Wilkinson; anna.wilkinson@burnet.edu.au
#  Date started: 27 jan 2026
#  Last update: 



# Libraries ---------------------------------------------------------------

library(rio)
library(here)
library(tidyverse)
library(janitor)
library(lubridate)
library(GGally)
library(viridis)
library(patchwork)
library(ggdist)

library(conflicted)
conflicted::conflicts_prefer(dplyr::select)
conflicted::conflicts_prefer(dplyr::filter)

# Import data  ------------------------------------------------------------

# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2026-01-27')
## OR
#tuesdata <- tidytuesdayR::tt_load(2026, week = 4)

companies <- tuesdata$companies
legal_nature <- tuesdata$legal_nature
qualifications <- tuesdata$qualifications
size <- tuesdata$size

# Option 2: Read directly from GitHub

# companies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-01-27/companies.csv')
# legal_nature <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-01-27/legal_nature.csv')
# qualifications <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-01-27/qualifications.csv')
# size <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-01-27/size.csv')
# 
# 

# Summary -----------------------------------------------------------------

summary(companies)

companies |> 
  distinct(company_name) |> 
  count()

companies |> 
  distinct(legal_nature) |> 
  count()

companies |> 
  distinct(owner_qualification) |> 
  count()

companies |> 
  distinct(company_size) |> 
  count()

summary(companies$capital_stock)

# Joining data ------------------------------------------------------------



# Exploratory plots -------------------------------------------------------

companies |> 
  ggplot() + 
  geom_histogram(aes(x = capital_stock))


companies |> 
  ggplot() + 
  geom_histogram(aes(x = log(capital_stock)))

companies |> 
  ggplot() + 
  geom_bar(aes(x = legal_nature),
           stat = "count") +
  coord_flip()

companies |> 
  ggplot() + 
  geom_bar(aes(x = owner_qualification),
           stat = "count") + 
  coord_flip()


companies |> 
  ggplot() + 
  geom_jitter(aes(x = owner_qualification, 
                  y = capital_stock))

companies |> 
  ggplot() + 
  geom_jitter(aes(x = owner_qualification, 
                  y = log(capital_stock)))


companies |> 
  ggplot() + 
  geom_jitter(aes(x = owner_qualification, 
                  y = log(capital_stock))) + 
  coord_flip()


companies |> 
  ggplot() + 
  geom_point(aes(x = owner_qualification, 
                 y = log(capital_stock)),
             shape = "-",
             size = 8) + 
  coord_flip()


companies |> 
  ggplot() + 
  geom_point(aes(x = owner_qualification, 
                 y = log(capital_stock)),
             shape = "*",
             size = 2) + 
  coord_flip()

companies |> 
  ggplot() + 
  geom_point(aes(x = owner_qualification, 
                 y = log(capital_stock)),
             shape = "+",
             size = 2) + 
  coord_flip()

companies |> 
  ggplot(aes(x = owner_qualification, 
             y = log(capital_stock))) + 
  geom_dotplot(binaxis = 'y',
               binwidth = 0.5, 
               stackdir = "center",
               dotsize = 0.2) +
  coord_flip() + 
  theme_minimal() 



companies |> 
  ggplot(aes(x = company_size, 
             y = log(capital_stock))) + 
  geom_dotplot(binaxis = 'y',
               binwidth = 0.01, 
               stackdir = "center",
               dotsize = 0.05) +
  coord_flip() + 
  theme_minimal() 



companies |> 
  ggplot(aes(x = company_size, 
             y = log(capital_stock))) + 
  geom_jitter()


# Pretty plot -------------------------------------------------------------

companies <- companies |> 
  mutate(company_size = stringr::str_to_sentence(company_size))

companies <- companies |> 
  mutate(company_size = fct_relevel(company_size, 
                                    "Micro-enterprise",
                                    "Small-enterprise",                                  "Other"))

companies |> 
  ggplot(aes(x = company_size, 
             y = log(capital_stock))) + 
  geom_jitter(alpha = 0.2,
              colour = "grey50") 

companies |> 
  ggplot(aes(x = company_size, 
             y = capital_stock)) + 
  geom_jitter(alpha = 0.2,
              colour = "grey50") +
  scale_y_log10(
    labels = scales::label_number(scale_cut = scales::cut_short_scale())
  )


my_plot <- 
  companies |> 
  ggplot(aes(x = company_size, 
             y = capital_stock)) + 
  geom_jitter(alpha = 0.2,
              colour = "grey50") +
  scale_y_log10(
    labels = scales::label_number(scale_cut = scales::cut_short_scale())
  )

  
my_plot <- 
  my_plot + 
  labs(y = "Capital stock (log)",
       x = "",
       title = "Brazilian companies size vs capital stock",
       caption = "Capital stock is\n'Declared share capital' (BRL)") + 
  theme_minimal()  

my_plot  



# Save plot ---------------------------------------------------------------

ggsave(filename = here::here("2026", "2026-01-27", "20260127.png"),
       plot = my_plot)

## END

