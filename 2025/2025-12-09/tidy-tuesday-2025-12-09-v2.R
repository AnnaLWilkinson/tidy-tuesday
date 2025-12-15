
# About this script -------------------------------------------------------

# Purpose:  
# Project:  
# Author:  
# Date started: 
# Last update date: 



# Libraries ---------------------------------------------------------------

pacman::p_load(rio, 
               here,
               tidyverse,
               janitor, 
               lubridate,
               fishualize)


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



# Summary  ----------------------------------------------------------------

View(qatarcars)
skimr::skim(qatarcars)


# Exploratory analysis ----------------------------------------------------

janitor::tabyl(qatarcars, origin)
janitor::tabyl(qatarcars, make)
janitor::tabyl(qatarcars, type)

# option 1 - start w d.f. and pipe to ggplot()
qatarcars |> 
  ggplot()

# option 2 - start w ggplot and put in argument data = d.f.
ggplot(data = qatarcars,
       mapping = aes(x = length)) +
  geom_histogram()


ggplot(data = qatarcars,
       mapping = aes(x = economy,
                     y = performance)) + 
  geom_point()


# add in a dynamic aesthetics - dot size by price

ggplot(data = qatarcars,
       mapping = aes(x    = economy, 
                     y    = performance,
                     size = price)) + 
  geom_point()


ggplot(data = qatarcars, 
       mapping = aes(x = type, 
                     y = economy)) + 
  geom_jitter()

ggplot(data = qatarcars, 
       mapping = aes(x = type, 
                     y = economy)) + 
  geom_jitter(width = 0.1)


ggplot(data = qatarcars, 
       mapping = aes(x = type, 
                     y = economy)) + 
  geom_boxplot() + 
  geom_jitter(width = 0.1) +
  theme_minimal()


# Pretty plot -------------------------------------------------------------





