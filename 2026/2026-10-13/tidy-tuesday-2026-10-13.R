
# About this script -------------------------------------------------------

# Purpose: Tidy Tuesday
# Project: AmphibiaWeb
# Author: Anna Wilkinson; anna.wilkinson
# Date started: 14 October 2026



# Load libraries ----------------------------------------------------------

library(tidyverse)
library(countrycode)
library(igraph)


# Load data ---------------------------------------------------------------

# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2026-10-13')
## OR
#tuesdata <- tidytuesdayR::tt_load(2026, week = 41)

frogs_and_toads_amphibiaweb <- tuesdata$frogs_and_toads_amphibiaweb

# Option 2: Read directly from GitHub

#frogs_and_toads_amphibiaweb <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-10-13/frogs_and_toads_amphibiaweb.csv')



# Summary -----------------------------------------------------------------

frogs_and_toads_amphibiaweb |> 
  count(national_status)

frogs_and_toads_amphibiaweb |> 
  count(interntnl_status)

frogs_and_toads_amphibiaweb |> 
  count(family)

frogs_and_toads_amphibiaweb |> 
  count(genus)

frogs_and_toads_amphibiaweb |> 
  count(ordr)

frogs_and_toads_amphibiaweb |> 
  count(specificepithet)

frogs_and_toads_amphibiaweb |> 
  count(subfamily)



# Process -----------------------------------------------------------------

country_names <-  frogs_and_toads_amphibiaweb |> 
                    select(amphib_id, isocc) |> 
                    separate_longer_delim(isocc, 
                                          delim = ",")
  
  
  
  
country_names <-  country_names |> 
  mutate(country_name =  countrycode::countrycode(isocc, 
                             origin = "iso2c",
                             destination = "country.name")
  )


country_names |> 
  count(country_name) |> 
  arrange(-n) |> 
  head(10)


## Species introduced intro countries
country_names_intro <-  frogs_and_toads_amphibiaweb |> 
  select(amphib_id, intro_isocc) |> 
  separate_longer_delim(intro_isocc, 
                        delim = ",")


country_names_intro <-  country_names_intro |> 
  mutate(country_name_intro =  countrycode::countrycode(intro_isocc, 
                                                  origin = "iso2c",
                                                  destination = "country.name")
  )

country_names_intro |> 
  count(country_name_intro) |> 
  arrange(-n) |> 
  head(10)

## Join
introduced_countries <- country_names_intro |> 
  filter(!is.na(country_name_intro)) |> 
  arrange(amphib_id)

travelling_frogs <-  frogs_and_toads_amphibiaweb |> 
  select(amphib_id, common_name) |> 
  right_join(introduced_countries, by = "amphib_id") |> 
  arrange(amphib_id)


travelling_frogs |> 
  count(common_name) |> 
  arrange(-n)

network_frogs <-  travelling_frogs |> 
  filter(amphib_id ==229) |> 
  rename(source = common_name,
         target = country_name_intro) |> 
  select(source, target)

network <- igraph::graph_from_data_frame(d = network_frogs, directed = F)
dim(network)
igraph_options(plot.layout = layout.sphere, main = "sphere")
plot(network,
     vertex.color = "lightgrey",
     vertex.frame.color = "darkgrey",
     vertex.shape = c("circle"),
     vertex.size = c(10:25),
     vertex.label.dist = 2,
     vertex.label.degree = c(0, pi/8),
     vertex.label.cex = c(1),
     vertex.label.color = "black")










