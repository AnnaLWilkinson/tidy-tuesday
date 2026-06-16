
# About this script -------------------------------------------------------

#  Purpose: Tidy Tuesday
#  Project: Sustainable Energy for All
#  Author: Anna Wilkinson; anna.wilkinson@burnet.edu.au
#  Date started: 26 May 2026


# Load libraries ----------------------------------------------------------

library(rio)
library(here)
library(janitor)
library(lubridate)
library(tidyverse)

# Load data ---------------------------------------------------------------

# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2026-05-26')
## OR
#tuesdata <- tidytuesdayR::tt_load(2026, week = 21)

energy_cleaned <- tuesdata$energy_cleaned

# Option 2: Read directly from GitHub

#energy_cleaned <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-05-26/energy_cleaned.csv')



# Summary -----------------------------------------------------------------

summary(energy_cleaned)




# Exploratory data analysis -----------------------------------------------

energy_cleaned %>% 
  ggplot() +
  geom_jitter(aes( x= yr,
                 y = divisia_decomp_analysis_structure_component_index))


target_countries <-  c("China", "India")   ## large changes in economy

energy_cleaned %>% 
  filter(country_name %in% target_countries) %>% 
  select(
    country_name, 
    yr,
    perc_renewable_of_total_electricity_output,
    total_final_energy_consumption_tfec
  ) %>% 
  
  ggplot(aes(x = yr, 
                 y = perc_renewable_of_total_electricity_output,
                 colour = country_name)) + 
  geom_point() + 
  geom_line() + 
  scale_y_continuous(limits = c(0,100)) +
  facet_wrap(~ country_name)


energy_cleaned %>% 
  filter(country_name %in% target_countries) %>% 
  select(
    country_name, 
    yr,
    perc_renewable_of_total_electricity_output,
    total_final_energy_consumption_tfec
  ) %>% 
  
  ggplot(aes(x = yr, 
             y = total_final_energy_consumption_tfec,
             colour = country_name)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~ country_name)


## top five economies in 2005

target_countries <-  c("United States", 
                       "Japan",
                       "Germany",
                       "United Kingdom",
                       "China", 
                       "India")

## top five SouthEast Asian economies (by GDP) today

target_countries <-  c("Indonesia", 
                       "Singapore",
                       "Thailand",
                       "Vietnam",
                       "Philipines")

# large and growing Asian economies

target_countries <-  c("China", 
                       "India",
                       "Japan",
                       "Vietnam",
                       "Phillipines",
                       "Indonesia")


energy_cleaned %>% 
  filter(country_name %in% target_countries) %>% 
  select(
    country_name, 
    yr,
    perc_renewable_of_total_electricity_output,
    total_final_energy_consumption_tfec
  ) %>% 
  
  ggplot(aes(x = yr, 
             y = total_final_energy_consumption_tfec,
             colour = country_name)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~ country_name,
             scales = "free_y")


energy_cleaned %>% 
  filter(country_name %in% target_countries) %>% 
  select(
    country_name, 
    yr,
    total_electricity_output_gigawatt_hours
  ) %>% 
  
  ggplot(aes(x = yr, 
             y = total_electricity_output_gigawatt_hours,  # total electricity output in Gigawatt-hours
             colour = country_name)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~ country_name,
             scales = "free_y")


energy_cleaned %>% 
  filter(country_name %in% target_countries) %>% 
  select(
    country_name, 
    yr,
    renewable_energy_consumption_tfec_pct
  ) %>% 
  
  ggplot(aes(x = yr, 
             y = renewable_energy_consumption_tfec_pct,  # pct of energy that was Renewable
             colour = country_name)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~ country_name,
             scales = "free_y")



energy_cleaned %>% 
  filter(country_name %in% target_countries) %>% 
  select(
    country_name, 
    yr,
    perc_renewable_of_total_electricity_output
  ) %>% 
  
  ggplot(aes(x = yr, 
             y = perc_renewable_of_total_electricity_output,  # Renewable energy pct of tot elec output
             colour = country_name)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~ country_name,
             scales = "free_y")


energy_cleaned %>% 
  filter(country_name %in% target_countries) %>% 
  select(
    country_name, 
    yr,
    solar_energy_consumption_tfec_pct
  ) %>% 
  
  ggplot(aes(x = yr, 
             y = solar_energy_consumption_tfec_pct,  # pct of energy that was solar
             colour = country_name)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~ country_name,
             scales = "free_y")


## China and India
target_countries <-  c("China", 
                       "India")


energy_cleaned %>% 
  filter(country_name %in% target_countries) %>% 
  select(
    country_name, 
    yr,
    biogas_consumption_tfec_pct, 
    geothermal_energy_consumption_tfec_pct,  # pct of energy that was geothermal
    hydro_energy_consumption_tfec_pct,  # pct of energy that was hydro
    liquid_biofuels_energy_consumption_tfec_pct, # pct of energy that was liquid biofuels
    marine_energy_consumption_tfec_pct, # pct of energy that was marine
    modern_biomass_energy_consumption_tfec_pct, # pct of energy that was modern biomass
    renewable_energy_consumption_tfec_pct, # pct of energy that was renewable
    solar_energy_consumption_tfec_pct, # pct of energy that was solar
    traditional_biomass_consumption_tfec_pct, # pct of energy that was traditional biomass
    waste_energy_consumption_tfec_pct, # pct of energy that was waste energy
    wind_energy_consumption_tfec_pct # pct of energy that was wind
  ) %>% 
  
  ggplot(aes(x = yr, 
             colour = country_name)) + 
  geom_point(aes(y = traditional_biomass_consumption_tfec_pct))  +  # pct of energy that was biomass
  geom_line(aes(y = traditional_biomass_consumption_tfec_pct)) + 
  
  geom_point(aes(y = renewable_energy_consumption_tfec_pct))  +  # pct of energy that was solar
  geom_line(aes(y = renewable_energy_consumption_tfec_pct)) + 
  
  geom_point(aes(y = biogas_consumption_tfec_pct))  +  # pct of energy that was solar
  geom_line(aes(y = biogas_consumption_tfec_pct)) + 
  
  
  facet_wrap(~ country_name,
             scales = "free_y")



energy_cleaned %>% 
  filter(country_name %in% target_countries) %>% 
  select(
    country_name, 
    yr,
    ends_with("terajoules")
  ) %>%

  ggplot(aes( x= yr,
              y = total_final_consumption_terajoules,
              colour = country_name)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~ country_name)
  
  

energy_cleaned %>% 
  filter(country_name %in% target_countries) %>% 
  select(
    country_name, 
    yr,
    ends_with("terajoules")
  ) %>%

  ggplot(aes( x= yr,
              colour = country_name)) + 
  geom_point(aes(y = total_final_consumption_terajoules)) + 
  geom_line(aes(y = total_final_consumption_terajoules)) + 
  
  facet_wrap(~ country_name)



energy_cleaned %>% 
  filter(country_name == "China") %>% 
  select(
    country_name, 
    yr,
    ends_with("terajoules")
  ) %>%
  
  ggplot(aes(x= yr)) + 
  
  geom_point(aes(y = total_final_consumption_terajoules)) + 
  geom_line(aes(y = total_final_consumption_terajoules)) + 
  
  geom_point(aes(y = wind_energy_consumption_terajoules)) + 
  geom_line(aes(y = wind_energy_consumption_terajoules)) + 
  
  facet_wrap(~ country_name)


china_terajoules <- energy_cleaned %>% 
  filter(country_name == "China") %>% 
  select(
    country_name, 
    yr,
    ends_with("terajoules") & !contains("savings") & !contains("supply")
  ) %>% 
  rename_with(~ str_remove(., "_consumption_terajoules"))   %>%  ## consumtpion vars only
  pivot_longer(biogas:wind_energy,
               names_to = "energy", 
               values_to = "terajoules")


china_terajoules %>% 
  filter(energy != "total_final",
         energy != "marine", 
         energy != "renewable_energy", ) %>% 
  ggplot() + 
  geom_bar(aes(x = yr, 
               y = terajoules),
           stat = "identity") + 
  facet_wrap (~ energy, 
              scales = "free_y")


china_terajoules %>% 
  filter(energy != "total_final",
         energy != "marine", 
         energy != "renewable_energy", ) %>% 
  ggplot() + 
  geom_bar(aes(x = yr, 
               y = terajoules),
           stat = "identity") + 
  facet_wrap (~ energy)
  
china_terajoules %>% 
  filter(energy != "total_final",
         energy != "marine", 
         energy != "renewable_energy",
         energy != "traditional_biomass") %>% 
  ggplot() + 
  geom_bar(aes(x = yr, 
               y = terajoules),
           stat = "identity") + 
  facet_wrap (~ energy)


china_terajoules %>% 
  filter(energy == "total_final" | energy == "renewable_energy" |
           energy == "hydro_energy") %>% 
  ggplot() + 
  geom_line(aes(x = yr, 
               y = terajoules,
               colour = energy)) 


## hydro energy
## China and India
target_countries <-  c("China", 
                       "India")


target_countries <-  c("China", 
                       "India",
                       "Japan",
                       "Vietnam",
                       "Phillipines",
                       "Indonesia")


energy_cleaned %>% 
  filter(country_name %in% target_countries) %>% 
  select(
    country_name, 
    yr,
    hydro_energy_consumption_tfec_pct,  # pct of energy that was hydro
    solar_energy_consumption_tfec_pct, # pct of energy that was solar
    wind_energy_consumption_tfec_pct # pct of energy that was wind
  ) %>% 
  
  rename_with(~ str_remove(., "_consumption_tfec_pct"))   %>%  ## consumtpion vars only
  pivot_longer(hydro_energy:wind_energy,
               names_to = "energy", 
               values_to = "pct") %>% 
  
  ggplot(aes(x = yr, 
             y = pct,
             colour = country_name)) + 
  geom_line() + 
  facet_wrap(~energy)


energy_cleaned %>% 
  filter(country_name %in% target_countries) %>% 
  select(
    country_name, 
    yr,
    ends_with("terajoules") & !contains("savings") & !contains("supply")
  ) %>% 
  rename_with(~ str_remove(., "_consumption_terajoules"))   %>%  ## consumption vars only
  pivot_longer(biogas:wind_energy,
               names_to = "energy", 
               values_to = "terajoules") %>% 
  
  filter(energy == "hydro_energy") %>% 
  
  ggplot(aes(x = yr, 
             y = terajoules,
             colour = country_name)) + 
  geom_line() 


energy_cleaned %>% 
  filter(country_name %in% target_countries) %>% 
  select(
    country_name, 
    yr,
    ends_with("terajoules") & !contains("savings") & !contains("supply")
  ) %>% 
  rename_with(~ str_remove(., "_consumption_terajoules"))   %>%  ## consumption vars only
  pivot_longer(biogas:wind_energy,
               names_to = "energy", 
               values_to = "terajoules") %>% 
  
  filter(energy == "hydro_energy") %>% 
  
  ggplot(aes(x = yr, 
             y = terajoules,
             colour = country_name)) + 
  geom_point() +
  geom_smooth() +
  theme_bw()
