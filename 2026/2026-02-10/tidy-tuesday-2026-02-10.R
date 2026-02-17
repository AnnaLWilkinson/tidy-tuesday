# About this script -------------------------------------------------------

#  Project: Tidy Tuesday
#  Purpose: The 2026 Winter Olympics
#  Author:  Anna Wilkinson; anna.wilkinson@burnet.edu.au
#  Date started: 10 Feb 2026
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
library(baffle)
library(conflicted)
library(sysfonts)
library(showtextdb)
library(showtext)
library(paletteer)
library(fontawesome)
conflicted::conflicts_prefer(dplyr::select)
conflicted::conflicts_prefer(dplyr::filter)

# Import data  ------------------------------------------------------------