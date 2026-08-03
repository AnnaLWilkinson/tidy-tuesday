
# About this script  ------------------------------------------------------

#  Purpose:  Submit Manta Tow Data to Tidy Tuesday 
#  Project:  Manta Tow data
#  Author:   Anna Wilkinson; anna.wilkinson@burnet.edu.au
#  Date started: 29 July 2026


# Load libraries ----------------------------------------------------------

library(tidyverse)

# Load data ---------------------------------------------------------------

#Citation:
#Australian Institute of Marine Science (AIMS). (2015). 
#AIMS Long-term Monitoring Program: Crown-of-thorns starfish and benthos Manta Tow Data (Great Barrier Reef).
#https://doi.org/10.25845/5c09b0abf315a, accessed 29-Jul-2026.

#Data collection:
# Observations started in January 1982 and are ongoing.
# 
# For each two-minute manta tow the variables recorded are: Number and size of COTS; 
# Percentage cover of live coral, dead coral, and soft coral; 
# Visibility; Any other observations of note.
# 
# The ambient variables recorded include, information about the survey
# (reef name, time, date, data collectors), and the weather conditions:
#   (Wind strength; Cloud cover; Sea state; and Tide).
# 
# Information is also recorded about the reef environment of each survey 
# (e.g. reef slope, substratum at reef base) and its benthic community
# (dominant benthic group, dominant hard coral, and dominant coral life form).


# 1. Define the direct download link for "Data Summarised to reef"
zip_url <- "https://api.aims.gov.au/data-v2.0/5bb9a340-4ade-11dc-8f56-00008a07204e/files/AIMS_LTMP_manta-tow-by-reef.zip"

# 2. Create an isolated temporary file path
temp_zip <- tempfile()

# 3. Download the zip folder (using binary mode "wb" for cross-platform stability)
download.file(zip_url, temp_zip, mode = "wb")

# 4. View the files packed inside the downloaded zip folder
file_list <- unzip(temp_zip, list = TRUE)
print(file_list)

# 5. Extract and read the target CSV file into an R data frame
# Note: Replace "AIMS_LTMP_manta-tow-by-reef.csv" if the unzipped name differs
manta_data_raw <- read.csv(unz(temp_zip, "manta-tow-by-reef/manta-tow-by-reef.csv"))

# 6. Preview the loaded Manta Tow dataset
head(manta_data_raw)

# 7. Clean up the downloaded temporary file from disk
unlink(temp_zip)



# Cleaning ----------------------------------------------------------------

manta_data_raw %>% 
  count(MEDIAN_LIVE_CORAL)


manta_data <- manta_data_raw %>% 
  janitor::clean_names() %>% 
  dplyr::rename(sector_code = sector,
                shelf_code  = shelf) %>% 
  dplyr::mutate(sample_date = lubridate::ymd(sample_date), 
                sector_name = case_when(
                  sector_code == "CA"   ~ "Cairns",
                  sector_code == "CB"   ~ "Capricorn-Bunker",
                  sector_code == "CG"   ~ "Cape Grenville",
                  sector_code == "CL"   ~ "Cooktown-Lizard Island",
                  sector_code == "CU"   ~ "Cape Upstart",
                  sector_code == "IN"   ~ "Innisfail",
                  sector_code == "PC"   ~ "Princess Charlotte Bay",
                  sector_code == "PO"   ~ "Pompey",
                  sector_code == "SW"   ~ "Swain",
                  sector_code == "TO"   ~ "Townsville",
                  sector_code == "TS"   ~ "Torres Strait",
                  sector_code == "WH"   ~ "Whitsunday",
                  is.na(sector_code)    ~ NA_character_,
                  TRUE ~ "CHECK"),
                shelf_name  = case_when(
                  shelf_code == "I" ~ "Inshore GBR",
                  shelf_code == "M" ~ "Mid-shelf GBR",
                  shelf_code == "O" ~ "Outer-shelf GBR",
                  is.na(shelf_code) ~ NA_character_,
                  TRUE ~ "CHECK"),
                
                

               )
                ))














