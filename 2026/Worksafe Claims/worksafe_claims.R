

#  About this script ------------------------------------------------------

#  Project: Tidy Tuesday
#  Purpose: Submit WorkSafe data to Tidy Tuesday
#  Author: Anna Wilkinson; anna.wilkinson@burnet.edu.au
#  Date started: 22 September 2026


# Load libraries ----------------------------------------------------------

install.packages("jsonlite")
install.packages("httr2")
library(httr2)
library(jsonlite)
library(tidyverse)
library(readxl)
library(purrr)
library(janitor)


# API request -------------------------------------------------------------

# Dataset metadata
meta <- request(
  "https://discover.data.vic.gov.au/api/3/action/package_show?id=202109f7-7a6b-41c7-bb9b-ba6d90252e76"
) |>
  req_perform() |>
  resp_body_json()

# Extract URLs
urls <- map_chr(meta$result$resources, "url")
urls

# Load data ---------------------------------------------------------------

latest_url <- urls[length(urls)]

tmp <- tempfile(fileext = ".xlsx")

download.file(
  latest_url,
  tmp,
  mode = "wb"
)

excel_sheets(tmp)


# Read the data into a tibble of data frames
# The `name` column acts as an index
claims_df_raw <-
  tibble(name = excel_sheets(tmp)) |>
  mutate(
    data = map(
      name
      , ~ read_excel(tmp, sheet = .x) %>%
        remove_empty(which = c('rows', 'cols')) # Get rid of completely empty columns and rows
    )
  )

sheets <- claims_df_raw$name
sheets

# Process data  -----------------------------------------------------------

## Age and gender ----------------------------------------------------------

claims_age_gender_raw <- claims_df_raw |> 
  filter(name == 'Age and gender') |> 
  pull(data) |> 
  pluck(1)

claims_age_gender_clean <- claims_age_gender_raw |> 
  janitor::clean_names() |> 
  mutate(gender = case_when(
    
    scheme_standardised_claims == "Female" ~ "female", 
    scheme_standardised_claims == "Male"   ~ "male", 
    scheme_standardised_claims == "I use a different term"   ~ "diff_term", 
    scheme_standardised_claims == "Non-Binary/Gender Diverse" ~ "non_binary_diverse", 
    scheme_standardised_claims == "Prefer not to say" ~ "prefer_not_say", 
    scheme_standardised_claims == "Total" ~ "total", 
    TRUE ~ NA_character_
  )) |> 
  fill(gender, .direction =  "down")

new_names <- claims_age_gender_clean |> 
  slice(4) |> 
  janitor::clean_names() |> 
  select(starts_with("x")) |> 
  mutate(occupation = "age_group",
         gender = "gender",
         across(everything(), ~ str_replace_all(., "/", "_"))) |> 
  select(occupation, starts_with("x"), gender) |> 
  unlist()
new_names

claims_age_gender_clean <- claims_age_gender_clean |> 
  setNames(new_names) |> 
  slice(-(1:4))

valid_age_group <- claims_age_gender_clean |> 
  distinct(age_group) |> 
  filter(str_detect(age_group, "-") | str_detect(age_group, "65+")| str_detect(age_group, "Under 15")| str_detect(age_group, "Not Stated"), 
         !str_detect(age_group, "Non")) |> 
  unlist()
valid_age_group

claims_age_gender_clean <-  claims_age_gender_clean |> 
  filter(age_group %in% valid_age_group)

claims_age_gender_clean <-  claims_age_gender_clean |> 
  pivot_longer(-c(age_group, gender), 
               names_to = "financial_year",
               values_to = "claims") |> 
  mutate(claims = stringr::str_trim(claims),
         claims = as.numeric(claims))


## Mechanism of injury -----------------------------------------------------

claims_mechanism_raw <- claims_df_raw |> 
  filter(name == 'Mechanism of injury') |> 
  pull(data) |> 
  pluck(1)

claims_mechanism_clean <-  claims_mechanism_raw |> 
  fill(`Scheme standardised claims`, .direction = "down") |> 
  row_to_names(row_number = 3) |> 
  clean_names() |> 
  filter(mechanism_of_injury_disease != "Total") |> 
  pivot_longer(-mechanism_of_injury_disease, 
               names_to = "financial_year", 
               values_to = "claims") |> 
  mutate(financial_year = str_remove_all(financial_year, "x"),
         claims = as.numeric(str_trim(claims))) 
        

## Nature of injury (affliction) ------------------------------------------

claims_nature_of_injury_raw <- claims_df_raw |> 
  filter(name == 'Nature of injury (affliction)') |> 
  pull(data) |> 
  pluck(1)

claims_nature_of_injury_clean <-  claims_nature_of_injury_raw |> 
  fill(`Scheme standardised claims`, .direction = "down") |> 
  row_to_names(row_number = 3) |> 
  clean_names() |> 
  filter(nature_of_injury != "Total") |> 
  pivot_longer(-nature_of_injury, 
               names_to = "financial_year", 
               values_to = "claims") |> 
  mutate(financial_year = str_remove_all(financial_year, "x"),
         claims = as.numeric(str_trim(claims))) 


## Bodily location ---------------------------------------------------------

claims_bodily_location_raw <- claims_df_raw |> 
  filter(name == 'Bodily location') |> 
  pull(data) |> 
  pluck(1)

claims_bodily_location_clean <-  claims_bodily_location_raw |> 
  fill(`Scheme standardised claims`, .direction = "down") |> 
  row_to_names(row_number = 3) |> 
  clean_names() |> 
  filter(-1 != "Total") |> 
  pivot_longer(-1, 
               names_to = "financial_year", 
               values_to = "claims") |> 
  mutate(financial_year = str_remove_all(financial_year, "x"),
         claims = as.numeric(str_trim(claims))) 


## Agency of injury --------------------------------------------------------

claims_agency_of_injury_raw <- claims_df_raw |> 
  filter(name == 'Agency of injury') |> 
  pull(data) |> 
  pluck(1)

claims_agency_of_injury_raw[1:3, 2] <- "sub_agency_of_injury"
  
claims_agency_of_injury_clean <-  claims_agency_of_injury_raw |> 
  fill(`Scheme standardised claims`, .direction = "down") |> 
  row_to_names(row_number = 3) |> 
  clean_names() |> 
  filter(-1 != "Total") |> 
  pivot_longer(-c(1:2), 
               names_to = "financial_year", 
               values_to = "claims") |> 
  mutate(financial_year = str_remove_all(financial_year, "x"),
         claims = as.numeric(str_trim(claims))) 


##  Occupation  ------------------------------------------------------------

claims_occupation_raw <- claims_df_raw |> 
  filter(name == 'Occupation') |> 
  pull(data) |> 
  pluck(1)

claims_occupation_clean <- claims_occupation_raw |> 
  fill(`Scheme standardised claims`, .direction = "down") |> 
  row_to_names(row_number = 3) |> 
  clean_names() |> 
  filter(-1 != "Total") |> 
  pivot_longer(-1, 
               names_to = "financial_year", 
               values_to = "claims") |> 
  mutate(financial_year = str_remove_all(financial_year, "x"),
         claims = as.numeric(str_trim(claims))) 


## Industry division -----------------------------------------------------

claims_industry_division_raw <- claims_df_raw |> 
  filter(name == 'Industry division') |> 
  pull(data) |> 
  pluck(1)

claims_industry_division_clean <- claims_industry_division_raw |> 
  fill(`Scheme standardised claims`, .direction = "down") |> 
  row_to_names(row_number = 3) |> 
  clean_names() |> 
  filter(-1 != "Total") |> 
  pivot_longer(-1, 
               names_to = "financial_year", 
               values_to = "claims") |> 
  mutate(financial_year = str_remove_all(financial_year, "x"),
         claims = as.numeric(str_trim(claims))) 


# Package up clean dfs -----------------------------------------------------

dfs <- sheets[-c(1:3)]

claims_data_clean <- tibble(
  name = c(dfs),
  data = list(
    tibble(claims_age_gender_clean),
    tibble(claims_mechanism_clean), 
    tibble(claims_nature_of_injury_clean),
    tibble(claims_bodily_location_clean),
    tibble(claims_agency_of_injury_clean),
    tibble(claims_occupation_clean),
    tibble(claims_industry_division_clean)
    
))



## END


