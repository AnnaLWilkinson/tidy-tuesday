

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

claims_dfs <- excel_sheets(tmp) |>
  set_names() |>
  map(~ read_excel(tmp, sheet = .x))


#  Occupation  ------------------------------------------------------------

claims_occupation_raw <- claims_dfs[["Occupation"]]

## Process data  -----------------------------------------------------------

new_names <- claims_occupation_raw |> 
  slice(4) |> 
  janitor::clean_names() |> 
  select(starts_with("x")) |> 
  mutate(occupation = "occupation",
         across(everything(), ~ str_replace_all(., "/", "_"))) |> 
  select(occupation, everything()) |> 
  unlist()
new_names

claims_occupation_clean <- claims_occupation_raw |> 
  setNames(new_names) |> 
  slice(-(1:4)) |> 
  pivot_longer(-occupation, 
               names_to = "financial_year", 
               values_to = "claims")




# Age and gender ----------------------------------------------------------

claims_age_gender_raw <- claims_dfs[["Age and gender"]]

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
  slice(5) |> 
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
  slice(-(1:5))

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
               values_to = "claims")







