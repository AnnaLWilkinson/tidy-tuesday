

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

## Occupation
claims_occupation_raw <- claims_dfs[["Occupation"]]


# Process data  -----------------------------------------------------------

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









