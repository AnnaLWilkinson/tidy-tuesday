

#  About this script ------------------------------------------------------

#  Project: Tidy Tuesday
#  Purpose: Submit WorkSafe data to Tidy Tuesday
#  Author: Anna Wilkinson; anna.wilkinson@burnet.edu.au
#  Date started: 22 September 2026


# Load libraries ----------------------------------------------------------

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
      , ~ read_excel(tmp, sheet = .x) |>
        remove_empty(which = c('rows', 'cols')) # Get rid of completely empty columns and rows
    )
  )

sheets <- claims_df_raw$name
sheets

# Process data  -----------------------------------------------------------

## Agency of injury  ------------------------------------------------------
## Bodily Location  -------------------------------------------------------
## Industry division  -----------------------------------------------------
## Injury year  -----------------------------------------------------------
## Mechanism of injury  ---------------------------------------------------
## Nature of injury  ------------------------------------------------------
## Occupation -------------------------------------------------------------

claims_df <-
  claims_df_raw |>
  # The column names of the dataframes tell us which dataframes/sheets contained claims data
  # Hoist pulls out the first column name into `fname`
  mutate(
    df_names = map(
      data
      , names
    )
  ) |>
  hoist(.col = df_names, fname = 1) |>
  # Subset to the data frames that contain claims data by filtering on `fname`
  filter(fname == 'Scheme standardised claims') |>
  # Age and gender is too different - we'll handle it separately
  filter(name != 'Age and gender') |>
  # Fix up the names in the data
  mutate(
    data = map(
        data
        , ~.x |>
          # Most sheets have the real column 1 name in row 2. Filling lets us put the value into row 3 so that it can be included when we use row_to_names
          fill(starts_with('Scheme'), .direction = 'down') |>
          row_to_names(3) |>
          clean_names()
      )
  ) |>
  # Get rid of intermediate columns
  select(-any_of(c('fname', 'df_names', 'glimpse'))) |>
  # Create a column with tidy data by pivoting year columns
  mutate(
    data_tidy = map(
      data
      , ~ .x |>
      pivot_longer(
        cols = starts_with('x')
        , names_to = 'financial_year'
        , values_to = 'claims'
      ) |>
      mutate(financial_year = gsub('^x', '', financial_year))
    )
    , data_tidy = case_when(
      name == 'Agency of injury' ~ map(
        data_tidy
        , ~.x |> rename_with(~gsub('na', 'subcategory_of_agency', .), starts_with('na'))
      )
      , TRUE ~ data_tidy
    )
  )


## Age and gender ----------------------------------------------------------
# We take the age and gender data frame separately because the data is
# In stacked tables within the sheet

claims_age_gender_raw <-
  claims_df_raw |>
  filter(name == 'Age and gender') |>
  pull(data) |>
  pluck(1) |>
  janitor::clean_names() |>
  mutate(
    gender = case_when(
      scheme_standardised_claims %in% c(
        "Female"
        ,"Male"
        ,"I use a different term"
        ,"Non-Binary/Gender Diverse"
        ,"Prefer not to say"
        ,"Total"
      ) ~ gsub('[ -/]', '_', tolower(scheme_standardised_claims))
      , TRUE ~ NA_character_
    )
  ) |>
  fill(gender, .direction =  "down")

# Nest the subtables and fix the column names
# Some column names are duplicated in the raw data
claims_age_gender_data <-
  claims_age_gender_raw |>
  group_by(gender) |>
  nest() |>
  filter(!is.na(gender)) |>
  mutate(
    data = map(
      data
      , ~.x |>
      fill(scheme_standardised_claims, .direction = 'down') |>
      row_to_names(3) |>
      clean_names() |>
      filter(
        !grepl('Scheme standardised', age_group)
        , !grepl('For period', age_group)
      ) |>
      remove_empty('cols')
    )
  ) |>
  unnest(data)

# Convert to a data frame with the wide and tidy data
age_gender_df <-
  claims_age_gender_data |>
  mutate(name = 'Age and gender') |>
  group_by(name) |>
  nest() |>
  mutate(
    data_tidy = map(
      data
      , ~.x |>
      pivot_longer(
        starts_with('x')
        , names_to = "financial_year"
        , values_to = 'claims'
      ) |>
      mutate(
        financial_year = gsub('^x', '', financial_year)
        , claims = as.integer(str_trim(claims))
      )
    )
  )

# Package up clean dfs -----------------------------------------------------

dfs <- sheets[-c(1:3)]

claims_data_clean <-
  bind_rows(
    claims_df
    , age_gender_df
  ) |>
  select(name, data = data_tidy)

## END

