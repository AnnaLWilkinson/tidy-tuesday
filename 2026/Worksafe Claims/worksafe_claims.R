

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


# Create a df with all valid age groups and all possible years of data
df_all_age_yr <-  claims_age_gender_clean |> 
  filter(gender == "female") |> 
  select(-gender) |> 
  remove_empty("cols") |> 
  fill(scheme_standardised_claims, .direction = "down") |> 
  row_to_names(row_number = 3) |> 
  clean_names() |> 
  filter(!str_detect(age_group, c("For period|All"))) |> 
  filter(str_detect(age_group, "-") | str_detect(age_group, "65+")| str_detect(age_group, "Under 15")| str_detect(age_group, "Not Stated"))  |> 
  
  pivot_longer(-age_group, 
               names_to = "financial_year",
               values_to = "claims") |> 
  mutate(financial_year = str_remove_all(financial_year, "x")) |> 
  distinct(age_group, financial_year)


### I use a different term df
diff_term <-  claims_age_gender_clean |> 
  filter(gender == "diff_term") |> 
  remove_empty("cols") |> 
  fill(scheme_standardised_claims, .direction = "down") |> 
  row_to_names(row_number = 3) |> 
  clean_names() |> 
  filter(!str_detect(age_group, c("For period|All"))) |> 
  rename(gender = diff_term) |> 
  pivot_longer(-c(age_group, gender), 
               names_to = "financial_year", 
               values_to = "claims") |> 
  mutate(financial_year = str_remove_all(financial_year, "x"),
         claims = as.numeric(str_trim(claims)))


# join with all possible age groups and fin years
diff_term_full <- full_join(df_all_age_yr, diff_term, by = c("age_group", "financial_year"))

### Non-binary/gender diverse
nonbin <-  claims_age_gender_clean |> 
  filter(gender == "non_binary_diverse") |> 
  remove_empty("cols") |> 
  fill(scheme_standardised_claims, .direction = "down") |> 
  row_to_names(row_number = 3) |> 
  clean_names() |> 
  filter(!str_detect(age_group, c("For period|All|Scheme"))) |> 
  rename(gender = non_binary_diverse) |> 
  pivot_longer(-c(age_group, gender), 
               names_to = "financial_year", 
               values_to = "claims") |> 
  mutate(financial_year = str_remove_all(financial_year, "x"),
         claims = as.numeric(str_trim(claims)))

# join with all possible age groups and fin years
non_bin_full <- full_join(df_all_age_yr, nonbin, by = c("age_group", "financial_year"))


### Prefer not to say
prefnotsay <-  claims_age_gender_clean |> 
  filter(gender == "prefer_not_say") |> 
  remove_empty("cols") |> 
  fill(scheme_standardised_claims, .direction = "down") |> 
  row_to_names(row_number = 3) |> 
  clean_names() |> 
  filter(!str_detect(age_group, c("For period|All|Scheme"))) |> 
  rename(gender = prefer_not_say) |> 
  pivot_longer(-c(age_group, gender), 
               names_to = "financial_year", 
               values_to = "claims") |> 
  mutate(financial_year = str_remove_all(financial_year, "x"),
         claims = as.numeric(str_trim(claims)))

# join with all possible age groups and fin years
pref_not_say_full <- full_join(df_all_age_yr, prefnotsay, by = c("age_group", "financial_year"))


### female and male
fe_ma <-  claims_age_gender_clean |> 
  filter(gender == "female" | gender == "male") |> 
  remove_empty("cols") |> 
  fill(scheme_standardised_claims, .direction = "down") |> 
  row_to_names(row_number = 3) |> 
  clean_names() |> 
  filter(!str_detect(age_group, c("For period|All|Scheme|Age|Male"))) |> 
  rename(gender = female) |> 
  pivot_longer(-c(age_group, gender), 
               names_to = "financial_year", 
               values_to = "claims") |> 
  mutate(financial_year = str_remove_all(financial_year, "x"),
         claims = as.numeric(str_trim(claims)))


### Bind rows
# all age groups and yrs: nrow = 182; 5 gender categories (female, male, non binary (duplicate year), diff term, pref not say)
(182*4) + 188  # nrow = 916

claims_age_gender_clean_bind <- bind_rows(fe_ma, pref_not_say_full)
(182*2) + 182
nrow(claims_age_gender_clean_bind)

claims_age_gender_clean_bind <- bind_rows(claims_age_gender_clean_bind, non_bin_full)
546 + 182
nrow(claims_age_gender_clean_bind)

claims_age_gender_clean_bind <- bind_rows(claims_age_gender_clean_bind, diff_term_full)
728 + 188
nrow(claims_age_gender_clean_bind)


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
  filter(bodily_location !="Total") |> 
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
  filter(agency_of_injury != "All") |> 
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
  filter(occupation != "Total") |> 
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
  filter(industry_division != "Total") |> 
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

