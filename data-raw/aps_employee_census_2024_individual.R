# Setup and Configuration ------------------------------------------------------
library(httr2)
library(readr)
library(janitor)
library(dplyr)

#1 Strongly agree 2 Agree 3 Neither agree nor disagree 4 Disagree 5 Strongly disagree
VARIABLES_LIKERT_SCALE_AGREEMENT <- c(
  "q17",
  "q18",
  "q20",
  "q22",
  "q23",
  "q24",
  "q32",
  "q35",
  "q39",
  "q43"
)

LEVELS_LIKERT_SCALE_AGREEMENT <- c(
  "Strongly agree",
  "Agree",
  "Neither agree nor disagree",
  "Disagree",
  "Strongly disagree"
)

#TODO:  Continue adding more levels from here:
#https://data.gov.au/data/dataset/3191607e-aa7b-4e01-94ec-13bf412f8fb4/resource/389bcb22-c933-4f80-82c9-c1d28c7445f1/download/2024-aps-employee-census-data-dictionary.pdf

VARIABLES_LIKERT_SCALE_AGREEMENT_AND_NA <- c(
  "q32",
  "q33"
)
VARIABLES_LIKERT_SCALE_EXTENT <- c(
  "q28"
)

VARIABLES_LIKERT_SCALE_LIKELIHOOD <- c(
  "q34",
  "q38"
)

VARIABLES_YES_NO <- c(
  "q44",
  "q45"
)

VARIABLES_YES_NO_NOTSURE <- c(
  "q30",
  "q47"
)



# Data Loading ------------------------------------------------------------
# This is the direct download link for the CSV file
data_url <- "https://data.gov.au/data/dataset/3191607e-aa7b-4e01-94ec-13bf412f8fb4/resource/55b6e7e7-25be-4012-af43-edaaffbd2ace/download/2024-aps-employee-census-public-release.csv"

# Define a temporary path to save the downloaded file
temp_file <- tempfile(fileext = ".csv")

message("Downloading data from: ", data_url)

# Download the CSV file
req <- request(data_url) %>%
  req_retry(max_tries = 3) # Add retry logic for robustness

# This usually takes around 10-20 seconds
resp <- req_perform(req, path = temp_file)

# Check for HTTP errors
resp_check_status(resp) # This will throw an error if status is 4xx or 5xx

message("Download successful. File saved to: ", temp_file)
message("Reading CSV file...")

raw_aps_employee_census_2024_individual <- read_csv(
  file = temp_file,
  show_col_types = FALSE # Suppress column type messages for cleaner output
)

aps_employee_census_2024_individual <- raw_aps_employee_census_2024_individual |>
  janitor::clean_names() |>
  rename(
    agency_size = as,
    gender = q1,
    age_range = q2,
    classification = q4
  ) |>
  mutate(across(everything(), as.character)) |>
  mutate(across(
    starts_with(VARIABLES_LIKERT_SCALE_AGREEMENT),
    ~ factor(., levels = LEVELS_LIKERT_SCALE_AGREEMENT, ordered = TRUE)
  ))

usethis::use_data(aps_employee_census_2024_individual, overwrite = TRUE)
