# data-raw/aps_employee_census_2024_individual.R





# Setup and Configuration ------------------------------------------------------
library(httr2)
library(readr)
library(janitor)
library(dplyr)

# Define common NA strings that might appear in the raw data
COMMON_NA_STRINGS <- c("NA", "N/A", "-", "", " ")

VARIABLES_LIKERT_SCALE_AGREEMENT <- c(
  "q17",
  "q18",
  "q20",
  "q22",
  "q23",
  "q24",
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

VARIABLES_LIKERT_SCALE_AGREEMENT_AND_NA <- c(
  "q32",
  "q33"
)

LEVELS_LIKERT_SCALE_AGREEMENT_AND_NA <- c(
  "Strongly agree",
  "Agree",
  "Neither agree nor disagree",
  "Disagree",
  "Strongly disagree",
  "Not applicable"
)

VARIABLES_LIKERT_SCALE_LIKELIHOOD <- c(
  "q34",
  "q38"
)

LEVELS_LIKERT_SCALE_LIKELIHOOD <- c(
  "Always",
  "Often",
  "Sometimes",
  "Rarely",
  "Never"
)

VARIABLES_YES_NO <- c(
  "q44",
  "q45"
)

LEVELS_YES_NO <- c(
  "Yes",
  "No"
)

VARIABLES_YES_NO_NOTSURE <- c(
  "q30",
  "q47"
)

LEVELS_YES_NO_NOTSURE <- c(
  "Yes",
  "No",
  "Not sure"
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
  show_col_types = FALSE, # Suppress column type messages for cleaner output
  na = COMMON_NA_STRINGS,
  col_types = cols(.default = col_character())
)





# Data Cleaning and Transformation ----------------------------------------
aps_employee_census_2024_individual <- raw_aps_employee_census_2024_individual |>
  janitor::clean_names() |>
  rename(
    agency_size = as, # Renaming 'as' column for clarity
    gender = q1,
    age_range = q2,
    classification = q4
  ) |>

  # Convert agency_size, gender, age_range and classification to factors
  mutate(
    agency_size = factor(
      agency_size,
      levels = c(
        "Small (Less than 250 employees)",
        "Medium (251 to 1,000 employees)",
        "Large (1,001 or more employees)"
      ), ordered = TRUE
    ),

    gender = factor(
      gender,
      levels = c(
        "Man or male",
        "Woman or female",
        "Non-binary",
        "I use a different term",
        "Prefer not to say"
      ), ordered = TRUE
    ),

    age_range = factor(
      age_range,
      levels = c(
        "Under 40 years",
        "40-54 years",
        "55 years or older",
        "Prefer not to say"
      ), ordered = TRUE
    ),

    classification = factor(
      classification,
      levels = c("Trainee/Graduate/Cadet/APS", "EL", "SES"),
      ordered = TRUE
    )
  ) |>

  # Convert various question types to ordered factors with explicit levels
  mutate(
    across(
    starts_with(VARIABLES_LIKERT_SCALE_AGREEMENT),
    ~ factor(., levels = c(
      "Strongly agree",
      "Agree",
      "Neither agree nor disagree",
      "Disagree",
      "Strongly disagree"
    ), ordered = TRUE)
  ),

  across(
    starts_with(VARIABLES_LIKERT_SCALE_AGREEMENT_AND_NA),
    ~ factor(., levels = c(
      "Strongly agree",
      "Agree",
      "Neither agree nor disagree",
      "Disagree",
      "Strongly disagree",
      "Not applicable"
    ), ordered = TRUE)
  ),

  across(
    starts_with("q28"),
    ~ factor(., levels = c(
      "Not at all",
      "Very little",
      "Somewhat",
      "To a great extent",
      "To a very great extent"
    ), ordered = TRUE)
  ),

  across(
    starts_with(VARIABLES_LIKERT_SCALE_LIKELIHOOD),
    ~ factor(., levels = c(
      "Always",
      "Often",
      "Sometimes",
      "Rarely",
      "Never"
    ), ordered = TRUE)
  ),

  across(
    starts_with(VARIABLES_YES_NO),
    ~ factor(., levels = c(
      "Yes",
      "No"
    ), ordered = TRUE)
  ),

  across(
    starts_with(VARIABLES_YES_NO_NOTSURE),
    ~ factor(., levels = c(
      "Yes",
      "No",
      "Not Sure"
    ), ordered = TRUE)
  ),

  q26 = factor(
    q26,
    levels = c(
      "Excellent",
      "Very good",
      "Average",
      "Below average",
      "Well below average"
    ),
    ordered = TRUE
  ),

  q27 = factor(
    q27,
    levels = c(
      "Well above capacity - too much work",
      "Slightly above capacity - lots of work to do",
      "At capacity - about the right amount of work to do",
      "Slightly below capacity - available for more work",
      "Well below capacity - not enough work"
    ),
    ordered = TRUE
  ),

  q40 = factor(
    q40,
    levels = c(
      "I want to leave my position as soon as possible",
      "I want to leave my position within the next 12 months",
      "I want to stay working in my position for the next one to two years",
      "I want to stay working in my position for at least the next three years"
    ),
    ordered = TRUE
  ),

  q41 = factor(
    q41,
    levels = c(
      "I am planning to retire",
      "I am pursuing another position within my agency",
      "I am pursuing a position in another agency",
      "I am pursuing work outside the APS",
      "It is the end of my non-ongoing, casual or contracted employment",
      "Other"
    ),
    ordered = TRUE
  ),

  # There are 18 levels with no ordering
  q42 = factor(q42),

  q50 = factor(q50),

  q52 = factor(
    q52,
    levels = c(
      "Yes",
      "No",
      "Not sure",
      "Would prefer not to answer"
    ),
    ordered = TRUE
  ),

  q54 = factor(q54)
  )





# Data Saving -------------------------------------------------------------
# Save the processed data into the `data/` directory
usethis::use_data(aps_employee_census_2024_individual, overwrite = TRUE)

# Clean up the temporary file
unlink(temp_file)
