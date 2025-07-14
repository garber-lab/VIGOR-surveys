# VIGOR-surveys

# Patient and Survey Data Processing Pipeline

This repository provides a modular, standardized framework for querying, cleaning, and analyzing patient and survey data from the Foundry API. The goal is to ensure consistent data handling across projects and teams by centralizing core functions that:

- Pull raw data via Foundry API sessions  
- Clean and preprocess patient and survey datasets  
- Prepare data for downstream analyses, including phenotype creation  

---

## Repository Structure

- **`credentials.R`**  
  Contains API credentials and secrets loaded securely for authenticating with the Foundry platform.  
  **Note:** The `.gitignore` file excludes the `credentials.R` file to protect sensitive information.  
  The credentials file should be formatted like this:

  ```r
  # User credentials
  email <- 'person@email.com'
  password <- 'FoundryP@ssword'
  ```
- **`constants.R`**  
  Defines global constants such as API URLs, timezones, and regex patterns to be used throughout the pipeline.

- **`query_functions.R`**  
  Houses functions responsible for initializing API sessions, authenticating, and retrieving raw patient and survey data from Foundry.

- **`/surveys/survey_functions.R`**  
  Includes functions focused on post-processing raw survey data—such as cleaning, extracting answers, joining with patient data, and generating phenotypes.

---

## Usage

The pipeline is designed to be modular, enabling you to:

1. Initialize an authenticated session with Foundry via `query_functions.R`.  
2. Load raw patient and survey data using shared querying functions.  
3. Clean and preprocess datasets using dedicated cleaning functions, ensuring consistent transformations and formats.  
4. Perform analysis or downstream processing with confidence that data is processed uniformly.

---

## Why Modular?

By encapsulating querying and cleaning steps into reusable functions, we:

- Avoid duplicating code across projects  
- Ensure consistent application of cleaning rules and date/time conversions  
- Facilitate collaboration by providing a shared, documented interface for data processing  
- Simplify debugging and maintenance as all key transformations are in one place

---

## Contributing

Please contribute any new functions or improvements by adding them to the appropriate module (`query_functions.R` or `/surveys/survey_functions.R`). This approach helps maintain a clean separation of concerns between data retrieval and data processing.

---

## Getting Started

Load the modules in your R session with:

```r
source(str_c(here(), "/credentials.R"))
source(str_c(here(), "/constants.R"))
source(str_c(here(), "/query_functions.R"))
source(str_c(here(), "/surveys/survey_functions.R"))
```
Then, proceed with writing your notebook as usual! Totally ok to include your notebook in this repo as well.
