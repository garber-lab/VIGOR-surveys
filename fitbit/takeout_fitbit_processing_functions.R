library(tidyverse)
library(lubridate)

#' Find Fitbit HRV Summary and Detail Files for a Date Range
#' 
#' Helper function that searches the specified root directory for Fitbit Heart Rate Variability
#' summary and detail files within a given date range. For each date, it looks for the summary
#' file from the previous day to align with the detail file from the current date.
#' 
#' @param start_date Date or character. Start date (inclusive) for searching files.
#' @param end_date Date or character. End date (inclusive) for searching files.
#' @param root_dir Character. Root directory path containing the Fitbit data folder structure.
#' 
#' @return A tibble with columns:
#' \describe{
#'   \item{file_date}{Date of the files.}
#'   \item{summary_file}{Full file path to the summary CSV from the previous day (or NA if missing).}
#'   \item{detail_file}{Full file path to the detail CSV from the current date (or NA if missing).}
#' }
#' 
#' @keywords internal
find_fitbit_hrv_files <- function(start_date, end_date, root_dir) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  date_seq <- seq(start_date, end_date, by = "day")
  
  hrv_dir <- file.path(root_dir, "Heart Rate Variability")
  
  files_df <- map_dfr(date_seq, function(d) {
    # For summary file, look for the previous day's file to align with current day's detail
    prev_date <- d - days(1)
    
    summary_file <- list.files(
      path = hrv_dir,
      pattern = paste0(
        "Daily Heart Rate Variability Summary - ",
        format(prev_date, "%Y-%m"),
        "\\-\\(",
        format(prev_date, "%d"),
        "\\)\\.csv$"
      ),
      full.names = TRUE
    )
    
    detail_file <- list.files(
      path = hrv_dir,
      pattern = paste0(
        "Heart Rate Variability Details - ",
        format(d, "%Y-%m-%d"),
        "\\.csv$"
      ),
      full.names = TRUE
    )
    
    tibble(
      file_date = d,
      summary_file = ifelse(length(summary_file) == 1, summary_file, NA_character_),
      detail_file = ifelse(length(detail_file) == 1, detail_file, NA_character_)
    )
  })
  
  return(files_df)
}


#' Read and Join Fitbit HRV Summary and Detail Files for a Single Date
#' 
#' Helper function that reads the summary file from the previous day and detail file from 
#' the current date, joins them by `file_date`, and handles missing files gracefully by 
#' returning rows with `NA`s for missing data. This aligns summary data with detail data 
#' to correct the one-day offset in Fitbit's HRV reporting.
#' 
#' @param file_date Date. The date corresponding to the detail file to read.
#' @param summary_file Character or NA. File path to the summary CSV from previous day or NA if missing.
#' @param detail_file Character or NA. File path to the detail CSV from current date or NA if missing.
#' @param summary_only Logical. If TRUE, only summary data is returned.
#' 
#' @return A tibble containing joined data for the date, with NA-filled rows if files are missing.
#' 
#' @keywords internal
read_and_join_hrv_for_date <- function(file_date, summary_file, detail_file, summary_only = FALSE) {
  if (is.na(summary_file) && is.na(detail_file)) {
    warning("No files found for ", format(file_date, "%Y-%m-%d"), ". Skipping.")
    return(NULL)
  }
  
  if (is.na(summary_file)) {
    warning("Missing summary file for ", format(file_date - days(1), "%Y-%m-%d"), " (needed for ", format(file_date, "%Y-%m-%d"), ")")
  }
  
  if (is.na(detail_file) && !summary_only) {
    warning("Missing detail file for ", format(file_date, "%Y-%m-%d"))
  }
  
  # Read summary or create a 1-row tibble with NA values
  # Note: summary data is from the previous day but joined with current file_date
  summary_df <- if (!is.na(summary_file)) {
    readr::read_csv(summary_file, show_col_types = FALSE) %>%
      mutate(file_date = file_date)
  } else {
    tibble(
      timestamp = as.POSIXct(NA),
      rmssd = NA_real_,
      nremhr = NA_real_,
      entropy = NA_real_,
      file_date = file_date
    )
  }
  
  if (summary_only) {
    return(summary_df)
  }
  
  # Read details or create a 1-row tibble with NA values
  details_df <- if (!is.na(detail_file)) {
    readr::read_csv(detail_file, show_col_types = FALSE) %>%
      mutate(file_date = file_date)
  } else {
    tibble(
      timestamp = as.POSIXct(NA),
      rmssd = NA_real_,
      coverage = NA_real_,
      low_frequency = NA_real_,
      high_frequency = NA_real_,
      file_date = file_date
    )
  }
  
  full_join(details_df, summary_df, by = "file_date", suffix = c("_detail", "_summary"))
}


#' Load Fitbit Heart Rate Variability Data for a Date Range
#' 
#' Main function to load Fitbit HRV data for a specified date range by reading,
#' joining, and combining daily summary and detail CSV files. Uses helper functions
#' to find files and read/join each day's data. Supports optionally loading only summary data.
#' The function automatically handles the one-day offset by reading summary data from the
#' previous day to align with detail data from the current date.
#' 
#' @param start_date Date or character. Start date (inclusive) of data to load.
#' @param end_date Date or character. End date (inclusive) of data to load.
#' @param root_dir Character. Root directory containing Fitbit HRV data folder.
#' @param summary_only Logical. If TRUE, only summary data is loaded (default FALSE).
#' 
#' @return A tibble combining joined HRV summary and detail data for all dates in range.
#' Missing files produce rows with NA values but preserve the `file_date`.
#' 
#' @examples
#' \dontrun{
#' load_fitbit_hrv("2025-07-29", "2025-07-31", "path/to/fitbit/root")
#' load_fitbit_hrv("2025-07-29", "2025-07-31", "path/to/fitbit/root", summary_only = TRUE)
#' }
load_fitbit_hrv <- function(start_date, end_date, root_dir, summary_only = FALSE) {
  files_df <- find_fitbit_hrv_files(start_date, end_date, root_dir)
  
  if (nrow(files_df) == 0) {
    warning("No HRV files found in the given date range and directory.")
    return(tibble())
  }
  
  purrr::pmap_dfr(
    list(
      file_date = files_df$file_date,
      summary_file = files_df$summary_file,
      detail_file = files_df$detail_file
    ),
    read_and_join_hrv_for_date,
    summary_only = summary_only
  )
}
