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
#' @param force_all_dates Logical. If TRUE, returns rows for all dates in range even if files are missing.
#'   If FALSE (default), only returns rows for dates where at least one file exists.
#' 
#' @return A tibble with columns:
#' \describe{
#'   \item{file_date}{Date of the files.}
#'   \item{summary_file}{Full file path to the summary CSV from the previous day (or NA if missing).}
#'   \item{detail_file}{Full file path to the detail CSV from the current date (or NA if missing).}
#' }
#' 
#' @keywords internal
find_fitbit_hrv_files <- function(start_date, end_date, root_dir, force_all_dates = FALSE) {
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
    
    # Check if we should include this date
    has_summary <- length(summary_file) == 1
    has_detail <- length(detail_file) == 1
    
    if (!force_all_dates && !has_summary && !has_detail) {
      return(NULL)  # Skip this date if no files exist
    }
    
    tibble(
      file_date = d,
      summary_file = ifelse(has_summary, summary_file, NA_character_),
      detail_file = ifelse(has_detail, detail_file, NA_character_)
    )
  })
  
  return(files_df)
}


#' Read and Join Fitbit HRV Summary and Detail Files for a Single Date
#' 
#' Helper function that reads the summary file from the previous day and detail file from 
#' the current date, joins them by `file_date`, and handles missing files gracefully. 
#' By default, returns NULL if the detail file is missing (no row created), but this can 
#' be overridden with force_missing_detail. This aligns summary data with detail data 
#' to correct the one-day offset in Fitbit's HRV reporting.
#' 
#' @param file_date Date. The date corresponding to the detail file to read.
#' @param summary_file Character or NA. File path to the summary CSV from previous day or NA if missing.
#' @param detail_file Character or NA. File path to the detail CSV from current date or NA if missing.
#' @param summary_only Logical. If TRUE, only summary data is returned.
#' @param force_missing_detail Logical. If TRUE, creates a row with NA detail values even when detail file is missing.
#'   If FALSE (default), returns NULL when detail file is missing (unless summary_only = TRUE).
#' 
#' @return A tibble containing joined data for the date, NULL if detail file missing and not forced, 
#'   or NA-filled rows if files are missing but forced.
read_and_join_hrv_for_date <- function(file_date, summary_file, detail_file, summary_only = FALSE, force_missing_detail = FALSE) {
  # If both files are missing, always return NULL
  if (is.na(summary_file) && is.na(detail_file)) {
    warning("No files found for ", format(file_date, "%Y-%m-%d"), ". Skipping.")
    return(NULL)
  }
  
  # If detail file is missing and we're not in summary_only mode and not forcing missing details
  if (is.na(detail_file) && !summary_only && !force_missing_detail) {
    warning("Missing detail file for ", format(file_date, "%Y-%m-%d"), ". Skipping (use force_missing_detail = TRUE to include).")
    return(NULL)
  }
  
  if (is.na(summary_file)) {
    warning("Missing summary file for ", format(file_date - days(1), "%Y-%m-%d"), " (needed for ", format(file_date, "%Y-%m-%d"), ")")
  }
  
  if (is.na(detail_file) && !summary_only) {
    warning("Missing detail file for ", format(file_date, "%Y-%m-%d"), " - creating row with NA detail values")
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


#' Load Fitbit Resting Heart Rate Data for a Date Range
#' 
#' Reads the daily resting heart rate file from Google Data export and filters
#' to the specified date range. Formats the data for easy joining with HRV data.
#' 
#' @param start_date Date or character. Start date (inclusive) of data to load.
#' @param end_date Date or character. End date (inclusive) of data to load.
#' @param root_dir Character. Root directory containing Fitbit data folders.
#' 
#' @return A tibble with columns:
#' \describe{
#'   \item{file_date}{Date extracted from timestamp.}
#'   \item{timestamp}{Original timestamp from file.}
#'   \item{resting_hr}{Resting heart rate in beats per minute.}
#' }
#' 
#' @examples
#' \dontrun{
#' load_fitbit_resting_hr("2025-07-29", "2025-07-31", "path/to/fitbit/root")
#' }
load_fitbit_resting_hr <- function(start_date, end_date, root_dir) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  rhr_file <- file.path(root_dir, "Physical Activity_GoogleData", "daily_resting_heart_rate.csv")
  
  if (!file.exists(rhr_file)) {
    warning("Resting heart rate file not found at: ", rhr_file)
    return(tibble())
  }
  
  rhr_data <- readr::read_csv(rhr_file, show_col_types = FALSE) %>%
    mutate(
      timestamp = as.POSIXct(timestamp, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      file_date = as.Date(timestamp),
      resting_hr = `beats per minute`
    ) %>%
    filter(file_date >= start_date & file_date <= end_date) %>%
    select(file_date, timestamp, resting_hr)
  
  return(rhr_data)
}


#' Load Fitbit Daily Readiness Data for a Date Range
#'
#' Reads the Fitbit daily readiness CSV from the Google Data export, filters it
#' to the specified date range, and formats it for joining with other Fitbit
#' metrics. Includes overall readiness score as well as category-specific
#' readiness indicators.
#'
#' @param start_date Date or character. Start date (inclusive) of data to load.
#' @param end_date Date or character. End date (inclusive) of data to load.
#' @param root_dir Character. Root directory containing Fitbit data folders.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{timestamp}{POSIXct. Original timestamp from the file (UTC).}
#'   \item{file_date}{Date. Date extracted from the timestamp.}
#'   \item{daily_readiness_cat}{Character. Overall readiness category.}
#'   \item{sleep_readiness_cat}{Character. Readiness category for sleep.}
#'   \item{hrv_readiness_cat}{Character. Readiness category for heart rate variability.}
#'   \item{rhr_readiness_cat}{Character. Readiness category for resting heart rate.}
#'   \item{daily_readiness_score}{Numeric. Overall readiness score.}
#' }
#'
#' @examples
#' \dontrun{
#' load_fitbit_daily_readiness("2025-07-29", "2025-07-31", "path/to/fitbit/root")
#' }

load_fitbit_daily_readiness <- function(start_date, end_date, root_dir) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  ready_file <- file.path(root_dir, "Physical Activity_GoogleData", "daily_readiness.csv")
  
  if (!file.exists(ready_file)) {
    warning("Daily readiness file not found at: ", ready_file)
    return(tibble())
  }
  
  ready_data <- readr::read_csv(ready_file, show_col_types = FALSE) %>%
    transmute(
      file_date = as.Date(timestamp),
      daily_readiness_cat = type,
      sleep_readiness_cat = `sleep readiness`,
      hrv_readiness_cat = `heart rate variability readiness`,
      rhr_readiness_cat = `resting heart rate readiness`,
      daily_readiness_score = score
    ) %>%
    filter(file_date >= start_date & file_date <= end_date)
  
  return(ready_data)
}



#' Combine Multiple Fitbit Data Types by Time Period
#' 
#' Joins multiple Fitbit datasets (HRV, resting HR, etc.) by a specified time period.
#' Supports aggregation by day, month, hour, or minute.
#' 
#' @param ... Data frames to combine. Each should have a `file_date` column or timestamp column.
#' @param by Character. Time period for aggregation: "day" (default), "month", "hour", or "minute".
#' @param join_type Character. Type of join: "full" (default), "inner", "left".
#' 
#' @return A tibble with combined data aggregated by the specified time period.
#' 
#' @examples
#' \dontrun{
#' hrv_data <- load_fitbit_hrv("2025-07-29", "2025-07-31", "path/to/root")
#' rhr_data <- load_fitbit_resting_hr("2025-07-29", "2025-07-31", "path/to/root")
#' 
#' # Combine by day (default)
#' combined <- combine_fitbit_data(hrv_data, rhr_data)
#' 
#' # Combine by month
#' combined_monthly <- combine_fitbit_data(hrv_data, rhr_data, by = "month")
#' }
combine_fitbit_data <- function(..., by = "day", join_type = "full") {
  datasets <- list(...)
  
  if (length(datasets) == 0) {
    warning("No datasets provided.")
    return(tibble())
  }
  
  # Validate join_type
  if (!join_type %in% c("full", "inner", "left")) {
    stop("join_type must be one of: 'full', 'inner', 'left'")
  }
  
  # Validate by parameter
  if (!by %in% c("day", "month", "hour", "minute")) {
    stop("by must be one of: 'day', 'month', 'hour', 'minute'")
  }
  
  # Determine time grouping column name
  time_col <- case_when(
    by == "day" ~ "file_date",
    by == "month" ~ "year_month", 
    by == "hour" ~ "datetime_hour",
    by == "minute" ~ "datetime_minute"
  )
  
  # Process each dataset
  processed_datasets <- map(datasets, function(df) {
    if (nrow(df) == 0) {
      return(df)
    }
    
    # Ensure we have a proper timestamp for hour/minute aggregation
    df_processed <- df
    
    # Handle timestamp column creation/conversion
    if ("timestamp" %in% names(df_processed)) {
      if (is.character(df_processed$timestamp)) {
        df_processed <- df_processed %>%
          mutate(timestamp = as.POSIXct(timestamp))
      }
      # If it's already POSIXct, leave it as is
    } else if ("file_date" %in% names(df_processed)) {
      df_processed <- df_processed %>%
        mutate(timestamp = as.POSIXct(file_date))
    } else {
      df_processed <- df_processed %>%
        mutate(timestamp = as.POSIXct(NA))
    }
    
    # Add the appropriate grouping column based on 'by' parameter
    if (by == "day") {
      # For daily, ensure file_date exists
      if (!"file_date" %in% names(df_processed)) {
        df_processed <- df_processed %>%
          mutate(file_date = as.Date(timestamp))
      }
    } else if (by == "month") {
      df_processed <- df_processed %>%
        mutate(year_month = floor_date(
          if ("file_date" %in% names(df_processed)) file_date else as.Date(timestamp), 
          "month"
        ))
    } else if (by == "hour") {
      df_processed <- df_processed %>%
        mutate(datetime_hour = floor_date(timestamp, "hour"))
    } else if (by == "minute") {
      df_processed <- df_processed %>%
        mutate(datetime_minute = floor_date(timestamp, "minute"))
    }
    
    # Check if we have the required time column
    if (!time_col %in% names(df_processed)) {
      stop("Could not create required time column '", time_col, "' for dataset")
    }
    
    # Group and summarize numeric columns for non-daily aggregation
    if (by != "day") {
      numeric_cols <- df_processed %>% 
        select(where(is.numeric)) %>% 
        names()
      
      if (length(numeric_cols) > 0) {
        # For non-daily aggregation, summarize numeric columns
        summary_exprs <- map(numeric_cols, ~ expr(mean(!!sym(.x), na.rm = TRUE))) %>%
          set_names(numeric_cols)
        
        df_processed <- df_processed %>%
          group_by(!!sym(time_col)) %>%
          summarise(!!!summary_exprs, .groups = "drop")
      } else {
        # No numeric columns, just get distinct time periods
        df_processed <- df_processed %>%
          select(!!sym(time_col)) %>%
          distinct()
      }
    } else {
      # For daily data, keep all columns but ensure we have the time column
      df_processed <- df_processed %>%
        select(!!sym(time_col), everything(), -any_of(c("timestamp", "year_month", "datetime_hour", "datetime_minute"))) %>%
        distinct()
    }
    
    return(df_processed)
  })
  
  # Remove empty datasets
  processed_datasets <- processed_datasets[map_lgl(processed_datasets, ~ nrow(.x) > 0)]
  
  if (length(processed_datasets) == 0) {
    warning("All datasets are empty after processing.")
    return(tibble())
  }
  
  if (length(processed_datasets) == 1) {
    return(processed_datasets[[1]])
  }
  
  # Join datasets
  result <- processed_datasets[[1]]
  
  for (i in 2:length(processed_datasets)) {
    if (join_type == "full") {
      result <- full_join(result, processed_datasets[[i]], by = time_col)
    } else if (join_type == "inner") {
      result <- inner_join(result, processed_datasets[[i]], by = time_col)
    } else if (join_type == "left") {
      result <- left_join(result, processed_datasets[[i]], by = time_col)
    }
  }
  
  return(result)
}
