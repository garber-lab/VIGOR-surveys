library(tidyverse)
library(lubridate)

find_fitbit_hrv_files <- function(start_date, end_date, root_dir) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  date_seq <- seq(start_date, end_date, by = "day")
  
  hrv_dir <- file.path(root_dir, "Heart Rate Variability")
  
  files_df <- map_dfr(date_seq, function(d) {
    summary_file <- list.files(
      path = hrv_dir,
      pattern = paste0(
        "Daily Heart Rate Variability Summary - ",
        format(d, "%Y-%m"),
        "\\-\\(",
        format(d, "%d"),
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


read_and_join_hrv_for_date <- function(file_date, summary_file, detail_file, summary_only = FALSE) {
  if (is.na(summary_file) && is.na(detail_file)) {
    warning("No files found for ", format(file_date, "%Y-%m-%d"), ". Skipping.")
    return(NULL)
  }
  
  if (is.na(summary_file)) {
    warning("Missing summary file for ", format(file_date, "%Y-%m-%d"))
  }
  
  if (is.na(detail_file) && !summary_only) {
    warning("Missing detail file for ", format(file_date, "%Y-%m-%d"))
  }
  
  # Read summary or create empty tibble with expected columns & one row for date
  summary_df <- if (!is.na(summary_file)) {
    readr::read_csv(summary_file, show_col_types = FALSE) %>%
      mutate(file_date = file_date)
  } else {
    # create 1-row tibble with NA summary columns and the file_date
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
  
  # Read details or create empty tibble with expected columns & one row for date
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

