library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)

#' Process compensation points and tickets
#'
#' This function processes compensation data by converting between points and tickets,
#' handling missing values, and calculating total compensation. Points are automatically
#' converted to tickets when they exceed the threshold (default 100 points = 1 ticket),
#' and remaining points are tracked separately.
#'
#' @param patients_df A data frame of patient data from Foundry containing compensation data with columns:
#'   - `compensation_points` (integer or character; points earned by participant)
#'   - `compensation_tickets` (integer or character; tickets earned by participant)
#'   - Other columns are preserved but not modified
#'
#' @param pts_to_tix Integer specifying how many points equal one ticket.
#'   Defaults to 100 points per ticket.
#'
#' @return A data frame with the same structure as input plus:
#'   - `compensation_points` (integer; remaining points after ticket conversion)
#'   - `compensation_tickets` (integer; total tickets after ticket conversion)
#'   - `total_points` (integer; total compensation value in points)
#'
#' @details
#' The function performs the following operations:
#' 1. Converts compensation fields to integers if they're not already
#' 2. Replaces NA, empty string, NULL, or "0" ticket values with 0
#' 3. Converts excess points (>99) to tickets if no tickets exist
#' 4. Adjusts points balance after ticket conversion
#' 5. Calculates total compensation value in points
#'
#' @examples
#' \dontrun{
#' # Basic usage with default 100 points per ticket
#' processed_data <- process_compensation(patient_data)
#' 
#' # Custom points-to-ticket ratio
#' processed_data <- process_compensation(patient_data, pts_to_tix = 150)
#' }
#'
#' @export
process_compensation <- function(patients_df, pts_to_tix = 100) {
  
  # ensure compensation fields are integers
  patients_df <- patients_df %>%
    mutate(
      compensation_points = as.integer(compensation_points),
      compensation_tickets = as.integer(compensation_tickets)
    )
  
  # process compensation logic
  patients_df <- patients_df %>%
    mutate(
      # set all NA/empty ticket values to 0
      compensation_tickets = case_match(compensation_tickets,
                                        c(NA, 0, NULL) ~ 0L,   # 0L ensures integer instead of numeric
                                        .default = compensation_tickets),
      
      # convert points to tickets when points > 99 but user doesn't have any tix
      compensation_tickets = case_when(
        compensation_points > 99 & compensation_tickets < 1 ~ floor(compensation_points / pts_to_tix),
        .default = compensation_tickets
      ),
      
      # reset points after conversion
      compensation_points = case_when(
        compensation_points > 99 & compensation_tickets >= 1 ~ 
          compensation_points - (compensation_tickets * pts_to_tix),
        .default = compensation_points
      ),
      
      # calculate total points (new field)
      total_points = as.integer(compensation_points + (pts_to_tix * compensation_tickets))
    )
  
  return(patients_df)
}


#' Clean raw patient data for analysis
#'
#' This function processes a raw patient data frame by selecting relevant columns,
#' renaming them for clarity, extracting family and position information from patient IDs,
#' annotating vitiligo status, converting date/time columns to a specified timezone,
#' summarizing family-level vitiligo counts and multiplex/simplex family status,
#' and processing compensation data.
#'
#' @param patients_raw A data frame containing raw patient data with columns including but not limited to:
#'   - `_id`
#'   - `patient_id`
#'   - `mdh_participant_id`
#'   - `age`
#'   - `invitationstatus`
#'   - `enrollmentdate`
#'   - `inserteddate`
#'   - `lastUpdateDate`
#'   - `enrolled`
#'   - `active_kitstatus`
#'   - `aou_complete`
#'   - `shipment_group`
#'   - `compensation_points`
#'   - `compensation_tickets`
#'   - `studyendreason`
#'
#' @param current_tz A string specifying the timezone for datetime conversion.
#'    Defaults to the system timezone returned by `Sys.timezone()`.
#'   
#' @param pts_to_tix Integer specifying how many points equal one ticket for 
#'    compensation processing. Defaults to 100. 
#'
#' @return A cleaned tibble with the following columns:
#'   - `patient_id`
#'   - `mdh_participant_id`
#'   - `last_updated_date` (POSIXct in `current_tz`)
#'   - `age` (decimal age computed from reported DOB in MDH)
#'   - `FamilyID` (integer extracted from patient_id)
#'   - `fam_pos` (integer indicating family position extracted from patient_id)
#'   - `vit_status_exp` (character; "vitiligo" or "healthy")
#'   - `vitiligo_status` (numeric; indicates having vitiligo at ANY TIME; 0 = no vitiligo, 1 = vitiligo)
#'   - `has_vitiligo` (integer; indicates initial VIT onset BEFORE enrolling; 0 = no, 1 = yes)
#'   - `has_vitiligo_post_baseline` (numeric; indicates initial VIT onset AFTER enrolling; 0 = no, 1 = yes)
#'   - `plex` (character; one of "multiplex", "simplex", or "zeroplex" based on family vitiligo counts)
#'   - `num_affected` (integer count of vitiligo cases per family)
#'   - `invitation_status` (updated invitation status with enrolled override)
#'   - `enrolled` (logical)
#'   - `active_kitstatus`
#'   - `aou_complete`
#'   - `enroll_date` (POSIXct in `current_tz`)
#'   - `invite_date` (POSIXct in `current_tz`)
#'   - `shipment_group`
#'   - `compensation_points` (integer; number of points earned towards next raffle ticket)
#'   - `compensation_tickets` (integer; number of total raffle tickets earned)
#'   - `total_points` (integer; total compensation value in points)
#'   - `studyendreason`
#'
#' @examples
#' \dontrun{
#' cleaned_data <- clean_patients_data(raw_patients_df, current_tz = "America/New_York")
#' 
#' # With custom points-to-ticket ratio
#' cleaned_data <- clean_patients_data(raw_patients_df, pts_to_tix = 150)
#' }
#'
#' @export
clean_patients_data <- function(patients_raw,
                                current_tz = Sys.timezone(),
                                pts_to_tix = 100) {
  
  patients_cleaned <- patients_raw %>%
    select(`_id`, patient_id, mdh_participant_id, everything()) %>%
    rename(
      invitation_status = invitationstatus,
      enroll_date = enrollmentdate,
      invite_date = inserteddate,
      last_updated_date = lastUpdateDate
    ) %>%
    mutate(
      # add "enrolled" to the status
      invitation_status = case_match(enrolled, TRUE ~ "enrolled",
                                     .default = invitation_status),
      FamilyID = as.integer(str_extract(patient_id, "[0-9]+")),
      fam_pos = as.integer(str_match(patient_id, "^[^_]+_\\d+_(\\d+)")[, 2]),
      # OLD FIELD - should be deprecated
      vit_status_exp = case_when(
        str_detect(patient_id, "_v$") ~ "vitiligo",
        .default = "healthy"
      ),
      # NEW FIELDS
      has_vitiligo_post_baseline = case_when(
        has_vitiligo == 1 ~ NA,
        has_vitiligo == 0 & is.na(has_vitiligo_post_baseline) ~ 0,
        .default = has_vitiligo_post_baseline
      ), 
      # convert to datetime fields
      across(.cols = c(enroll_date, invite_date, last_updated_date),
             .fns = ~ with_tz(as_datetime(.x), tzone = current_tz))
    ) %>%
    # calculate each pt's current VIT status
    rowwise() %>%
    mutate(vitiligo_status = sum(c_across(c(has_vitiligo, 
                                            has_vitiligo_post_baseline)), 
                                 na.rm = T)) %>%
    ungroup() %>%
    
    # # count num of vit pt's in each fam
    # mutate(
    #   num_affected = sum(vit_status_exp == "vitiligo"),
    #   .by = FamilyID
    # ) %>%
    
    # count number of VIT-affected pts in each family
    mutate(
      num_affected = sum(vitiligo_status),
      .by = FamilyID
    ) %>%
    # handle plex label
    mutate(
      plex = case_when(
        num_affected >= 2 ~ "multiplex",
        num_affected == 1 ~ "simplex",
        num_affected == 0 ~ "zeroplex"
      )
    ) %>%
    # TODO: create and insert a function to handle raffle points + tickets
    # process compensation data
    process_compensation(pts_to_tix = pts_to_tix) %>%
    select(
      patient_id,
      mdh_participant_id,
      last_updated_date,
      age,
      FamilyID,
      fam_pos,
      vit_status_exp,
      vitiligo_status,
      has_vitiligo,
      has_vitiligo_post_baseline,
      plex,
      num_affected,
      invitation_status,
      enrolled,
      active_kitstatus,
      aou_complete,
      enroll_date,
      invite_date,
      shipment_group,
      compensation_points,
      compensation_tickets,
      total_points,
      studyendreason
    )
  
  return(patients_cleaned)
}


#' Clean and join raw survey data with patient data
#'
#' This function processes raw survey responses by joining them with cleaned patient data,
#' filtering out unmatched records, converting datetime fields to a specified timezone,
#' extracting and tidying answer text, and unnesting answers into individual rows.
#'
#' @param surveys_raw A data frame containing raw survey response data. Must include columns:
#'   - `participantid` (used to join with patient data)
#'   - `date`, `inserteddate`, `creationDate`, `lastUpdateDate` (datetime fields)
#'   - `answers` (raw survey answer strings)
#'   - `id` (survey response ID)
#'   - Other columns related to survey metadata
#'
#' @param patients_clean A cleaned patient data frame, typically output from `clean_patients_data()`,
#'   with column `mdh_participant_id` used for joining.
#'
#' @param ans_pat A regex pattern string used to extract answer text from the raw `answers` field.
#'   Defaults to `"\\\"(.*?)\\\""` (matches quoted strings).
#'
#' @param current_tz A string specifying the timezone to which all datetime fields
#'   should be converted. Defaults to `"America/New_York"`.
#'
#' @return A cleaned tibble where each row corresponds to one extracted answer from the survey,
#'   including joined patient metadata and converted datetime fields.
#'
#' @examples
#' \dontrun{
#' cleaned_surveys <- clean_surveys_data(surveys_raw_df, patients_clean_df)
#' }
#'
#' @export
clean_surveys_data <- function(surveys_raw,
                               patients_clean,
                               ans_pat = "\\\"(.*?)\\\"",
                               current_tz = "America/New_York") {
  
  surveys_clean <- surveys_raw %>%
    left_join(
      y = patients_clean,
      by = c("participantid" = "mdh_participant_id"),
      suffix = c("", "_pt")
    ) %>%
    filter(!is.na(patient_id)) %>%
    mutate(
      # Convert datetimes to specified timezone
      across(
        .cols = c(date, inserteddate, creationDate, lastUpdateDate),
        .fns = ~ with_tz(as_datetime(.x), tzone = current_tz)
      ),
      # Extract date without time
      recorded_date = as_date(date),
      # Extract answer text
      answers = answers %>%
        str_extract(ans_pat) %>%
        str_to_lower() %>%
        str_split("\",\"")
    ) %>%
    rename(
      recorded_dttm = date,
      response_id = id
    ) %>%
    select(
      patient_id, recorded_dttm, recorded_date, response_id, everything(),
      -c(participantid:surveyid, inserteddate:DID)
    ) %>%
    unnest(answers)
  
  return(surveys_clean)
}


#' Create a summary table of enrollment statistics
#'
#' This function computes enrollment statistics at three levels: individuals, families, and founders.
#' It compares the full patient dataset (`patients_all`) to the cleaned dataset (`patients_clean`)
#' and calculates metrics such as number enrolled, number invited, number dropped, and percent enrolled.
#'
#' @param patients_all A data frame containing all invited patients with columns `patient_id`, `FamilyID`, `fam_pos`, and `invitation_status`.
#' @param patients_clean A cleaned and filtered subset of patients_all, with the same required columns.
#' @param last_updated_fdry A POSIXct timestamp indicating the last update time of the data (typically 5 AM EST).
#'
#' @return A tidy data frame with one row per level (`individuals`, `families`, `founders`) and columns:
#'   - `last_updated`
#'   - `level`
#'   - `enrolled`, `invited`, `dropped`
#'   - `enroll_pct`
#'   - `avg_enrolled_fam_size` (only for families)
#'
#' @examples
#' make_summary_enrollment_table(patients_all, patients_clean, last_updated_fdry)
#'
#' @export
make_summary_enrollment_table <- function(patients_all, patients_clean, last_updated_fdry) {
  patients_all %>%
    summarize(
      last_updated = first(last_updated_fdry),
      
      # Individual-level
      pt_enrolled = n_distinct(patient_id[invitation_status == "enrolled"]),
      pt_invited = n_distinct(patient_id),
      pt_dropped = length(setdiff(patient_id, patients_clean$patient_id)),
      
      # Family-level
      fam_enrolled = n_distinct(FamilyID[invitation_status == "enrolled"]),
      fam_invited = n_distinct(FamilyID),
      fam_dropped = length(setdiff(FamilyID, patients_clean$FamilyID)),
      
      # Founders
      founders_enrolled = n_distinct(patient_id[fam_pos == 1 & invitation_status == "enrolled"]),
      founders_invited = n_distinct(patient_id[fam_pos == 1]),
      founders_dropped = length(setdiff(
        patient_id[fam_pos == 1],
        patients_clean$patient_id[patients_clean$fam_pos == 1]
      ))
    ) %>%
    mutate(
      avg_enrolled_fam_size = pt_enrolled / fam_enrolled,
      pt_enroll_pct = pt_enrolled / pt_invited,
      fam_enroll_pct = fam_enrolled / fam_invited,
      founders_enroll_pct = founders_enrolled / founders_invited
    ) %>%
    pivot_longer(
      cols = -last_updated,
      names_to = "metric",
      values_to = "value"
    ) %>%
    mutate(level = case_when(
      str_starts(metric, "pt_") ~ "individuals",
      str_starts(metric, "fam_") ~ "families",
      str_starts(metric, "founders") ~ "founders",
      str_starts(metric, "avg_enrolled_fam_size") ~ "families",
      TRUE ~ NA_character_
    )) %>%
    mutate(metric = str_remove(metric, "^(pt_|fam_|founders_)")) %>%
    pivot_wider(names_from = metric, values_from = value) %>%
    relocate(last_updated, level)
}

