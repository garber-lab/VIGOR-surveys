# load libs

if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
if (!requireNamespace("RCurl", quietly = TRUE)) {
  install.packages("RCurl")
}
if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here")
}

library(tidyverse)
library(RCurl)

# set project root anchor by specifying the credentials file location relative
# to current script
here::i_am("redcap_credentials.R")

# source the credentials file to get REDCap API tokens (for TJ)
source(here::here("redcap_credentials.R"))


api_url <- "https://arcsapps.umassmed.edu/redcap/api/"

# read in Phase 2 data
result_phase2 <- postForm(
  api_url,
  token = api_token_phase2,
  content = 'record',
  format = 'csv',
  type = 'flat',                      # output as one record per row
  rawOrLabel = 'label',               # show labels for multiple choice fields
  rawOrLabelHeaders = 'label',        # export field labels
  exportSurveyFields = 'true'         # export survey ID and time stamp
)

# create function to preprocess the data
preprocess <- function(api_result, form_type) {
  # split api result (single string) into a chracter vector --> split on every newline NOT preceded by whitespace
  cv <- str_split_1(string = api_result, pattern = "(?<!\\s)\n")
  
  # store char. vector in 1 column DF
  cv_df <- tibble(cv)
  
  # init. col names --> split on every comma NOT followed by whitespace
  col_names <- str_split(cv[1], "\\,(?!\\s)")[[1]]
  
  # count number of blank columns in col_names
  blank_counter <- 0
  
  # assign names to blank columns ("")
  for (i in 1:length(col_names)) {
    if (col_names[i] == "") {
      blank_counter <- blank_counter + 1
      col_names[i] <- str_c("blank", blank_counter, sep = "_")
    }
  }
  
  # # assign names to blank col names ("")
  # for (i in 1:length(col_names)) {
  #   col_names[i] <- if_else(col_names[i] == "",
  #                           "blank",
  #                           col_names[i])
  # }
  # 
  # # assign subscript to duplicated col names
  # for (i in 1:length(col_names)) {
  #   col_names[i] <- if_else(i %in% which(duplicated(col_names)),
  #                           paste(col_names[i], "2", sep = "_"),
  #                           col_names[i])
  # }
  
  # separate data into multi columns --> split on every comma NOT followed by whitespace ("\s")
  df <- cv_df %>% 
    separate(col = cv,
             into = col_names,
             sep = "\\,(?!\\s)") %>% 
    # filter out first (header) and last (empty) rows
    filter(!`"Record ID"` %in% c("\"Record ID\"", "")) %>%
    mutate(form = form_type) %>%
    select(1, form, everything())
  
  # remove redundant quotation marks from values
  colnames(df) <- colnames(df) %>%
    str_replace_all(pattern = "\"", replacement = "") %>%
    str_squish()
  
  # clean up DF
  df_clean <- df %>%
    mutate(
      # convert character fields to integers
      across(
        c(
          `Record ID`,
          `Birth year`,
          `Zipcode`,
          `Postal Code`,
          Child,
          Parent,
          Sibling,
          Cousin,
          `Nieces/Nephews`
        ),
        as.integer
      ),
      # convert timestamp to datetimes
      across(c(`Survey Timestamp`), 
             as_datetime),
      # remove redundant quotation marks from all other char fields
      across(
        .cols = where(is.character),
        .fns = ~ str_replace_all(string = .x,
                                 pattern = "\"",
                                 replacement = "") %>% 
          str_squish()
      )
    ) %>%
    # mutate(p1_id = row_number()) %>%
    select(-c(blank_1, blank_2))
  
  
  # updated RedCap form with PC gender choices (April 2024)
  df_clean <- df_clean %>%
    rename(Sex = `Gender Identity`)
  
  
  return(df_clean)
}


phase2 <- preprocess(result_phase2, "phase2")


# filter out records who dropped out or are too young
p2_clean <- phase2 %>%
  filter(`Dropped out?` != "Yes") %>%
  filter(`Birth year` <= year(today()) - 6)

# manually add Referral IDs to P2 responses which don't have one
p2_clean <- p2_clean %>%
  mutate(
    `Referral ID` = case_when(
      `Record ID` == 364 ~ "53",    # confirmed by Manuel on Oct 2, 2023
      `Record ID` == 450 ~ "139",   # confirmed by Manuel on Oct 4, 2023 [slack]
      .default = `Referral ID`
    )
  )


# # split result into character vector where each character is 1 row
# cv_phase2 <- str_split_1(
#   string = result_phase2, 
#   pattern = "(?<!\\s)\n"            # split on every newline NOT preceded by a whitespace character (")
# )
# 
# # store char vector as tibble with 1 column
# cv_df_phase2 <- tibble(cv_phase2)
# 
# # init. col names for DF to store data in
# col_names_phase2 <- str_split(cv_phase2[1], "\\,(?! )")[[1]]
# 
# # replace the blank names with actual names
# col_names_phase2[66] <- "blank_1"
# col_names_phase2[67] <- "blank_2"
# 
# # separate data into multi columns
# df_phase2 <- cv_df_phase2 %>% separate(
#   col = cv_phase2,
#   into = col_names_phase2,
#   sep = "\\,(?!\\s)"      # split on every comma that is NOT followed by whitespace ("\s")
# ) %>% filter(
#   !`"Record ID"` %in% c("\"Record ID\"", "")
# ) %>%
#   # add new col to identify which Phase data it is
#   mutate(form = "phase2") %>%
#   select(1, form, everything())
# 
# 
# # remove redundant quotation marks from values
# colnames(df_phase2) <- colnames(df_phase2) %>% 
#   str_replace_all(pattern = "\"", replacement = "") %>%
#   str_squish()
# 
# 
# # conditionally add Canada-specific columns if they don't already exist
# if (FALSE %in% (c("Province", "Postal Code") %in% colnames(df_phase2))) {
#   df_phase2 <- df_phase2 %>%
#     mutate("Province" = NA,
#            "Postal Code" = NA) %>%
#     select(
#       `Record ID`:State,
#       Province,
#       Zipcode,
#       `Postal Code`,
#       `Phone Number (no spaces or dashes):`:`Complete?`
#     )
# }
# 
# # clean up DF
# p2_clean <- df_phase2 %>%
#   # convert char fields to numeric
#   mutate(
#     across(.cols = c(`Record ID`, `Referral ID`, `Birth year`, 
#                      Zipcode, `Postal Code`, Child, Parent, Sibling),
#            .fns = as.integer)
#   ) %>%
#   # remove redundant quotation marks from all char fields
#   mutate(across(.cols = where(is.character),
#                 .fns = ~ str_replace_all(string = .x,
#                                          pattern = "\"",
#                                          replacement = "") %>% str_squish()
#   )) %>%
#   # remove the 2 blank columns
#   select(-c(blank_1:blank_2)) %>%
#   # filter out DA test entry
#   filter(`Record ID` != 1)
