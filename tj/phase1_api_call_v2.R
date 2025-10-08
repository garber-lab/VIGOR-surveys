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


# init. API URL
api_url <- "https://arcsapps.umassmed.edu/redcap/api/"

# export Phase 1 data from REDCap
result_phase1.1 <- postForm(
  uri = api_url,
  token = api_token_phase1.1,
  content = 'record',
  format = 'csv',
  type = 'flat',                  # outputs as 1 record per row
  rawOrLabel = 'label',           # show labels for multiple choice fields
  rawOrLabelHeaders = 'label',    # export field labels
  exportSurveyFields = 'true'     # export survey ID and time stamp
)

# export Phase 1 iPad data
result_ipad <- postForm(
  uri = api_url,
  token = api_token_ipad,
  content = "record",
  format = "csv",
  type = "flat",                  # outputs as 1 record per row
  rawOrLabel = "label",           # show labels for multiple choice fields
  rawOrLabelHeaders = "label",    # export field labels
  exportSurveyFields = 'true'     # export survey ID and time stamp
)

# export Phase 1 second round data
result_phase1.2 <- postForm(
  uri = api_url,
  token = api_token_phase1.2,
  content = "record",
  format = "csv",
  type = "flat",                  # outputs as 1 record per row
  rawOrLabel = "label",           # show labels for multiple choice fields
  rawOrLabelHeaders = "label",    # export field labels
  exportSurveyFields = 'true'     # export survey ID and time stamp
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
  
  
  # assign suffix to duplicate col names that appear more than once
  for (i in 1:length(col_names)) {
    if (i %in% which(duplicated(col_names))) {
      col_names[i] <- str_c(col_names[i], "2", sep = "_")
    }
  }

  # # assign subscript to duplicated col names
  # for (i in 1:length(col_names)) {
  #   col_names[i] <- if_else(i %in% which(duplicated(col_names)),
  #                           paste(col_names[i], "2", sep = "_"),
  #                           col_names[i])
  # }
  
  # separate data into multi columns --> split on every comma NOT followed by whitespace ("\s")
  df <- cv_df %>% separate(col = cv,
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
      across(c(`Record ID`, `Birth year`, `Zipcode`), 
             as.integer),
      # convert timestamps to datetimes
      across(c(`Survey Timestamp`, `Survey Timestamp_2`), 
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
    
  
  return(df_clean)
}


# init. Phase 1 data into preprocessed DFs
df_phase1.1 <- preprocess(result_phase1.1, "p1_primary") %>%
  rename(Sex = `Gender Identity`,
         "Do you live in the United States or Canada?" = `Do you live in the United States?`,
         "Dropped out?" = `Dropped Out?`)

df_ipad <- preprocess(result_ipad, "p1_ipad") %>%
  mutate("Do you live in the United States or Canada?" = "Yes") %>%
  rename("Dropped out?" = `Dropped Out?`) %>%
  # keep all records from Aug 15 or earlier
  filter(date(`Survey Timestamp`) <= "2023-08-15") %>%
  select(
    `Record ID`:`Survey Timestamp_2`,
    `Do you live in the United States or Canada?`,
    everything()
  )

df_phase1.2 <- preprocess(result_phase1.2, "p1_second_round") %>%
  # updated RedCap form with PC gender choices (April 2024)
  rename(Sex = `Gender Identity`)

# df_phase1_v2 <- bind_rows(df_phase1.1_v2, df_ipad, df_phase1.2)

# # remove redundant quotation marks from values
# colnames(df_phase1.1) <- colnames(df_phase1.1) %>%
#   str_replace_all(pattern = "\"", replacement = "") %>%
#   str_squish()
# 
# # clean up DF
# p1.1_clean <- df_phase1.1 %>%
#   mutate(
#     # convert character fields to integers
#     across(c(`Record ID`, `Birth year`, `Zipcode`), 
#            as.integer),
#     # convert timestamps to datetimes
#     across(c(`Survey Timestamp`, `Survey Timestamp_2`), 
#            as_datetime),
#     # remove redundant quotation marks from all other char fields
#     across(
#       .cols = where(is.character),
#       .fns = ~ str_replace_all(string = .x,
#                                pattern = "\"",
#                                replacement = "") %>% 
#         str_squish()
#     )
#   ) %>%
#   mutate(p1_id = row_number()) %>%
#   select(p1_id, everything(), -c(blank, blank_2))



# isolate 'initial phase1.1'
p1.1_initial <- df_phase1.1 %>%
  filter(`Survey Timestamp` < "2023-07-19")

# isolate 'new' phase1.1 (used for in-clinic patients)
p1.1_new <- df_phase1.1 %>%
  filter(`Survey Timestamp` > "2023-07-19")

# combine Phase 1 DFs
df_phase1_unclean <- bind_rows(
  p1.1_initial, 
  df_ipad, 
  df_phase1.2, 
  p1.1_new
) %>%
  # add 'universal' Phase 1 ID
  mutate(p1_id = row_number(),
         `Postal Code` = as.integer(`Postal Code`)) %>%
  # select(p1_id, everything())
  select(p1_id, `Record ID`:State, Province, Zipcode, `Postal Code`, everything())


# # remove phase 1.2 ID 54 (p1_id 1173; Referral ID 329) -> indicated via email that she no longer wants to participate (Sept 12, 2023)
# df_phase1 <- df_phase1 %>%
#   filter(`Record ID` != 54 | form != "p1_second_round")

# filter out all individuals who dropped out of the study OR who are too young
df_phase1 <- df_phase1_unclean %>%
  filter(`Dropped out?` != "Yes") %>%
  filter(`Birth year` <= year(today()) - 6)

# # remove "dormant" Referral IDs from existing recruiters with multiple
# df_phase1 <- df_phase1 %>%
#   filter(!`Referral ID` %in% c(381, 416, 43, 334, 387))


# subset all existing recruiters (referral ID exists)
p1_recruiters_existing <- df_phase1 %>%
  filter(`Referral ID` != "") %>%
  mutate(`Referral ID` = as.integer(`Referral ID`))

# # subset all distinct Phase 1 existing recruiters
# p1_recruiters_existing_distinct <- p1_recruiters_existing %>%
#   distinct(`First Name`, `Last Name`, `Birth year`, .keep_all = T)
# 
# # isolate all duplicate Phase 1 existing recruiters
# p1_recruiters_existing_dup <- p1_recruiters_existing %>%
#   filter(!p1_id %in% p1_recruiters_existing_distinct$p1_id)
# 
# # remove "dormant" Referral IDs from existing recruiters with multiple
# p1_recruiters_existing <- p1_recruiters_existing %>%
#   filter(!`Referral ID` %in% c(381, 416, 43, 334, 387))

# init. max existing Referral ID
max_ref_id <- p1_recruiters_existing %>% pull(`Referral ID`) %>% max()


# subset all non-recruiters to to see who fits criteria to become recruiter
p1_recruiters_new <- df_phase1 %>%
  filter(`Referral ID` == "") %>%
  # retain those live in US/Canada + gave consent
  filter(
    `Do you live in the United States or Canada?` == "Yes",
    `Do you consent to filling out your information and being contacted by our research team?` == "I consent"
  ) %>%
  # retain those who completed form, OR completed all relevant columns
  filter(
    (
      `Complete?_2` == "Complete" | (
        `First Name` != "" &
          `Last Name` != "" &
          `Do you have vitiligo?` != "" &
          # either address or city should be 
          (`Street Address` != "" | City != "") &
          # (State != "" | Province != "") &
          # State != "" &
          # TODO: Add province / Canada location stuff!!!
          (`Phone Number (no spaces or dashes):` != "" |
             Email != "")
      )
    ) & State != ""
  ) %>%
  # retain ppl who can recruit
  filter(
    # retain vitiligo patients who can recruit immediate family (Phase 1.1 + ipad)
    (
      `Would you be willing and able to recruit your siblings, children, or family members for this study?` == "Yes" |
        `How many children can you enroll?` > 0 |
        `How many siblings can you enroll?` > 0 |
        `How many family members can you enroll?` > 0
    ) |
      # OR, retain healthy patients with vitiligo family members (Phase 1.1 + ipad)
      (
        `Do you have an affected family member?` == "Yes" |
          `(choice=Child)` == "Checked" |
          `(choice=Parent)` == "Checked" |
          `(choice=Sibling)` == "Checked"
      ) |
      # OR, retain vitiligo patients who can recruit immediate family (Phase 1.2)
      (
        `Would you be willing and able to recruit your family members for this study?` == "Yes" |
          `How many children can you enroll?` > 0 |
          `How many siblings can you enroll?` > 0 |
          `How many parents can you enroll?` > 0 |
          `How many cousins can you enroll?` > 0 |
          `How many nieces and nephews can you enroll?` > 0 |
          `How many other people can you enroll?` > 0
      ) |
      # OR, retain healthy patients who can recruit vitiligo family members (Phase 1.2)
      (
        `Do you have an affected family member and if so, would you be willing to recruit them for this study?` == "Yes" |
          `How many children can you enroll?_2` > 0 |
          `How many siblings can you enroll?_2` > 0 |
          `How many parents can you enroll?_2` > 0 |
          `How many cousins can you enroll?_2` > 0 |
          `How many nieces and nephews can you enroll?_2` > 0 |
          `How many other people can you enroll?_2` > 0
      )
  ) %>%
  # AND, retain those who are comfortable/whose families are comfortable collecting medical samples at home
  filter(
    `Are you comfortable collecting medical samples at home? For this study, we will ask you to collect samples of your skin, blood, and saliva using devices we will supply. Click the following links to read more about the devices that we use: Blood drawing device Skin tape Microneedle patches Saliva collection kit` == "Yes" |
      `Are you comfortable collecting medical samples at home? For this study, we will ask you to collect samples of your skin, blood, and saliva using devices we will supply. Click the following links to read more about the devices that we use: Blood drawing device Skin tape Micro patches Saliva collection kit` == "Yes" |
      `Do you believe that your family members would also be comfortable collecting medical samples at home?` == "Yes"
  )

# compute new Referral IDs where existing ID left off
p1_recruiters_new <- p1_recruiters_new %>%
  # remove any duplicate new recruiters
  distinct(`First Name`, `Last Name`, `Birth year`, .keep_all = T) %>%
  # remove recruiters that already exist in `p1_recruiters_existing` (use name + birth year AND email + phone number as personal IDs)
  filter(
    !str_to_title(`First Name`) %in% str_to_title(p1_recruiters_existing$`First Name`) |
      !str_to_title(`Last Name`) %in% str_to_title(p1_recruiters_existing$`Last Name`) |
      !`Birth year` %in% p1_recruiters_existing$`Birth year` |
      !str_to_lower(Email) %in% str_to_lower(p1_recruiters_existing$Email) |
      !`Phone Number (no spaces or dashes):` %in% p1_recruiters_existing$`Phone Number (no spaces or dashes):`
  )


# new recruiters with very similar first + last name to an existing recruiter
p1_recruiters_new_dups <- p1_recruiters_new %>%
  # normalize First + Last names to remove all spaces and punctuation
  mutate(across(
    .cols = c(`First Name`, `Last Name`),
    .fns = ~ str_to_lower(str_replace_all(.x, "[\\s[:punct:]]+", ""))
  )) %>%
  # only get records from p1_recruiters_existing that have the same normalized 
  # First + Last name combo
  semi_join(
    y = (
      p1_recruiters_existing %>% mutate(across(
        .cols = c(`First Name`, `Last Name`),
        .fns = ~ str_to_lower(str_replace_all(.x, "[\\s[:punct:]]+", ""))
      )) %>% anti_join(y = p1_recruiters_new,
                       by = "p1_id")
    ),
    by = c("First Name", "Last Name")
  )


p1_recruiters_new <- p1_recruiters_new %>%
  # remove the new dup identified
  anti_join(y = p1_recruiters_new_dups,
            by = "p1_id") %>%
  mutate(`Referral ID` = max_ref_id + row_number())

# # init. the P1 recruiters
# p1_recruiters <- df_phase1 %>%
#   # retain those who completed form + live in US + gave consent
#   filter(
#     `Complete?_2` == "Complete",
#     `Do you live in the United States or Canada?` == "Yes",
#     `Do you consent to filling out your information and being contacted by our research team?` == "I consent"
#   ) %>%
#   filter(
#     # retain vitiligo patients who can recruit immediate family (Phase 1.1 + ipad)
#     (
#       `Would you be willing and able to recruit your siblings, children, or family members for this study?` == "Yes" |
#         `How many children can you enroll?` > 0 |
#         `How many siblings can you enroll?` > 0 |
#         `How many family members can you enroll?` > 0
#     ) |
#       # OR, retain healthy patients with vitiligo family members (Phase 1.1 + ipad)
#       (
#         `Do you have an affected family member?` == "Yes" |
#           `(choice=Child)` == "Checked" |
#           `(choice=Parent)` == "Checked" |
#           `(choice=Sibling)` == "Checked"
#       ) |
#       # OR, retain vitiligo patients who can recruit immediate family (Phase 1.2)
#       (
#         `Would you be willing and able to recruit your siblings, children, or parents for this study?` == "Yes" |
#           `How many children can you enroll?` > 0 |
#           `How many siblings can you enroll?` > 0 |
#           `How many parents can you enroll?` > 0
#       ) |
#       # OR, retain healthy patients who can recruit vitiligo family members (Phase 1.2)
#       (
#         `Do you have an affected family member and if so, would you be willing to recruit them for this study?` == "Yes" |
#           `How many children can you enroll?_2` > 0 |
#           `How many siblings can you enroll?_2` > 0 |
#           `How many parents can you enroll?_2` > 0
#       )
#   ) %>%
#   # AND, retain those who are comfortable/whose families are comfortable collecting medical samples at home
#   filter(
#     `Are you comfortable collecting medical samples at home? For this study, we will ask you to collect samples of your skin, blood, and saliva using devices we will supply. Click the following links to read more about the devices that we use: Blood drawing device Skin tape Microneedle patches Saliva collection kit` == "Yes" |
#       `Do you believe that your family members would also be comfortable collecting medical samples at home?` == "Yes"
#   )
# 
# # add new col to store referral ID
# p1_recruiters <- p1_recruiters %>%
#   mutate("Referral ID" = row_number()) %>%
#   select(p1_id, `Record ID`, form, `Referral ID`, everything())


# bind existing + new recruiters into new DF
p1_recruiters <- bind_rows(p1_recruiters_existing, p1_recruiters_new) %>%
  select(p1_id, `Record ID`, form, `Referral ID`, everything())

# # remove phase 1.2 ID 54 (p1_id 1173; Referral ID 329) -> indicated via email that she no longer wants to participate (Sept 12, 2023)
# p1_recruiters <- p1_recruiters %>%
#   filter(`Record ID` != 54 | form != "p1_second_round")

# subset all distinct Phase 1 records
p1_recruiters_distinct <- p1_recruiters %>%
  distinct(`First Name`, `Last Name`, `Birth year`, .keep_all = T)

# isolate all duplicate Phase 1 records
p1_recruiters_dup <- p1_recruiters %>%
  filter(!p1_id %in% p1_recruiters_distinct$p1_id)

# # remove excess Referral IDs from recruiters who were assigned multiple
# p1_recruiters <- p1_recruiters %>%
#   filter(!`Referral ID` %in% c(381, 416, 43, 334, 387))

