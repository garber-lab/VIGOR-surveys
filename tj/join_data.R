# join_data.R

# clear the current R environment
rm(list = ls())

# init. libraries
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here")
}
library(tidyverse)

########################## TIME OF LAST UPDATE #################################

# time last updated
last_updated <- now()

# convert to EST
last_updated_est <- with_tz(last_updated, "US/Eastern")

# format the minute value
minute <- minute(last_updated_est)

if (minute < 10) {
  minute <- str_c("0", minute)
}


last_updated_fmt <- str_glue("Accurate as of {date(last_updated_est)} @ {hour(last_updated_est)}:{minute} EST")
# print(last_updated_fmt)
################################################################################


# source in Phase 1 and Phase 2 REDCap data via API calls
# source("~/U01_PROSPECTIVE_VITILIGO/scripts/phase_1/phase1_api_call_v2.R")
# source("~/U01_PROSPECTIVE_VITILIGO/scripts/phase_2/phase2_api_call.R")
# source("../../phase_1/phase1_api_call_v2.R", chdir = T)
# source("phase2_api_call.R", chdir = T)

# set project root anchor by specifying the API call file location relative
# to current script
here::i_am("tj/phase1_api_call_v2.R")

# source in Phase I and II REDCap data via separate API calls
source(here::here("tj", "phase1_api_call_v2.R"))
source(here::here("tj", "phase2_api_call.R"))


# join Phase 1 data onto Phase 2 data using Referral ID as the join key
df_join <- p2_clean %>%
  # change data type to match Phase 1 Referral ID type
  mutate(`Referral ID` = as.integer(`Referral ID`)) %>%
  left_join(
    p1_recruiters,
    by = "Referral ID",             # join key
    suffix = c("_p2", "_p1")        # assign suffix to repeating column names (_p2 for Phase 2 cols; _p1 for Phase 1 cols)
  )


# standardize the case for every text field
df_join <- df_join %>%
  mutate(
    # use Title Case 
    across(
      .cols = c(
        `First Name_p1`,
        `Last Name_p1`,
        `Street Address_p1`,
        City_p1,
        State_p1,
        Province_p1,
        `First Name_p2`,
        `Last Name_p2`,
        `Street Address_p2`,
        City_p2,
        State_p2,
        Province_p2
      ),
      .fns = ~ str_to_title(.x)
    ), 
    # use lower case
    across(.cols = c(Email_p1, Email_p2),
           .fns = ~ str_to_lower(.x))
  )

# abbreviate streets
df_join <- df_join %>%
  mutate(
    across(
      .cols = c(`Street Address_p1`, `Street Address_p2`),
      .fns = ~ case_when(
        str_detect(string = .x, pattern = "Avenue") ~ 
          str_replace_all(string = .x, pattern = "Avenue", replacement = "Ave"),
        str_detect(string = .x, pattern = "Road") ~
          str_replace_all(string = .x, pattern = "Road", replacement = "Rd"),
        str_detect(string = .x, pattern = "Street") ~
          str_replace_all(string = .x, pattern = "Street", replacement = "St"),
        str_detect(string = .x, pattern = "Drive") ~
          str_replace_all(string = .x, pattern = "Drive", replacement = "Dr"),
        str_detect(string = .x, pattern = "Lane") ~
          str_replace_all(string = .x, pattern = "Lane", replacement = "Ln"),
        str_detect(string = .x, pattern = "Place") ~
          str_replace_all(string = .x, pattern = "Place", replacement = "Pl"),
        str_detect(string = .x, pattern = "Circle") ~
          str_replace_all(string = .x, pattern = "Circle", replacement = "Cir"),
        str_detect(string = .x, pattern = "Court") ~
          str_replace_all(string = .x, pattern = "Court", replacement = "Ct"),
        str_detect(string = .x, pattern = "Terrace") ~
          str_replace_all(string = .x, pattern = "Terrace", replacement = "Ter"),
        .default = .x
      )
    )
  )


# # init. DF of duplicate records
# df_dup <- df_join %>%
#   # get records where first/last name AND birth year are identical
#   filter(
#     `First Name_p1` == `First Name_p2`,
#     `Last Name_p1` == `Last Name_p2`,
#     `Birth year_p1` == `Birth year_p2`
#   ) %>% 
#   # create cols to store info for identical fields
#   mutate(
#     same_name = case_when(
#       `First Name_p1` == `First Name_p2` &
#         `Last Name_p1` == `Last Name_p2` ~ "yes",
#       .default = "no"
#     ),
#     same_birth_yr = case_when(`Birth year_p1` == `Birth year_p2` ~ "yes", .default = "no"),
#     same_email = case_when(Email_p1 == Email_p2 ~ "yes", .default = "no"),
#     same_address = case_when(`Street Address_p1` == `Street Address_p2` ~ "yes", .default = "no"),
#     same_city = case_when(City_p1 == City_p2 &
#                             State_p1 == State_p2 ~ "yes", .default = "no"),
#     same_affected = case_when(
#       `Do you have vitiligo?_p1` == `Do you have vitiligo?_p2` ~ "yes",
#       .default = "no"
#     )
#   ) %>%
#   filter(
#     `First Name_p1` == `First Name_p2`, `Last Name_p1` == `Last Name_p2`, 
#     `Birth year_p1` == `Birth year_p2`, Email_p1 == Email_p2, 
#     `Street Address_p1` == `Street Address_p2`, City_p1 == City_p2, 
#     `Do you have vitiligo?_p1` == `Do you have vitiligo?_p2`
#   )


# distinct phase 2 records (remove identical records from Phase 2)
join_distinct <- df_join %>%
  distinct(`First Name_p2`, `Last Name_p2`, `Birth year_p2`, .keep_all = T)

# every duplicate Phase 2 record
p2_dups <- df_join %>%
  filter(!`Record ID_p2` %in% join_distinct$`Record ID_p2`)


# self-recruiters: where phase 1 recruiters fills out phase 2 form (joined data)
p1_dups <- join_distinct %>%
  filter(
    `First Name_p1` == `First Name_p2` &
      `Last Name_p1` == `Last Name_p2` &
      `Birth year_p1` == `Birth year_p2`
  )


# non-identical phase 2 records referred by self-recruiters
p1_dups_other_rec <- join_distinct %>%
  filter(p1_id %in% p1_dups$p1_id &
           !`Record ID_p2` %in% p1_dups$`Record ID_p2`)


# solo self-recruiters: all self-recruiters who only referred themself and nobody else
p1_dups_solo <- p1_dups %>%
  filter(!p1_id %in% p1_dups_other_rec$p1_id)


# reformat solo self-recruiters (remove phase 1 suffix from cols)
p1_dups_solo_v2 <- p1_dups_solo %>%
  select(p1_id:form_p1,
         `Referral ID`,
         `Survey Identifier_p1`:`Are you comfortable collecting medical samples at home? For this study, we will ask you to collect samples of your skin, blood, and saliva using devices we will supply. Click the following links to read more about the devices that we use: Blood drawing device Skin tape Micro patches Saliva collection kit_p1`) %>%
  rename_with(.fn = ~ str_replace(.x, "_p1", ""),
              .cols = str_subset(colnames(.), ".+_p1$"))


# all Phase 1 records who didn't recruit anybody (includes solo self-recruiters) (only among existing recruiters)
p1_no_refs <- p1_recruiters_existing %>%
  # bind_rows(p1_dups_solo_v2) %>%
  distinct(`First Name`, `Last Name`, Sex, `Birth year`, .keep_all = T) %>%
  filter(!`Referral ID` %in% join_distinct$`Referral ID`) %>%
  bind_rows(p1_dups_solo_v2)


# joined phase 1-phase 2 records without any of the duplicates 
join_distinct_clean <- join_distinct %>%
  filter(!`Record ID_p2` %in% p1_dups$`Record ID_p2`)





############# Handle Phase 1 duplicate Recruiters ################

# init. distinct records from Phase 1 recruiters
dist_recruiters <- p1_recruiters_existing %>%
  distinct(str_to_title(`First Name`), 
           str_to_title(`Last Name`), 
           `Birth year`, 
           .keep_all = T)

# find the "duplicate" records that are missing from dist_recruiters
dup_recruiters <- p1_recruiters_existing %>%
  filter(!p1_id %in% dist_recruiters$p1_id)

# combine original + duplicate records of each identical recruiter
p1_recruiters_existing_dups <- p1_recruiters_existing %>%
  filter(
    str_to_title(`First Name`) %in% str_to_title(dup_recruiters$`First Name`),
    str_to_title(`Last Name`) %in% str_to_title(dup_recruiters$`Last Name`),
    `Birth year` %in% dup_recruiters$`Birth year`
  )


# subset recruiters who are active among duplicate recruiters (successfully referred somebody)
p1_recruiters_existing_dups_active <- p1_recruiters_existing %>%
  filter(
    `Referral ID` %in% join_distinct_clean$`Referral ID`
  )

# subset inactive recruiters among duplicates (haven't referred yet)
p1_recruiters_existing_dups_inactive <- p1_recruiters_existing_dups %>%
  filter(
    !`Referral ID` %in% join_distinct_clean$`Referral ID`
  )

# for people with multiple Ref IDs that are ALL inactive, still keep them as eligible recruiters with the most recent Ref ID
# remove all Ref IDs from the system except the most recent Ref ID
p1_recruiters_existing_dups_inactive_inelligible <- p1_recruiters_existing_dups_inactive %>%
  filter(
    !str_to_title(`First Name`) %in% str_to_title(p1_recruiters_existing_dups_active$`First Name`) |
      !str_to_title(`Last Name`) %in% str_to_title(p1_recruiters_existing_dups_active$`Last Name`) |
      !`Birth year` %in% p1_recruiters_existing_dups_active$`Birth year`
  ) %>%
  group_by(`First Name`, `Last Name`, `Birth year`) %>%
  filter(`Referral ID` != max(`Referral ID`)) %>%
  ungroup()

# every "duplicate" recruiter and the corresponding Referral ID to MUTE
p1_recruiters_existing_dups_muted <- p1_recruiters_existing_dups_inactive %>%
  filter(
    `First Name` %in% p1_recruiters_existing_dups_active$`First Name` &
      `Last Name` %in% p1_recruiters_existing_dups_active$`Last Name` &
      `Birth year` %in% p1_recruiters_existing_dups_active$`Birth year`
  ) %>%
  # select(p1_id,
  #        `Record ID`,
  #        form,
  #        `First Name`,
  #        `Last Name`,
  #        `Birth year`,
  #        `Referral ID`) %>%
  bind_rows(p1_recruiters_existing_dups_inactive_inelligible) %>%
  arrange(`Last Name`, `First Name`, `Referral ID`)

# remove all inactive referral IDs from use
p1_recruiters_existing_clean <- p1_recruiters_existing %>%
  filter(
    !`Referral ID` %in% p1_recruiters_existing_dups_muted$`Referral ID`
  )

# combine existing + new recruiters w/o any duplicates
p1_recruiters_clean <- bind_rows(p1_recruiters_existing_clean, p1_recruiters_new) %>%
  select(p1_id, `Record ID`, form, `Referral ID`, everything())



############ Handle Phase 1 duplicate non-recruiters ################


# init. all non-recruiters in Phase 1 (must have a name)
non_recruiters <- df_phase1 %>%
  # retain all records with a name but no Referral ID
  filter(`First Name` != "", `Last Name` != "", `Referral ID` == "")

# obtain all distinct records of non-recruiters in Phase 1 (distinct name + birth year)
dist_non_recruiters <- non_recruiters %>%
  distinct(`First Name`, `Last Name`, `Birth year`, .keep_all = T)

# cross-check distinct records by also checking distinct name + city
dist_non_recruiters_v2 <- dist_non_recruiters %>%
  distinct(`First Name`, `Last Name`, City, .keep_all = T)

# init. all duplicate records of non-recruiters
dup_non_recruiters <- non_recruiters %>%
  filter(!p1_id %in% dist_non_recruiters_v2$p1_id)


########################## Preprocessing #############################


# full Phase 1 data with recruiters too
p1_all <- bind_rows(
  # phase 1 records who AREN'T recruiters
  df_phase1 %>% 
    filter(
      `Referral ID` == "",                    # no referral ID
      !p1_id %in% p1_recruiters_new$p1_id     # record isn't part of new recruiter batch
    ) %>% 
    select(colnames(p1_recruiters_clean)),
  # phase 1 existing + new recruiters
  p1_recruiters_clean %>% mutate(`Referral ID` = as.character(`Referral ID`)),
  # # phase 1 new recruiters
  # p1_recruiters_new %>% mutate(`Referral ID` = as.character(`Referral ID`))
) %>%
  arrange(p1_id)



# invalid responses
p1_all_invalid <- p1_all %>%
  filter(
    `Referral ID` == "",
    # `Do you consent to filling out your information and being contacted by our research team?` != "I consent" |
    #   `Do you live in the United States or Canada?` != "Yes" |
    `First Name` == "" | `Last Name` == "" | (`Phone Number (no spaces or dashes):` == "" & Email == "")
  )



# init. DF with all active recruiters (have a referral in Phase 2)
p1_join <- join_distinct_clean %>%
  # remove "_p1" suffix
  rename_with(
    .fn = ~ str_replace_all(.x, "_p1$", ""),
    .cols = colnames(.)
  ) %>%
  select(all_of(colnames(p1_all))) %>%
  distinct(`First Name`, `Last Name`, `Birth year`, .keep_all = T) %>%
  mutate(role = "recruiter") %>%
  select(p1_id:form, role, everything())


# init. DF with all phase 2 referrals
p2_join <- join_distinct_clean %>%
  # remove "_p2" suffix
  rename_with(
    .fn = ~ str_replace_all(.x, "_p2$", ""),
    .cols = colnames(.)
  ) %>%
  select(all_of(colnames(p2_clean))) %>%
  mutate(role = "referral") %>%
  select(`Record ID`:form, role, everything())


# join all active recruiters + their referrals into 1 DF
all_join <- bind_rows(p1_join, p2_join) %>%
  mutate(
    `VIGOR ID` = case_when(
      (`VIGOR ID` == "" | is.na(`VIGOR ID`)) & role == "recruiter" ~ str_c("VIGOR", `Referral ID`, "1", sep = "_"),
      .default = `VIGOR ID`
    ),
    `VIGOR ID` = case_when(
      `Do you have vitiligo?` == "Yes" & role == "recruiter" ~ str_c(`VIGOR ID`, "v", sep = "_"),
      .default = `VIGOR ID`
    ),
    age_yr_end = year(today()) - `Birth year`
  ) %>%
  # automatically compute and correct Family Size from what's in RedCap
  mutate(
    `Family Size` = n(),
    .by = `Referral ID`
    ) %>% 
  select(p1_id:role, `VIGOR ID`, `Referral ID`:`Birth year`, age_yr_end, everything(), -role)





# init. modified `join_distinct_clean` DF that categorizes families as "priority" or "non-priority"
joined_status <- join_distinct_clean %>%
  mutate(
    fam_size = n() + 1, .by = `Referral ID`
  ) %>%
  mutate(
    age_p1 = year(today()) - `Birth year_p1`,
    age_p2 = year(today()) - `Birth year_p2`,
    status = case_when(
      fam_size >= 3 ~ "priority",
      fam_size == 2 & age_p2 <= 30 & `Who referred you to this study?` %in% c("Parent", "Sibling") ~ "priority",
      fam_size <= 2 & (age_p2 > 30 | !`Who referred you to this study?` %in% c("Parent", "Sibling")) ~ "nonpriority"
    )
  ) %>%
  arrange(`Referral ID`) %>%
  select(`Record ID_p2`:`Referral ID`, fam_size, status, everything())



########## Remove existing Phase II Records from new Phase I Founders ##########

# p1_id of the most recently added founder
last_founder_p1_id <- p1_recruiters_existing_clean %>% slice_tail() %>% pull(p1_id)

# DF of all new P1 submissions to manually check if they're eligible to become a founder
expected_new_recruiters <- p1_all %>%
  filter(
    # get all P1 records submitted after the record with the last official Referral ID
    p1_id > last_founder_p1_id | 
      # OR all P1 records who appear eligible to become founders but were submitted before the last official Referral ID
      p1_id %in% p1_recruiters_new$p1_id
  ) %>%
  filter(
    `Do you live in the United States or Canada?` == "Yes",
    `Do you consent to filling out your information and being contacted by our research team?` == "I consent"
  ) %>%
  # retain those who completed form, OR completed all relevant columns
  filter((
    `Complete?_2` == "Complete" | (
      `First Name` != "" &
        `Last Name` != "" &
        `Do you have vitiligo?` != "" &
        (`Street Address` != "" |
           City != "") &
        # (State != "" | Province != "") &
        # State != "" &
        # TODO: Add province / Canada location stuff!!!
        (`Phone Number (no spaces or dashes):` != "" |
           Email != "")
    )
  ) & State != "") %>%
  mutate(age = year(last_updated_est) - `Birth year`) %>%
  rename(
    has_vit = `Do you have vitiligo?`,
    phone = `Phone Number (no spaces or dashes):`,
    can_recruit_vit = `Would you be willing and able to recruit your family members for this study?`,
    can_recruit_healthy = `Do you have an affected family member and if so, would you be willing to recruit them for this study?`,
    comf_sampling_self = `Are you comfortable collecting medical samples at home? For this study, we will ask you to collect samples of your skin, blood, and saliva using devices we will supply. Click the following links to read more about the devices that we use: Blood drawing device Skin tape Micro patches Saliva collection kit`,
    comf_sampling_fam = `Do you believe that your family members would also be comfortable collecting medical samples at home?`
  ) %>%
  select(
    `Survey Timestamp`,
    p1_id:`Referral ID`,
    `First Name`,
    `Last Name`,
    `Birth year`,
    age,
    Email,
    phone,
    `Street Address`,
    Apt,
    City,
    State,
    Province,
    has_vit,
    can_recruit_vit,
    str_subset(colnames(.), "How many .+ can you enroll\\?$"),
    can_recruit_healthy,
    str_subset(colnames(.), "How many .+ can you enroll\\?_2"),
    comf_sampling_self,
    comf_sampling_fam,
    -form
  )


# col to determine why phase I pt may be ineligible
expected_new_recruiters_context <- expected_new_recruiters %>%
  mutate(
    founder_eligibility = case_when(
      # has vit, unwilling to recruit
      has_vit == "Yes" &
        can_recruit_vit != "Yes" &
        `How many children can you enroll?` %in% c(0, "", NA) &
        `How many siblings can you enroll?` %in% c(0, "", NA) &
        `How many family members can you enroll?` %in% c(0, "", NA) &
        `How many parents can you enroll?` %in% c(0, "", NA) &
        `How many cousins can you enroll?` %in% c(0, "", NA) &
        `How many nieces and nephews can you enroll?` %in% c(0, "", NA) &
        `How many other people can you enroll?` %in% c(0, "", NA) ~ F,
      # healthy, unwilling to recruit
      has_vit == "No" &
        can_recruit_healthy != "Yes" &
        `How many children can you enroll?_2` %in% c(0, "", NA) &
        `How many siblings can you enroll?_2` %in% c(0, "", NA) &
        `How many parents can you enroll?_2` %in% c(0, "", NA) &
        `How many cousins can you enroll?_2` %in% c(0, "", NA) &
        `How many nieces and nephews can you enroll?_2` %in% c(0, "", NA) &
        `How many other people can you enroll?_2` %in% c(0, "", NA) ~ F,
      # either they or their fam not comf with self-sampling
      comf_sampling_self != "Yes" & comf_sampling_fam != "Yes" ~ F, 
      # duplicate entry
      `Referral ID` %in% c("", NA) ~ F,
      .default = T
    ),
    eligibility_reason = case_when(
      has_vit == "Yes" &
        can_recruit_vit != "Yes" &
        `How many children can you enroll?` %in% c(0, "", NA) &
        `How many siblings can you enroll?` %in% c(0, "", NA) &
        `How many family members can you enroll?` %in% c(0, "", NA) &
        `How many parents can you enroll?` %in% c(0, "", NA) &
        `How many cousins can you enroll?` %in% c(0, "", NA) &
        `How many nieces and nephews can you enroll?` %in% c(0, "", NA) &
        `How many other people can you enroll?` %in% c(0, "", NA) ~ "unwilling to recruit",
      has_vit == "No" &
        can_recruit_healthy != "Yes" &
        `How many children can you enroll?_2` %in% c(0, "", NA) &
        `How many siblings can you enroll?_2` %in% c(0, "", NA) &
        `How many parents can you enroll?_2` %in% c(0, "", NA) &
        `How many cousins can you enroll?_2` %in% c(0, "", NA) &
        `How many nieces and nephews can you enroll?_2` %in% c(0, "", NA) &
        `How many other people can you enroll?_2` %in% c(0, "", NA) ~ "unwilling to recruit",
      comf_sampling_self != "Yes" & comf_sampling_fam != "Yes" ~ "uncomf. sampling",
      `Referral ID` %in% c("", NA) ~ "likely duplicate entry",
      .default = NA
    )
  ) %>%
  select(founder_eligibility, eligibility_reason, everything())


# check if new recruiter already exists in `p2_join`
expected_new_recruiters_in_p2 <- expected_new_recruiters_context %>%
  # normalize case for First + Last names
  mutate(across(.cols = c(`First Name`, `Last Name`),
                .fns = ~str_to_lower(str_replace_all(.x, "[\\s[:punct:]]+", "")))) %>%
  semi_join(
    y = (p2_join %>% mutate(across(
      .cols = c(`First Name`, `Last Name`),
      .fns = ~ str_to_lower(str_replace_all(.x, "[\\s[:punct:]]+", ""))
    ))),
    by = c("First Name", "Last Name")
  )


# check if a new founder already exists in p1_all with a slightly different name (normalize for case and punctuation)
expected_new_recruiters_dups_in_p1 <- expected_new_recruiters_context %>%
  # normalize First + Last names to remove all spaces and punctuation
  mutate(across(
    .cols = c(`First Name`, `Last Name`),
    .fns = ~ str_to_lower(str_replace_all(.x, "[\\s[:punct:]]+", ""))
  )) %>%
  # keep expected new recruiters whose name exists earlier in p1_all
  semi_join(
    y = (
      p1_all %>%
        # normalize First + Last names
        mutate(across(
          .cols = c(`First Name`, `Last Name`),
          .fns = ~ str_to_lower(str_replace_all(.x, "[\\s[:punct:]]+", ""))
        )) %>%
        # keep records with same names as those in expected new recruiters
        semi_join(
          y = (expected_new_recruiters_context %>% mutate(across(
            .cols = c(`First Name`, `Last Name`),
            .fns = ~ str_to_lower(str_replace_all(.x, "[\\s[:punct:]]+", ""))
          ))),
          by = c("First Name", "Last Name")
        ) %>%
        # remove the expected new recruiters' records to search for earlier p1_all records
        anti_join(y = expected_new_recruiters_context,
                  by = "p1_id")
    ),
    by = c("First Name", "Last Name")
  )

# expected new recruiters who are eligible to become founders
real_new_recruiters <- expected_new_recruiters_context %>%
  filter(
    !`Record ID` %in% expected_new_recruiters_in_p2$`Record ID` &
      !`Record ID` %in% expected_new_recruiters_dups_in_p1$`Record ID`
  )

# expected recruiters who aren't eligible to become founders
ineligible_new_recruiters <- expected_new_recruiters_context %>%
  anti_join(y = real_new_recruiters,
            by = "p1_id")


expected_new_recruiters_context <- expected_new_recruiters_context %>%
  mutate(
    # mark records that are in Phase II (likely)
    founder_eligibility = if_else(p1_id %in% expected_new_recruiters_in_p2$p1_id, F, founder_eligibility),
    eligibility_reason = if_else(p1_id %in% expected_new_recruiters_in_p2$p1_id, "likely recruited in Phase II", eligibility_reason),
    
    # mark records that are already in Phase I (likely)
    founder_eligibility = if_else(p1_id %in% expected_new_recruiters_dups_in_p1$p1_id, F, founder_eligibility),
    eligibility_reason = if_else(p1_id %in% expected_new_recruiters_dups_in_p1$p1_id, "likely duplicate entry", eligibility_reason),
    `Referral ID` = if_else(p1_id %in% expected_new_recruiters_dups_in_p1$p1_id, "", `Referral ID`)
  )

# update p1_all so Referral IDs won't carry over for ineligible new recruiters
p1_all <- p1_all %>%
  mutate(
    `Referral ID` = case_when(p1_id %in% ineligible_new_recruiters$p1_id ~ "",
                              .default = `Referral ID`)
  )


# init. the lowest Referral ID available among expected new recruiters
min_ref_id_exp <- min(as.integer(expected_new_recruiters_context$`Referral ID`))

# filter out ineligible founders and re-increment Referral ID
expected_new_recruiters_updated <- expected_new_recruiters_context %>%
  filter(founder_eligibility) %>%
  mutate(`Referral ID` = as.character(min_ref_id_exp + row_number() - 1))


# update `p1_all` to have correct Referral IDs
p1_all <- p1_all %>%
  left_join(
    y = expected_new_recruiters_updated %>% select(p1_id, refID_elig=`Referral ID`),
    by = "p1_id"
  ) %>%
  # re-increment Referral ID
  mutate(`Referral ID` = case_when(!is.na(refID_elig) ~ refID_elig,
                                   .default = `Referral ID`)) %>%
  select(-refID_elig)

# update the new Phase I Founders
p1_recruiters_new <- p1_all %>%
  # filter for new Founders (Referral ID should be updated already)
  semi_join(p1_recruiters_new, "p1_id") %>%
  mutate(`Referral ID` = as.integer(`Referral ID`)) %>%
  filter(!is.na(`Referral ID`))


################# Remove Phase 1 records that might belong to existing families ###########

p1_all_minus_extra_fam <- p1_all %>%
  filter(
    !p1_id %in% c(
      1080, 1143, 1901, 2539,
      321, 1634, 1647,
      193, 2733,
      21, 388,
      214, 364,
      1547, 1567,
      1347, 1351,
      2374, 2401, 2403,
      430, 626,
      # 378, 2794,
      790, 884, 948, 2630,
      244, 1894,
      1564, 2668,
      756, 2530, 2534, 2582,
      1114, 1665,
      476, 1569, 1679, 2753,
      1867, 1887, 1993, 2006,
      # 764, 909,
      20, 1514, 1581,
      # 58, 742,
      608, 1683,
      789, 2012,
      2552, 2568,
      # 1711, 2110,
      502, 628, 1421, 2752,
      # 48, 
      1156, 2052, 2294, 2553,
      965, 1134, 1319,
      # 293, 1078,
      # 478, 
      524, 1110, 1509, 2606, 2692,
      304, 1701,
      493, 1522, 1678,
      63, 2583,
      479, 1722, 2200, 2669,
      2017, 2061,
      2758, 2773,
      333, 2195,
      2176, 2745,
      410, 1783, 2222,
      940, 1146,
      123, 235,
      1606, 2050
    )
  )








########################## RACE ################################################


# pivot the race 
p1_race <- p1_all %>%
  # for records with no `Race ...` col checked, mark them as "Prefer not to answer"
  mutate(
    `Race (choice=Prefer not to answer)` = case_when(
      `Race (choice=American Indian or Alaskan Native)` == "Unchecked" &
        `Race (choice=Asian {race_asian})` == "Unchecked" &
        `Race (choice=Black or African American)` == "Unchecked" &
        `Race (choice=Hawaiian or Pacific Islander)` == "Unchecked" &
        `Race (choice=White)` == "Unchecked" &
        `Race (choice=Other {otherrace})` == "Unchecked" &
        `Race (choice=Prefer not to answer)` == "Unchecked" ~ "Checked",
      .default = `Race (choice=Prefer not to answer)`
    )
  ) %>%
  pivot_longer(
    # "Race" not followed by " Asian"
    cols = colnames(.)[which(str_detect(colnames(.), "Race(?! Asian).*$"))],
    names_to = "race",
    names_pattern = "(?<=\\=)([\\w\\s]+)",
    values_to = "race_val"
  ) %>%
  filter(race_val == "Checked") %>%
  mutate(across(race, ~str_squish(.x))) %>%
  select(p1_id:Sex, race, `other race`:`How many cousins can you enroll?_2`)

# rename race designations
p1_race_clean <- p1_race %>%
  # convert race names
  mutate(
    race = case_when(
      race == "White" ~ "white",
      race == "Asian" ~ "asian",
      race == "Prefer not to answer" ~ NA_character_,
      race == "Other" ~ "other",
      race == "Black or African American" ~ "black",
      race == "Latinx" ~ "latinx",
      race == "Hawaiian or Pacific Islander" ~ "pacific_islander",
      race == "American Indian or Alaskan Native" ~ "native",
      .default = race
    )
  ) %>%
  # remove duplicate records (people with multiple races) -> creates race hierarchy that favors minority enrollment
  distinct(p1_id, .keep_all = T)

# add "other" races to race col
p1_race_clean_v2 <- p1_race_clean %>%
  mutate(
    race = case_when(
      str_detect(`other race`, "(S|s)pan") | Ethnicity == "Hispanic or Latino" ~ "latinx",
      str_detect(`other race`, "(M|m).xic") ~ "latinx",
      str_detect(`other race`, "(P|p)uert") ~ "latinx", 
      str_detect(`other race`, "(D|d)ominica") ~ "latinx",
      str_detect(`other race`, "(A|a)fr") ~ "black",
      str_detect(`other race`, "(V|v)erde") ~ "black",
      str_detect(`other race`, "[:alpha:]+(ian|IAN)") ~ "asian",
      str_detect(`other race`, "stan") ~ "asian",
      str_detect(`other race`, "ese") ~ "asian", 
      str_detect(`other race`, "(M|m)editerranean") ~ "asian",
      str_detect(`other race`, "(B|b)angladesh") ~ "asian",
      str_detect(`other race`, "(M|m)iddle") ~ "asian",
      str_detect(`other race`, "(A|a)rab") ~ "asian",
      str_detect(`other race`, "(E|e)urope") ~ "white",
      str_detect(`other race`, "(G|g)reek") ~ "white",
      `other race` == "" & race == "other" ~ NA_character_,
      .default = race
    )
  ) 


p2_race <- join_distinct_clean %>%
  mutate(
    `Race (choice=Prefer not to answer)_p2` = case_when(
      `Race (choice=American Indian or Alaskan Native)_p2` == "Unchecked" &
        `Race (choice=Asian {race_asian})_p2` == "Unchecked" &
        `Race (choice=Black or African American)_p2` == "Unchecked" &
        `Race (choice=Hawaiian or Pacific Islander)_p2` == "Unchecked" &
        `Race (choice=White)_p2` == "Unchecked" &
        `Race (choice=Other {otherrace})_p2` == "Unchecked" &
        `Race (choice=Prefer not to answer)_p2` == "Unchecked" ~ "Checked",
      .default = `Race (choice=Prefer not to answer)_p2`
    )
  ) %>%
  pivot_longer(
    cols = str_subset(colnames(.), "Race(?! Asian).*_p2$"),
    names_to = "race_p2",
    names_pattern = "(?<=\\=)([\\w\\s]+)",
    values_to = "race_p2_val"
  ) %>%
  filter(
    race_p2_val == "Checked"
  ) %>%
  mutate(across(race_p2, ~str_squish(.x))) %>%
  mutate(
    race_p2 = case_when(
      race_p2 == "White" ~ "white",
      race_p2 == "Asian" ~ "asian",
      race_p2 == "Prefer not to answer" ~ NA_character_,
      race_p2 == "Other" ~ "other",
      race_p2 == "Black or African American" ~ "black",
      race_p2 == "Latinx" ~ "latinx",
      race_p2 == "Hawaiian or Pacific Islander" ~ "pacific_islander",
      race_p2 == "American Indian or Alaskan Native" ~ "native",
      .default = race_p2
    )
  ) %>%
  # select(`Record ID_p2`, race_p2, race_p2_val) %>%
  distinct(`Record ID_p2`, .keep_all = T)


# add "other" races to race col
p2_race_clean <- p2_race %>%
  mutate(
    # process "other" races for Referrals
    race_p2 = case_when(
      str_detect(`other race_p2`, "(S|s)pan") | Ethnicity_p2 == "Hispanic or Latino" ~ "latinx",
      str_detect(`other race_p2`, "(M|m).xic") ~ "latinx",
      str_detect(`other race_p2`, "(P|p)uert") ~ "latinx",
      str_detect(`other race_p2`, "(D|d)ominica") ~ "latinx",
      str_detect(`other race_p2`, "(L|l)atin") ~ "latinx",
      str_detect(`other race_p2`, "(A|a)fr") ~ "black",
      str_detect(`other race_p2`, "(V|v)erde") ~ "black",
      str_detect(`other race_p2`, "[:alpha:]+(ian|IAN)") ~ "asian",
      str_detect(`other race_p2`, "[:alpha:]+stan") ~ "asian",
      str_detect(`other race_p2`, "[:alpha:]+ese") ~ "asian",
      str_detect(`other race_p2`, "(M|m)editerranean") ~ "asian",
      str_detect(`other race_p2`, "(B|b)angladesh") ~ "asian",
      str_detect(`other race_p2`, "(M|m)iddle") ~ "asian",
      str_detect(`other race_p2`, "(A|a)rab") ~ "asian",
      str_detect(`other race_p2`, "(L|l)anka") ~ "asian",
      str_detect(`other race_p2`, "(E|e)urope") ~ "white",
      str_detect(`other race_p2`, "(G|g)reek") ~ "white",
      `other race_p2` == "" & race_p2 == "other" ~ NA_character_,
      .default = race_p2
    )
  )



all_join_race <- all_join %>% 
  # indicate Race as "Prefer not to answer" if everything is unchecked
  mutate(
    `Race (choice=Prefer not to answer)` = case_when(
      `Race (choice=American Indian or Alaskan Native)` == "Unchecked" &
        `Race (choice=Asian {race_asian})` == "Unchecked" &
        `Race (choice=Black or African American)` == "Unchecked" &
        `Race (choice=Hawaiian or Pacific Islander)` == "Unchecked" &
        `Race (choice=White)` == "Unchecked" &
        `Race (choice=Other {otherrace})` == "Unchecked" &
        `Race (choice=Prefer not to answer)` == "Unchecked" ~ "Checked",
      .default = `Race (choice=Prefer not to answer)`
    )
  ) %>%
  # pivot race values (non-asian races)
  pivot_longer(
    cols = str_subset(colnames(.), "Race(?! Asian)"),
    names_to = "race",
    names_pattern = "(?<=\\=)([\\w\\s]+)",
    values_to = "race_val"
  ) %>%
  filter(race_val == "Checked") %>%
  mutate(race = str_squish(race)) %>%
  # remove duplicate records with multiple races -> naturally removes "white" first
  distinct(`Record ID`, form, .keep_all = T)


all_join_race_v2 <- all_join_race %>%
  mutate(
    race = case_when(
      str_detect(`other race`, "(S|s)pan") | Ethnicity == "Hispanic or Latino" ~ "Latinx",
      str_detect(`other race`, "(M|m).xic") ~ "Latinx",
      str_detect(`other race`, "(P|p)uert") ~ "Latinx",
      str_detect(`other race`, "(D|d)ominica") ~ "Latinx",
      str_detect(`other race`, "(L|l)atin") ~ "Latinx",
      str_detect(`other race`, "(C|c)uba") ~ "Latinx",
      str_detect(`other race`, "(A|a)fr") ~ "Black or African American",
      str_detect(`other race`, "(V|v)erde") ~ "Black or African American",
      str_detect(`other race`, "[:alpha:]+(ian|IAN)") ~ "Asian",
      str_detect(`other race`, "[:alpha:]+stan") ~ "Asian",
      str_detect(`other race`, "[:alpha:]+ese") ~ "Asian", 
      str_detect(`other race`, "(M|m)editerranean") ~ "Asian",
      str_detect(`other race`, "(B|b)angladesh") ~ "Asian",
      str_detect(`other race`, "(M|m)iddle") ~ "Asian",
      str_detect(`other race`, "(A|a)rab") ~ "Asian",
      str_detect(`other race`, "(L|l)anka") ~ "Asian",
      str_detect(`other race`, "(E|e)urope") ~ "White",
      str_detect(`other race`, "(G|g)reek") ~ "White",
      `other race` == "" & race == "Other" ~ "Prefer not to answer",
      .default = race
    )
  )



