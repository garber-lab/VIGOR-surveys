# load libs
library(tidyverse)

# source in `df_join`
# source("~/U01_PROSPECTIVE_VITILIGO/scripts/phase_2/join_data.R")
source("join_data.R", chdir = T)

# pivot both Phase 1 + 2 race together (exclude Asian/Other races)
race_distinct <- join_distinct_clean %>%
  pivot_longer(
    cols = colnames(.)[which(str_detect(colnames(.), "Race(?! Asian).+_p1$"))],
    names_to = "race_p1",
    names_pattern = "(?<=\\=)([\\w\\s]+)",
    values_to = "race_val_p1"
  ) %>%
  filter(race_val_p1 == "Checked") %>%
  pivot_longer(
    cols = colnames(.)[which(str_detect(colnames(.), "Race(?! Asian).+_p2$"))],
    names_to = "race_p2",
    names_pattern = "(?<=\\=)([\\w\\s]+)",
    values_to = "race_val_p2"
  ) %>%
  mutate(
    across(.cols = c(race_p1, race_p2),
           .fns = ~ str_squish(.x))
  ) %>%
  filter(race_val_p2 == "Checked") %>%
  select(`Record ID_p2`:Sex_p2, race_p2,
         `other race_p2`:Sex_p1, race_p1,
         `other race_p1`:`How many cousins can you enroll?_2`)

# # pivot both Phase 1 + 2 race together (exclude Asian/Other races)
# df_race <- df_join %>%
#   pivot_longer(
#     cols = colnames(df_join)[which(str_detect(colnames(df_join), "Race(?! Asian).+_p1$"))],
#     names_to = "race_p1",
#     names_pattern = "(?<=\\=)([\\w\\s]+)",        # all words/spaces preceded by "="
#     values_to = "race_val_p1"
#   ) %>%
#   filter(race_val_p1 == "Checked") %>%
#   # select(`Record ID_p1`, `Referral ID`, race_p1, race_val_p1)
#   pivot_longer(
#     cols = colnames(df_join)[which(str_detect(colnames(df_join), "Race(?! Asian).+_p2$"))],
#     names_to = "race_p2",
#     names_pattern = "(?<=\\=)([\\w\\s]+)",        # all words/spaces preceded by "="
#     values_to = "race_val_p2"
#   ) %>%
#   mutate(
#     across(
#       .cols = c(race_p1, race_p2),
#       .fns = ~ str_squish(.x)
#     )
#   ) %>%
#   filter(race_val_p2 == "Checked") %>%
#   # select(p1_id, `Referral ID`, race_p1, `Record ID_p2`, race_p2)
#   select(`Record ID_p2`:Sex_p2, race_p2, 
#          `other race_p2`:Sex_p1, race_p1, 
#          `other race_p1`:`How many parents can you enroll?_2`)

# init. factors to rename race vals
race_fac_p1 <- factor(
  race_distinct$race_p1,
  levels = c("White", "Asian", "Prefer not to answer",
             "Other", "Black or African American",
             "Latinx", "Hawaiian or Pacific Islander",
             "American Indian or Alaskan Native"),
  labels = c("white", "asian", NA,
             "other", "black", 
             "latinx", "pacific_islander",
             "native")
)

race_fac_p2 <- factor(
  race_distinct$race_p2,
  levels = c("White", "Asian", "Prefer not to answer",
             "Other", "Black or African American",
             "Latinx", "Hawaiian or Pacific Islander",
             "American Indian or Alaskan Native"),
  labels = c("white", "asian", NA,
             "other", "black",
             "latinx", "pacific_islander",
             "native")
)


# factorize race with custom labels
race_distinct <- race_distinct %>%
  mutate(
    race_p1 = race_fac_p1,
    race_p2 = race_fac_p2
  ) %>%
  # remove duplicate "race" rows --> creates race hierarchy that favors minorities
  filter(!duplicated(`Record ID_p2`))



# init. new DF to store all non-asians
race_non_asian <- race_distinct %>%
  filter(race_p2 != "asian", `Race Asian_p2` == "", race_p1 != "asian")


# init. DF of just asians
race_asian <- race_distinct %>%
  filter(race_p2 == "asian" | `Race Asian_p2` != "" | race_p1 == "asian") %>%
  # pivot all race asian cols for Phase 1
  pivot_longer(
    cols = colnames(race_distinct)[which(str_detect(colnames(race_distinct), "Race Asian(?!_p2).+$"))],
    names_to = "race_asian_p1",
    names_pattern = "(?<=\\=)([\\w\\s]+)",          # all words/spaces preceded by "="
    values_to = "race_asian_val_p1"
  ) %>%
  filter(race_asian_val_p1 == "Checked")


# combine non-asian + asian records
race_df <- bind_rows(race_non_asian, race_asian) %>%
  arrange(`Record ID_p2`) %>%
  rename("race_asian_p2" = `Race Asian_p2`) %>%
  mutate(
    across(.cols = c(race_asian_p2, race_asian_p1),
           .fns = ~ str_squish(.x))
  ) %>%
  select(
    `Record ID_p2`:`other race_p2`, 
    `race_asian_p2`, 
    # Ethnicity_p2:"other race_p1"
    `Ethnicity_p2`:`other race_p1`,
    `race_asian_p1`,
    `Ethnicity_p1`:`How many parents can you enroll?_2`,
    -`Race Asian_p1`
  ) %>%
  filter(!duplicated(`Record ID_p2`))


# init. factor to store Asian race data
asian_fac_p2 <- factor(
  race_df$race_asian_p2,
  levels = c("Korean", "Vietnamese", "Asian Indian",
             "Other Asian", "Chinese", ""),
  labels = c("korean", "vietnamese", "indian",
             "other", "chinese"),
  exclude = ""
)

asian_fac_p1 <- factor(
  race_df$race_asian_p1,
  levels = c('Korean', 'Vietnamese', 'Asian Indian', 'Other Asian',
             'Chinese', NA),
  labels = c('korean', 'vietnamese', 'indian', 'other',
             'chinese')
)


# factorize asian race
race_df <- race_df %>%
  mutate(race_asian_p2 = asian_fac_p2,
         race_asian_p1 = asian_fac_p1)





