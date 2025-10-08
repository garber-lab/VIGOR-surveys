# test_source.R

# install necessary packages
if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here")
}

# set project root anchor by specifying the credentials file location relative
# to current script
here::i_am("redcap_credentials.R")

# source the credentials file by building its path relative to the anchored 
# root
source(here::here("redcap_credentials.R"))



# source("../redcap_credentials.R", chdir = T)

print(str_c("Phase I token (obsolete): ", api_token_phase1.1))
