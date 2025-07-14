###URLs
access_url_base <- 'https://viafoundry.umassmed.edu'
access_url <- 'https://viafoundry.umassmed.edu/vmeta/api/v1/users/login'
headers_post <- c('Content-Type' = 'application/json')
shipments_url <- "/vmeta/api/v1/projectid/667196af25f716a2d9f39248/data/shipments/populated"
patients_url = "/vmeta/api/v1/projects/vigor_production/data/patient"
surveys_url <- "/vmeta/api/v1/projects/vigor_production/data/surveys"

### Patterns

#id patterns
fam_id_pat <- "(?<=VIGOR_)\\d+"
fam_pos_pat <- "\\d+(?=_v)|\\d+$"

# init. pattern for extracting strings from `answers` field
ans_pat <- "(?<=\").{1,1000}(?=\")"

###Important dates
webinar_day <- mdy("8-20-2024")

###Timezones
run_time_est <- with_tz(now(), "US/Eastern")
runt_time_est_fmt <- str_c("Last updated:", run_time_est, sep = " ")
current_tz <- Sys.timezone()