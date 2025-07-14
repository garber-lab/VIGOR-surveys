library(httr)
library(jsonlite)
library(glue)
library(purrr)
library(stringr)
library(lubridate)
library(hms)

# Function to initialize Foundry session and return the token
init_foundry_session <- function(email, password, access_url) {
  # Build login body
  body <- list(email = email, password = password)
  
  headers_post <- c('Content-Type' = 'application/json')
  
  # Perform POST request to authenticate
  post_resp <- POST(
    url = access_url,
    add_headers(.headers = headers_post),
    body = toJSON(body, auto_unbox = TRUE),
    encode = "json"
  )
  
  # Extract access token
  foundry_token <- content(post_resp)$token
  
  return(foundry_token)
}

# Function to fetch data given a token and URL
get_df <- function(foundry_token, access_url_base, api_url) {
  headers_get <- c('Authorization' = glue('Bearer {foundry_token}'))
  
  url <- str_c(access_url_base, api_url)
  
  get_req <- GET(url = url, add_headers(.headers = headers_get))
  
  resp <- get_req %>%
    content(as = "text") %>%
    fromJSON()
  
  df <- resp %>% pluck("data", "data")
  
  return(df)
}


# Function to get the date/time of last Foundry update
get_time_fountry_last_updated <- function(run_time_est = Sys.time(),
                                          formatted = FALSE) {
  # Force run_time_est into EST/EDT
  run_time_est <- with_tz(run_time_est, tzone = "America/New_York")
  run_time_hms <- hms::as_hms(run_time_est)
  update_time_hms <- hms::as_hms("05:00:00")
  
  # Determine last_updated timestamp
  if (run_time_hms > update_time_hms) {
    last_updated_fdry <- as_datetime(
      str_c(today(tz = "America/New_York"), update_time_hms, sep = " "),
      tz = "America/New_York"
    )
  } else {
    last_updated_fdry <- as_datetime(
      str_c(today(tz = "America/New_York") - 1, update_time_hms, sep = " "),
      tz = "America/New_York"
    )
  }
  
  if (formatted) {
    return(str_c(
      "Last updated:", date(last_updated_fdry),
      hms::hms(
        seconds = round(second(last_updated_fdry)),
        minutes = minute(last_updated_fdry),
        hours = hour(last_updated_fdry)
      ),
      tz(last_updated_fdry),
      sep = " "
    ))
  } else {
    return(last_updated_fdry)
  }
}