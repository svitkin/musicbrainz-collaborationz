rm(list = ls())

library(purrr)
library(dplyr)
library(stringr)
library(lubridate)

mb_get_artist_id <- function(artist_name) {
  # url creation
  base_url <- "http://musicbrainz.org/ws/2"
  url <- base::paste(c(base_url, "artist"), collapse = "/")

  parsed_url <- httr::parse_url(url)
  parsed_url$query <- base::list(query = artist_name, limit = 1,  offset = 0)

  url <- httr::build_url(parsed_url)

  # api call
  get_data <- httr::GET(url,
                        httr::add_headers(
                          Accept ="application/json",
                          "user-agent" = "collaboration R shiny app"))

  # status check
  status <- httr::status_code(get_data)
  if(status != 200) stop(base::paste0("Status code: ", status))

  # extract content
  content_data <- httr::content(get_data, type = "application/json")

  # return data
  return(content_data$artists[[1]]$id)
}

mb_get_release_data_by_artist <- function(artist_name, limit, offset) {
  arid <- mb_get_artist_id(artist_name)

  # url creation
  base_url <- "http://musicbrainz.org/ws/2"
  url <- base::paste(c(base_url, "release"), collapse = "/")

  parsed_url <- httr::parse_url(url)
  parsed_url$query <- base::list(query = paste0("arid:", arid), limit = limit,  offset = offset)

  url <- httr::build_url(parsed_url)

  # api call
  get_data <- httr::GET(url,
                        httr::add_headers(
                          Accept ="application/json",
                          "user-agent" = "collaboration R shiny app"))

  # status check
  status <- httr::status_code(get_data)
  if(status != 200) stop(base::paste0("Status code: ", status))

  # extract content
  httr::content(get_data, type = "application/json")$releases
}

mb_get_release_collaborations_by_artist <- function(artist_name) {
  offset <- 0
  limit <- 100
  release_data <- list()
  release_data[[length(release_data) + 1 ]] <- mb_get_release_data_by_artist(artist_name, limit, offset)
  while (length(release_data[[length(release_data)]]) >= limit) {
    offset <- offset + limit
    release_data[[length(release_data) + 1 ]] <- mb_get_release_data_by_artist(artist_name, limit, offset)
  }

  release_data <- unlist(release_data, recursive = FALSE)

  map_df(release_data,
         function(rel) {
           data.frame(release_title = rel$title,
                      release_date = ifelse(is.null(rel$date), NA, rel$date),
                      release_type = ifelse(is.null(rel$`release-group`$`primary-type`), NA, rel$`release-group`$`primary-type`),
                      artist_2 = unique(unlist(lapply(rel$`artist-credit`,
                                                      function(cred) str_trim(cred$name)))),
                      stringsAsFactors = FALSE)
         }) %>%
    filter(str_to_lower(artist_2) != str_to_lower(artist_name)) %>%
    mutate(artist_1 = artist_name) %>%
    mutate(year = as.numeric(str_extract(release_date, "^\\d{4}"))) %>%
    group_by(release_title, release_type, artist_2, year) %>%
    arrange(year) %>%
    slice(1) %>%
    ungroup()
}
