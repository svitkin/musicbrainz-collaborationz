library(purrr)
library(dplyr)
library(stringr)
library(lubridate)

mb_get_artist <- function(artist_name) {
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
  list(arid = content_data$artists[[1]]$id,
       name = content_data$artists[[1]]$name,
       aliases = unlist(lapply(content_data$artists[[1]]$aliases,
                              function(alias) alias$name)))
}

mb_get_release_data_helper <- function(artist_name, limit, offset) {
  artist_lookup <- mb_get_artist(artist_name)
  arid <- artist_lookup$arid

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

mb_get_release_data_loop <- function(artist_name, sleep_seconds  = 0) {
  offset <- 0
  limit <- 100
  release_data <- list()
  release_data[[length(release_data) + 1 ]] <- mb_get_release_data_by_artist_helper(artist_name, limit, offset)
  while (length(release_data[[length(release_data)]]) >= limit) {
    offset <- offset + limit
    Sys.sleep(sleep_seconds)
    release_data[[length(release_data) + 1 ]] <- mb_get_release_data_by_artist_helper(artist_name, limit, offset)
  }

  release_data <- unlist(release_data, recursive = FALSE)
}

mb_get_release_data_by_artist <- function(artist_name) {
  tryCatch({
    mb_get_release_data_loop(artist_name)
  },
  error = function(error) {
    # If error trying to get all data, then sleep between iterations of data pulls
    tryCatch({
      mb_get_release_data_loop(artist_name, 3)
    },
    error = function(error) {
      validate(FALSE, "Error retrieving data from MusicBrainz. Please wait a few seconds and try again or try a different search.")
    })
  })
}

mb_get_release_collaborations_by_artist <- function(artist_name) {
    release_data <- mb_get_release_data_by_artist(artist_name)
    artist_lookup <- mb_get_artist(artist_name)
    artist_filter_check <- str_to_lower(c(artist_lookup$name, artist_lookup$aliases))

    map_df(release_data,
           function(rel) {
             data.frame(release_title = rel$title,
                        release_date = ifelse(is.null(rel$date), NA, rel$date),
                        release_type = ifelse(is.null(rel$`release-group`$`primary-type`), NA, rel$`release-group`$`primary-type`),
                        artist_2 = unique(unlist(lapply(rel$`artist-credit`,
                                                        function(cred) str_trim(cred$name)))),
                        stringsAsFactors = FALSE)
           }) %>%
      filter(!str_to_lower(artist_2) %in% artist_filter_check) %>%
      mutate(artist_1 = artist_name) %>%
      mutate(year = as.numeric(str_extract(release_date, "^\\d{4}"))) %>%
      group_by(release_title, release_type, artist_2, year) %>%
      arrange(year) %>%
      slice(1) %>%
      ungroup()
}

