library(tidyverse)

current_links <- function(website) {
  
  # base directory links
  directory <- paste0(here::here(), "/data/links/")
  
  # list all craigslist links previoulsy scraped
  directory %>%
    list.files(recursive = TRUE) %>%
    .[stringr::str_detect(., website)] %>%
    .[stringr::str_detect(., as.character(Sys.Date()), negate = TRUE)] -> previous_links
  
  directory %>%
    list.files(recursive = TRUE) %>%
    .[stringr::str_detect(., website)] %>%
    .[stringr::str_detect(., as.character(Sys.Date()))] -> current_links
  
  stringr::str_glue(directory, previous_links) %>%
    purrr::map(~ readr::read_csv(.)) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(time = "previous") -> previous_links
  
  current_links <- readr::read_csv(paste0(directory, current_links)) %>%
    dplyr::mutate(time = "current")
  
  # combine previous and current links and remove duplicates
  current_links %>%
    dplyr::bind_rows(previous_links) %>%
    dplyr::group_by(url) %>%
    dplyr::mutate(n = dplyr::n()) %>%
    dplyr::filter(n == 1 & time == "current") %>%
    dplyr::pull(url) -> all_links
  
  return(all_links)
  
}