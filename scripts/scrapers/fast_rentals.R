library(RSelenium)
library(rvest)
library(tidyverse)

# if directory already exists then we move on
if(!dir.exists(here::here(paste0("data/uncleaned_data/", lubridate::floor_date(Sys.Date(), unit = "month"))))) {
  
  # create a new folder for every new month and year 
  dir.create(here::here(paste0("data/uncleaned_data/", lubridate::floor_date(Sys.Date(), unit = "month"))))
  dir.create(here::here(paste0("data/cleaned_data/", lubridate::floor_date(Sys.Date(), unit = "month"))))
  dir.create(here::here(paste0("data/links/", lubridate::floor_date(Sys.Date(), unit = "month"))))
  
}

# if directory already exists then we move on
if(!here::here(paste0("data/uncleaned_data/", lubridate::floor_date(Sys.Date(), unit = "month"), "/", Sys.Date()))) {
  
  # create a new folder for the new data that will be scraped
  dir.create(here::here(paste0("data/uncleaned_data/", lubridate::floor_date(Sys.Date(), unit = "month"), "/", Sys.Date())))
  dir.create(here::here(paste0("data/cleaned_data/", lubridate::floor_date(Sys.Date(), unit = "month"), "/", Sys.Date())))
  dir.create(here::here(paste0("data/links/", lubridate::floor_date(Sys.Date(), unit = "month"), "/", Sys.Date())))
  
}

# docker run -d -p 4445:4444 selenium/standalone-firefox:latest
# connect to driver with docker
remDr <- remoteDriver(
  remoteServerAddr = "192.168.99.100",
  port = 4445L
)
remDr$open()


c(
  "https://www.rentfaster.ca/bc/vancouver/rentals/?l=15,49.2567,-123.1412",
  "https://www.rentfaster.ca/bc/vancouver/rentals/?l=16,49.2854,-123.1305",
  "https://www.rentfaster.ca/bc/vancouver/rentals/?l=16,49.2799,-123.128",
  "https://www.rentfaster.ca/bc/vancouver/rentals/?l=16,49.2808,-123.1095",
  "https://www.rentfaster.ca/bc/vancouver/rentals/?l=16,49.2755,-123.1213",
  "https://www.rentfaster.ca/bc/vancouver/rentals/?l=15,49.2718,-123.1531",
  "https://www.rentfaster.ca/bc/vancouver/rentals/?l=14,49.2608,-123.072",
  "https://www.rentfaster.ca/bc/vancouver/rentals/?l=13,49.2201,-123.0911",
  "https://www.rentfaster.ca/bc/vancouver/rentals/?l=15,49.2567,-123.1412",
  "https://www.rentfaster.ca/bc/vancouver/rentals/?l=14,49.2346,-123.1674",
  "https://www.rentfaster.ca/bc/vancouver/rentals/?l=14,49.214,-123.1161"
) -> urls

l <- list()
for(i in 1:length(urls)) {
  
  print(i)
  
  Sys.sleep(runif(1, min = 10, max = 20))
  remDr$navigate(urls[[i]])
  
  Sys.sleep(runif(1, min = 10, max = 20))
  web_elements <- remDr$findElements(using = "tag", "a")
  
  Sys.sleep(runif(1, min = 10, max = 20))
  l[[i]] <- unlist(sapply(web_elements, function(x) {x$getElementAttribute("href")}))
  
}

l %>%
  purrr::flatten_chr() %>%
  .[stringr::str_detect(., "(pet-friendly|non-smoking)")] %>%
  unique() %>%
  {dplyr::tibble(url = .)} -> links

readr::write_csv(links,  here::here(paste0("data/links/", 
                                           lubridate::floor_date(Sys.Date(), unit = "month"), 
                                           "/", 
                                           Sys.Date()-1,
                                           "/fast_rentals.csv")))
df_all <- data.frame()
links <- dplyr::pull(links)
for(i in 1:length(links)) {
  
  print(i)
  Sys.sleep(runif(1, min = 10, max = 15))
  
  url <- xml2::read_html(links[[i]])
  url %>%
    rvest::html_nodes("script") %>%
    rvest::html_text() %>%
    purrr::map(~ .[stringr::str_detect(., "window.unit")]) %>%
    purrr::discard(~length(.) == 0) %>%
    purrr::pluck(1) %>%
    stringr::str_remove("\\;") %>%
    stringr::str_remove("window.units = ") %>%
    stringr::str_squish() %>%
    stringr::str_split(pattern = ",") -> a
  
  a %>%
    purrr::pluck(1) %>%
    purrr::map(~ purrr::set_names(., stringr::str_remove(., "\\:.*"))) %>%
    purrr::flatten_chr() %>%
    { .[which(stringr::str_detect(names(.), "(furnishing|type|price|beds|baths|sq_feet)"))] } %>%
    stringr::str_sub(start = 2, end = -2) %>%
    purrr::set_names(., stringr::str_remove(., "\\:.*")) -> b
  
  b %>%
    { .[which(stringr::str_detect(names(.), "(furnishing|type|price|beds|baths|sq_feet)"))] } %>%
    { unique(names(.)) } %>%
    stringr::str_sub(end = -2) %>%
    rep(., length(b) / length(.)) %>%
    { purrr::set_names(b, .) } -> c
  
  c %>%
  { dplyr::tibble(furnishing = .[stringr::str_detect(names(.), "furnishing")],
                  type = .[stringr::str_detect(names(.), "type")], 
                  price = .[stringr::str_detect(names(.), "price")],
                  beds = .[stringr::str_detect(names(.), "beds")],
                  sq_feet = .[stringr::str_detect(names(.), "sq_feet")],
                  baths = .[stringr::str_detect(names(.), "baths")]) } %>%
    dplyr::mutate_at(vars(price, sq_feet, baths, beds), ~ readr::parse_number(.)) %>% 
    dplyr::mutate_at(vars(furnishing, type), ~stringr::str_extract(., ':".*') %>%
                     stringr::str_sub(start = 3, end = -1)) -> d
  
  url %>%
    rvest::html_nodes(xpath = '//*[@property = "postalCode"]') %>%
    rvest::html_text() %>%
    { dplyr::tibble(postal_code = .) } -> postal_code
  
  if(nrow(postal_code) == 0) {
    
    postal_code[1, ] <- NA
    
  }
  
  url %>%
    rvest::html_nodes(xpath = '//*[@property = "streetAddress"]') %>%
    rvest::html_text() %>%
    { dplyr::tibble(address = .) } -> address
  
  if(nrow(address) == 0) {
    
    address[1, ] <- NA
    
  }
  
  url %>%
    rvest::html_nodes(xpath = '//*[@property = "description"]') %>%
    rvest::html_text() %>%
    .[[1]] %>%
    { dplyr::tibble(description = .) } -> description
  
  url %>%
    rvest::html_nodes("meta") %>%
    rvest::html_nodes(xpath = '//*[@name = "geo.position"]') %>%
    as.character() %>%
    stringr::str_remove(".* content=") %>%
    stringr::str_sub(start = 2, end = -4) %>%
    stringr::str_split(pattern = ",") %>%
    purrr::flatten_chr() %>%
    { dplyr::tibble(lat = .[[1]], lon = .[[2]]) } -> geo
  
  f <- dplyr::tibble(url = links[[i]])
  
  d %>%
    base::cbind(postal_code, address, description, geo, f) -> e
  
  df_all <- df_all %>%
    dplyr::bind_rows(e)
  readr::write_csv(df_all,  here::here(paste0("data/uncleaned_data/", 
                                              lubridate::floor_date(Sys.Date(), unit = "month"), 
                                              "/", 
                                              Sys.Date()-1,
                                              "/fast_rentals.csv")))
  
}

df_all %>%
  dplyr::rename(furnished = furnishing, bed = beds,
                bath = baths, sqft = sq_feet) %>%
  dplyr::mutate(bed = ifelse(is.na(bed), 0, bed)) %>%
  na.omit() %>%
  dplyr::distinct(price, bed, bath, sqft, .keep_all = TRUE) %>%
  readr::write_csv( here::here(paste0("data/cleaned_data/", 
                                      lubridate::floor_date(Sys.Date(), unit = "month"), 
                                      "/", 
                                      Sys.Date()-1,
                                      "/fast_rentals.csv")))
