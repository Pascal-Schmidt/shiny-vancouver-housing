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

# base url from where we are scraping data
base_url <- "https://rentals.ca/vancouver"

pages <- 52

# add how many pages there are
extension <- paste0("?p=", 2:pages)
extension <- c(base_url, paste0(base_url, extension))

# save links in links and get all links
links <- list()
for(i in 1:length(extension)) {
  
  beepr::beep()
  print(i)
  url <- extension[[i]]
  remDr$navigate(url)
  Sys.sleep(runif(n = 1, min = 5, max = 10))
  
  web_elements <- remDr$findElements(using = "class", "listing-card__details-link")
  
  links[[i]] <- unlist(sapply(web_elements, function(x) {x$getElementAttribute("href")}))
  
  links %>%
    purrr::flatten_chr() %>%
    unique() %>%
    {dplyr::tibble(url = .)} %>% 
    readr::write_csv( here::here(paste0("data/links/", 
                                        lubridate::floor_date(Sys.Date(), unit = "month"), 
                                        "/", 
                                        Sys.Date(),
                                        "/rentals.csv")))
  
}

# all collected links with RSelenium
links %>%
  purrr::flatten_chr() %>%
  unique() %>%
  {dplyr::tibble(url = .)} -> all_urls

readr::write_csv(all_urls, here::here(paste0("data/links/", 
                                             lubridate::floor_date(Sys.Date(), unit = "month"), 
                                             "/", 
                                             Sys.Date()-1,
                                             "/rentals.csv"))) 

all_urls <- readr::read_csv(here::here(paste0("data/links/", 
                                              lubridate::floor_date(Sys.Date(), unit = "month"), 
                                              "/", 
                                              Sys.Date()-1,
                                              "/rentals.csv"))) %>%
  dplyr::pull()

# get information from json in html source code
# no RSelenium needed anymore
df_all <- dplyr::tibble()
for(i in 1:length(all_urls)) {
  
  print(i)  
  beepr::beep()
  
  # if listing has been removed continue with next url
  tryCatch({
    url <- xml2::read_html(all_urls[[i]])
  }, error = function(e) {cat("ERROR :", conditionMessage(e), "\n")} )
  
  Sys.sleep(runif(1, min = 5, max = 8))
  
  url %>%
    rvest::html_nodes("meta") %>%
    as.character() %>%
    .[stringr::str_detect(., "og:description")] %>%
    stringr::str_remove('.*content=\"') %>%
    stringr::str_extract("is (a|an) [a-z]+") -> type
  
  type <- dplyr::tibble(type = type)
  
  url %>%
    rvest::html_nodes(xpath = '//*[@type="application/ld+json"]') %>%
    xml2::as_list() %>%
    purrr::pluck(1, 1, 1) %>%
    stringr::str_remove_all("\\\n|\\\\n") %>%
    rjson::fromJSON() -> json
    
  json %>%
    roomba::roomba(cols = c("latitude", "longitude", "price", "postalCode", 
                            "streetAddress", "name", "value"), keep = any) %>%
    purrr::pmap(~ c(...)) %>%
    purrr::map(~ .[!is.na(.)]) %>%
    purrr::flatten_chr() %>%
    dplyr::bind_rows() -> df_json
    
    json %>%
      .[["description"]] %>%
      dplyr::as_tibble() %>% 
      dplyr::rename("description" = value) -> df_json2
  
  url %>%
    as.character() %>%
    stringr::str_extract('"bath(s)": [0-9]+') %>%
    stringr::str_remove_all('\\\"') %>%
    purrr::set_names("bath") %>%
    dplyr::bind_rows() -> bath
  
  url <- dplyr::tibble(url = all_urls[[i]])
  
  df <- dplyr::bind_cols(bath, df_json2, df_json, type, url)
  
  df_all <- df_all %>%
    dplyr::bind_rows(df)
  
  # write uncleaned csv data to file
  df_all %>%
    readr::write_csv(here::here(paste0("data/uncleaned_data/", 
                                       lubridate::floor_date(Sys.Date(), unit = "month"), 
                                       "/", 
                                       Sys.Date()-1,
                                       "/rentals.csv")))
  
}

# read in uncleaned data
df_all <- readr::read_csv(here::here(paste0("data/uncleaned_data/", 
                                            lubridate::floor_date(Sys.Date(), unit = "month"), 
                                            "/", 
                                            Sys.Date()-1,
                                            "/rentals.csv")))

df_all %>%
  
  # remove missing data
  dplyr::filter(!is.na(value)) %>%
  
  # rename for proper naming convention
  janitor::clean_names() %>%
  dplyr::rename("lat" = latitude, "lon" = longitude, 
                "sqft" = value, "bed" = name, "address" = street_address) %>% 
  dplyr::filter(!is.na(sqft)) %>%
  dplyr::mutate_at(vars(bed, bath), ~readr::parse_number(.)) %>% 
  
  # lower case description
  dplyr::mutate(ID = dplyr::row_number()) %>%
  dplyr::select(description, ID) %>%
  dplyr::mutate(description = stringr::str_to_lower(description) %>%
                  textstem::lemmatize_words()) %>%
  tidytext::unnest_tokens(words, description, token = "ngrams", n = 2) %>%
  dplyr::filter(!words %in% tidytext::stop_words$word) %>%
  dplyr::filter(stringr::str_detect(words, "(furnished|unfurnished)")) %>%
  dplyr::filter(stringr::str_detect(words, "\\d\\d\\d\\d")) %>% 
  dplyr::group_by(ID) %>% 
  dplyr::mutate(n = n()) %>%
  dplyr::filter(n > 1) %>% 
  dplyr::ungroup() %>%
  dplyr::mutate(price = readr::parse_number(words),
                furnished = str_extract(words, "(furnished|unfurnished)")) %>%
  dplyr::filter(!(price == 2900 & furnished == "unfurnished")) %>%
  dplyr::select(furnished, price, ID) %>%
  dplyr::right_join(df_all2, by = "ID") %>% 
  dplyr::mutate(price.y = ifelse(!is.na(price.x), price.x, price.y)) %>%
  dplyr::select(-c(ID, price.x)) %>%
  dplyr::rename(price = price.y) %>% 
  
  # get furnished or unfurnished information from description
  dplyr::mutate(furnished = ifelse(stringr::str_detect(description, "\\b.*unfurnished.*\\b") &
                                     is.na(furnished),
                                   "unfurnished", NA)) %>%
  dplyr::mutate(furnished = ifelse(stringr::str_detect(description, "\\b.*furnished.*\\b") &
                                     is.na(furnished),
                                   "furnished", furnished)) %>% 
  dplyr::mutate(furnished = ifelse(is.na(furnished), "unfurnished", furnished)) %>% 
  dplyr::mutate(type = stringr::str_split(type, pattern = " ") %>%
                  purrr::map_chr(~ .[3])) %>% 
  readr::write_csv(here::here(paste0("data/cleaned_data/", 
                                     lubridate::floor_date(Sys.Date(), unit = "month"), 
                                     "/", 
                                     Sys.Date()-1,
                                     "/rentals.csv")))

