library(RSelenium)
library(rvest)
library(tidyverse)
library(googleway)
library(config)

Sys.setenv(R_CONFIG_ACTIVE = "default")
config <- config::get(file = "google_api_key.yml")


# docker run --name chrome -v /dev/shm:/dev/shm -d -p 4445:4444 -p 5901:5900 selenium/standalone-chrome-debug:latest

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

# connect to driver with docker
remDr <- remoteDriver(
  remoteServerAddr = "192.168.99.100",
  port = 4445L
)
remDr$open()

base_url <- "https://sothebysrealty.ca/en/search-results/city-vancouver-reg-greater-vancouver-bc-real-estate/price-0-10000000/status-1/view-grid/show-mls/sort-1/pp-32/"
url <- paste0(base_url, "page-", 2:83)

all_links <- list()
for(i in 59:length(url)) {
  
  print(i)
  runif(1, min = 5, max = 8)
  remDr$navigate(url[[i]])
  
  web_elements <- remDr$findElements(using = "class", value = "plink")
  all_links[[i]] <- unlist(sapply(web_elements, function(x) {x$getElementAttribute("href")}))
  beepr::beep()
  
  all_links %>%
    purrr::flatten_chr() %>%
    unique() %>%
    {dplyr::tibble(url = .)} %>%
    readr::write_csv(here::here(paste0("data/links/", 
                                       lubridate::floor_date(Sys.Date(), unit = "month"), 
                                       "/", 
                                       Sys.Date()-1,
                                       "/sotheby.csv"))) 
  
}

# unique links as character vector
all_links %>%
  purrr::flatten_chr() %>%
  unique() %>%
  {dplyr::tibble(url = .)} %>%
  readr::write_csv(here::here(paste0("data/links/", 
                                     lubridate::floor_date(Sys.Date(), unit = "month"), 
                                     "/", 
                                     Sys.Date()-1,
                                     "/sotheby.csv")))

all_links <- readr::read_csv(here::here(paste0("data/links/", 
                                               lubridate::floor_date(Sys.Date(), unit = "month"), 
                                               "/", 
                                               Sys.Date()-1,
                                               "/sotheby.csv"))) %>%
  dplyr::pull()

df_all <- data.frame()
for(i in 1:length(all_links)) {
  
  print(i)
  
  tryCatch({
  url <- xml2::read_html(all_links[[i]])
  }, error = function(e) {cat("ERROR :", conditionMessage(e), "\n")} )
  
  Sys.sleep(runif(1, 3, 5))
  url %>%
    rvest::html_nodes(xpath = '//*[@class = "price_social"]') %>%
    rvest::html_text() %>%
    .[[1]] -> price
  
  url %>%
    rvest::html_nodes(xpath = '//*[@class = "quick_facts"]') %>%
    .[[2]] %>%
    as.character() %>%
    stringr::str_split(pattern = "\\<") %>%
    purrr::map(~ stringr::str_remove_all(., '(.*\\>|\\\n)')) %>%
    purrr::flatten_chr() %>%
    .[. != ""] %>%
    purrr::set_names(., c("address", "bed", "bath", "bb_sqft")) %>%
    dplyr::bind_rows() -> characteristics
  
  url %>%
    rvest::html_nodes(xpath = '//*[@class = "mod_content"]') %>%
    .[[1]] %>%
    as.character() %>%
    stringr::str_split('<li>') %>%
    purrr::flatten_chr() %>%
    stringr::str_remove_all(., '(\\\n\\<span\\>|\\<\\/span\\>|\\<\\/li\\>\\\n)') %>%
    .[stringr::str_detect(., "(Property Type|Lot Size|Taxes|Year Built)")] %>%
    dplyr::as_tibble() %>%
    tidyr::separate(value, into = c("col", "val"), sep = ":") %>%
    tidyr::pivot_wider(names_from = col, values_from = val) -> more_characteristics
  
  url %>%
    rvest::html_nodes(xpath = '//*[@class = "mod_content"]') %>%
    rvest::html_text() %>%
    .[[3]] -> description
  
  url <- dplyr::tibble(url = all_links[[i]])
  
  data.frame(price = price, description = description) %>%
    dplyr::bind_cols(characteristics, more_characteristics, url) -> df
  
  df_all <- df_all %>%
    dplyr::bind_rows(df)
  readr::write_csv(df_all, here::here(paste0("data/uncleaned_data/", 
                                             lubridate::floor_date(Sys.Date(), unit = "month"), 
                                             "/", 
                                             Sys.Date()-1,
                                             "/sotheby.csv")))
  
}
readr::read_csv(here::here(paste0("data/uncleaned_data/", 
                                  lubridate::floor_date(Sys.Date(), unit = "month"), 
                                  "/", 
                                  Sys.Date()-1,
                                  "/sotheby.csv"))) -> df_all

df_all %>%
  janitor::clean_names() %>%
  dplyr::select(-c(lot_size:other_taxes_2015)) %>%
  dplyr::mutate_at(vars(price, bed, bb_sqft, other_taxes_2019), .funs = ~ readr::parse_number(.)) %>%
  dplyr::rename("sqft" = bb_sqft, "type" = property_type) %>%
  dplyr::mutate(type = stringr::str_remove(type, "Residential, ") %>%
                  stringr::str_to_lower()) %>%
  dplyr::as_tibble() %>% 
  dplyr::mutate(bath = stringr::str_replace(bath, "\\+1", ".5"),
                bath = stringr::str_replace(bath, "\\+2", ".5"),
                bath = stringr::str_replace(bath, "\\+3", ".5"),
                bath = readr::parse_number(bath)) -> df_all
  

api_key <- config$google_api_key
address <- df_all$address
lat_lon <- vector(mode = "list", length = length(address))
for(i in 1:length(address)) {
  
  Sys.sleep(0.5)
  print(i)
  lat_lon[[i]] <- googleway::google_geocode(address = address[[i]],
                                            key = api_key,
                                            simplify = TRUE)
  
}

lat_lon %>%
  purrr::map(~ purrr::pluck(., "results", "geometry", "location")) %>%
  purrr::map(~ .[1, ]) %>%
  purrr::map(~ bind_cols(., data.frame(ID = 1))) %>%
  dplyr::bind_rows() %>% 
  dplyr::bind_cols(df_all) %>% 
  dplyr::select(-c(contains("taxes"), ID)) %>% 
  dplyr::rename(lon = lng) -> df_all


readr::write_csv(df_all, here::here(paste0("data/cleaned_data/", 
                                           lubridate::floor_date(Sys.Date(), unit = "month"), 
                                           "/", 
                                           Sys.Date()-1,
                                           "/sotheby.csv")))
  
  

