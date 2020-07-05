library(rvest)
library(tidyverse)

# only scrape new links
source(here::here("scripts/helper_fn/current_links.R"))

# create folders
source(here::here("scripts/helper_fn/create_folder.R"))
create_folders()

urls <- paste0("https://www.rew.ca/properties/areas/vancouver-bc/sort/featured/asc/page/", 
               2:25, 
               "?query=Vancouver%2C+BC")

all_links <- list()
for(i in 1:length(urls)) {
  
  print(i)
  Sys.sleep(runif(n = 1, min = 5, max = 15))
  
  url <- xml2::read_html(urls[[i]])
  
  url %>%
    rvest::html_nodes(".displaypanel-body") %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href") -> all_links[[i]]
  
  all_links %>%
    purrr::flatten_chr() %>%
    {dplyr::tibble(url = .)} %>%
    readr::write_csv(here::here(paste0("data/links/", 
                                       lubridate::floor_date(Sys.Date(), unit = "month"), 
                                       "/", 
                                       Sys.Date(),
                                       "/rew.csv")))
  
}


all_links <- paste0("https://www.rew.ca", all_links %>%
                      purrr::flatten_chr())

# write links to csv
all_links %>%
  unique() %>%
  {dplyr::tibble(url = .)} %>%
  readr::write_csv(here::here(paste0("data/links/", 
                                     lubridate::floor_date(Sys.Date(), unit = "month"), 
                                     "/", 
                                     Sys.Date(),
                                     "/rew.csv")))

all_links_rew <- current_links("rew")
df_all <- data.frame()
for(i in 1:length(all_links)) {
  
  print(i)
  # if listing has been removed continue with next url
  tryCatch({
    url <- all_links_rew[[i]] %>%
      xml2::read_html()
  
  Sys.sleep(runif(1, min = 8, max = 15))
  
  # price
  url %>%
    rvest::html_nodes(".propertyheader-price") %>%
    rvest::html_text() %>%
    readr::parse_number() %>%
    purrr::set_names("price") %>%
    dplyr::bind_rows() -> price
  
  # bed, bath, sqft, type
  url %>%
    rvest::html_nodes(".clearfix") %>%
    rvest::html_nodes("strong") %>%
    rvest::html_text() %>%
    purrr::set_names(c("bed", "bath", "sqft", "type")) %>%
    dplyr::bind_rows() -> bbs
  
  # address
  url %>%
    rvest::html_nodes(".propertyheader-address") %>%
    rvest::html_text() %>%
    purrr::set_names("address") %>%
    dplyr::bind_rows() -> address
    
  # latitude, longitude, description, geo
  url %>%
    rvest::html_nodes(xpath = '//*[@type="application/ld+json"]') %>%
    xml2::as_list() %>% 
    purrr::pluck(1, 1, 1) %>%
    stringr::str_remove_all("\\\n|\\\\n") %>%
    rjson::fromJSON() %>%
    purrr::map(~ purrr::keep(., names(.) %in% c("description", "geo"))) %>%
    roomba::roomba(cols = c("description", "longitude",
                            "latitude", "postalCode"), keep = any) %>%
    purrr::pmap(~ c(...)) %>%
    purrr::map(~ .[which(!is.na(.))]) %>%
    purrr::flatten_dfr() -> a
  
  a %>%
    dplyr::mutate(description = description %>%
                    stringr::str_squish() %>%
                    textclean::replace_url() %>%
                    stringr::str_to_lower() %>%
                    gsub("[[:punct:]]+", " ", .) %>%
                    stringr::str_squish()
                  ) -> a
  
  # features
  url %>%
    rvest::html_nodes("tbody") %>%
    rvest::html_nodes("th") %>%
    rvest::html_text() -> feature
    
  url %>%
    rvest::html_nodes("tbody") %>%
    rvest::html_nodes("td") %>%
    rvest::html_text() -> characteristic
  
  characteristic %>%
    purrr::set_names(feature) %>%
    dplyr::bind_rows() -> characteristics
  
  df <- dplyr::bind_cols(price, address, bbs, a, characteristics) %>%
    dplyr::tibble(url = all_links[[i]])
  
  }, error = function(e) {cat("ERROR :", conditionMessage(e), "\n")} )
  
  df_all <- df_all %>%
    dplyr::bind_rows(df)
  
  readr::write_csv(df_all, here::here(paste0("data/uncleaned_data/", 
                                             lubridate::floor_date(Sys.Date(), unit = "month"), 
                                             "/", 
                                             Sys.Date(),
                                             "/rew.csv")))
  
}

df_all <- readr::read_csv(here::here(paste0("data/uncleaned_data/", 
                                            lubridate::floor_date(Sys.Date(), unit = "month"), 
                                            "/", 
                                            Sys.Date(),
                                            "/rew.csv")))

df_all %>%
  janitor::clean_names() %>% 
  dplyr::select(price:longitude, property_age, url) %>%
  dplyr::rename(year_built = property_age, lon = longitude, lat = latitude) %>%
  dplyr::mutate(year_built = stringr::str_extract(year_built, ".*\\d\\d\\d\\d") %>%
                  readr::parse_number()) %>% 
  dplyr::mutate(type = dplyr::case_when(type == "Apt/Condo" ~ "condo",
                                        TRUE ~ as.character(type))) %>% 
  dplyr::mutate(type = tolower(type)) %>%
  readr::write_csv(here::here(paste0("data/cleaned_data/", 
                                     lubridate::floor_date(Sys.Date(), unit = "month"), 
                                     "/", 
                                     Sys.Date(),
                                     "/rew.csv")))

