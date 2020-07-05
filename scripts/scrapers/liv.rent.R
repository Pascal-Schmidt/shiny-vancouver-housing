library(rvest)
library(tidyverse)
library(janitor)
library(roomba)
library(jsonlite)

# only scrape new links
source(here::here("scripts/helper_fn/current_links.R"))

# create folders
source(here::here("scripts/helper_fn/create_folder.R"))
create_folders()

# base url from where we are scraping data
base_url <- "https://liv.rent/rental-listings/city/vancouver"

# get all links
pages <- 70
urls <- c(base_url, paste0(base_url, "?page=", 2:pages))
all_links_liv <- vector(mode = "list", length = length(urls))
for(i in seq_along(urls)) {
  
  print(i)
  Sys.sleep(runif(1, min = 5, max = 8))
  url <- urls[[i]]
  
  url <- xml2::read_html(url)
  url %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href") %>%
    .[stringr::str_detect(., "/rental-listings/detail/")] -> all_links_liv[[i]]
  
  all_links_liv %>%
    purrr::flatten_chr() %>%
    unique() %>%
    {dplyr::tibble(url = .)} %>%
    readr::write_csv(here::here(paste0("data/links/", 
                                       lubridate::floor_date(Sys.Date(), unit = "month"), 
                                       "/", 
                                       Sys.Date(),
                                       "/liv_rent.csv")))
  
}


all_links_liv <- current_links("liv_rent")

# read txt
# read.table("liv.rent_links_unique_vec.txt") %>%
#   dplyr::pull() %>%
#   as.character() -> all_links_liv

# get information for housing
all_urls <- paste0("https://liv.rent", all_links_liv)
df_all <- data.frame()
for(i in 1:length(all_urls)) {
  
  Sys.sleep(runif(1, min = 5, max = 15))
  
  tryCatch({
    
    print(i)
    # parse url
    url <- all_urls[[i]] %>%
      xml2::read_html()
    
    # get bedroom, bathroom, squarefeet, furnished or not
    url %>%
      rvest::html_nodes(xpath = "//*[@class = 'listing-summary__DetailsWrapper-x9pgec-4 bwPgLV']") %>%
      rvest::html_nodes("p") %>%
      rvest::html_text() %>%
      stringr::str_to_lower() -> temp
    
    temp %>%
      purrr::set_names(stringr::str_extract(., "[a-z]+")) %>%
      dplyr::bind_rows() -> bbs
    
    # get type of property
    url %>%
      rvest::html_nodes("h4") %>%
      rvest::html_text() %>%
      .[[1]] %>%
      purrr::set_names("type") %>%
      dplyr::bind_rows() -> type
    
    # get all other information from json
    url %>%
      rvest::html_nodes(xpath = "//*[@type = 'application/ld+json']") %>%
      xml2::as_list() %>%
      purrr::pluck(1, 1, 1) %>%
      purrr::when(is.null(.) ~ data.frame(NANA = NA),
                  ~ stringr::str_remove_all(., "\\\n|\\\\n") %>%
                    rjson::fromJSON() %>%
                    purrr::discard(names(.) == "image")) -> json
    
    json %>%
      purrr::map(~ purrr::pluck(., 1)) %>% 
      dplyr::bind_rows() %>% 
      dplyr::select(`@type`, name, description, floorSize, url) -> df_json
    
    df_json %>%
      dplyr::mutate(description = description %>%
                      stringr::str_squish() %>%
                      textclean::replace_url() %>%
                      stringr::str_to_lower() %>%
                      gsub("[[:punct:]]+", " ", .) %>%
                      stringr::str_squish()
      ) -> df_json
      
    json %>%
      roomba::roomba(cols = c("postalCode", "streetAddress", 
                              "latitude", "longitude", "price"), keep = any) %>%
      dplyr::as_tibble() %>%
      purrr::pmap(~ c(...)) %>%
      purrr::map(~ .[!is.na(.)]) %>%
      purrr::flatten_chr() %>%
      dplyr::bind_rows() -> df_json2
    
    # combine scraped data into data frame
    df <- dplyr::bind_cols(bbs, type, df_json, df_json2)
    
  }, error = function(e) {cat("ERROR :", conditionMessage(e), "\n")})
  
  df_all %>%
    dplyr::bind_rows(df) -> df_all
  readr::write_csv(df_all,  here::here(paste0("data/uncleaned_data/", 
                                              lubridate::floor_date(Sys.Date(), unit = "month"), 
                                              "/", 
                                              Sys.Date(),
                                              "/liv_rent.csv")))
  
}

# read in uncleaned data
out <-  readr::read_csv(here::here(paste0("data/uncleaned_data/", 
                                          lubridate::floor_date(Sys.Date(), unit = "month"), 
                                          "/", 
                                          Sys.Date(),
                                          "/liv_rent.csv")))

# filter out missing locations and only keep distinct rows
out %>%
  dplyr::distinct(bed_bath, name, price, floorSize, .keep_all = TRUE) %>%
  dplyr::filter(!is.na(latitude)) -> out

out %>%
  dplyr::mutate(bedroom = ifelse(is.na(bedroom), bedrooms, bedroom),
                bathroom = ifelse(is.na(bathroom), bathrooms, bathroom),
                furnished = ifelse(is.na(furnished), unfurnished, furnished),
                furnished = ifelse(is.na(furnished), furnish, furnished),
                bedroom = ifelse(is.na(bedroom) & !is.na(studio), 0, bedroom),
                bathroom = ifelse(is.na(bathroom) & (bedroom == 1 | bedroom == 0), 1, bathroom)) %>%
  dplyr::select(-c(bedrooms, bathrooms, furnish, unfurnished, `@type`, 
                   floorSize, private, studio, streetAddress)) %>% 
  dplyr::mutate_at(vars(ft, bedroom, bathroom), ~ readr::parse_number(.)) %>% 
  dplyr::mutate(bathroom = ifelse(is.na(bathroom) & bedroom == 1, 1, bathroom)) %>% 
  janitor::clean_names() %>% 
  dplyr::rename(lon = longitude, lat = latitude, sqft = ft, bed = bedroom,
                bath = bathroom, addres = name) %>% 
  na.omit() %>%
  dplyr::mutate(type = dplyr::case_when(stringr::str_detect(type, "House") ~ "house",
                                        stringr::str_detect(type, "Apartment") ~ "apartment",
                                        stringr::str_detect(type, "Room") ~ "room",
                                        stringr::str_detect(type, "Townhouse") ~ "townhouse",
                                        TRUE ~ as.character(type))) %>%
  dplyr::mutate_if(is.character, tolower) %>% 
  na.omit() -> out
 
# write csv
readr::write_csv(out,  here::here(paste0("data/cleaned_data/", 
                                         lubridate::floor_date(Sys.Date(), unit = "month"), 
                                         "/", 
                                         Sys.Date(),
                                         "/liv_rent.csv")))
