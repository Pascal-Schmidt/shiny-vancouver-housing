library(tidyverse)
library(rvest)
library(textclean)
library(config)

Sys.setenv(R_CONFIG_ACTIVE = "default")
config <- config::get(file = "google_api_key.yml")

# only scrape new links
source(here::here("scripts/helper_fn/current_links.R"))

# create folders
source(here::here("scripts/helper_fn/create_folder.R"))
create_folders()

# bed(s), bath(s) function
bb <- function(url, path) {
  
  url %>%
    html_nodes("ul") %>%
    rvest::html_nodes(path) %>%
    rvest::html_text() %>%
    .[[1]] %>%
    readr::parse_number() -> temp
    
    if(length(temp) == 0) {
      
      temp <- NA
      return(temp)
      
    } else {
      
      return(temp)
      
    }
  
}

base_url <- "https://www.point2homes.com/CA/Condos-For-Sale/BC/Vancouver/Downtown-Vancouver.html"
pages <- paste0(base_url, "?page=", 2:11)

all_links <- list()
for(i in seq_along(pages)) {
  
  print(i)
  Sys.sleep(runif(1, 5, 8))
  url <- xml2::read_html(pages[[i]])
  url %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href") %>%
    as.character() %>%
    stringr::str_extract_all("CA.*For-Sale/BC/Vancouver/Downtown-Vancouver/[0-9]+.*") %>%
    purrr::flatten_chr() %>%
    unique() %>%
    na.omit() %>%
    paste0("https://www.point2homes.com/", .) -> all_links[[i]]
  
  all_links %>%
    purrr::flatten_chr() %>%
    unique() %>%
    {dplyr::tibble(url = .)} %>%
    here::here(paste0("data/links/", 
                      lubridate::floor_date(Sys.Date(), unit = "month"), 
                      "/", 
                      Sys.Date(),
                      "/p2h.csv"))
    
  
}


# df_all <- readr::read_csv(here::here(paste0("data/uncleaned_data/", 
#                                             lubridate::floor_date(Sys.Date(), unit = "month"), 
#                                             "/", 
#                                             Sys.Date(),
#                                             "/p2h.csv"))) %>%
#   dplyr::mutate_all(as.character)


all_links_p2h <- current_links("p2h")
df_all <- data.frame()
for(i in 1:length(all_links_p2h)) {
  
  tryCatch({
    
    print(i)
    url <- xml2::read_html(all_links_p2h[[i]])
    
    Sys.sleep(runif(1, 5, 15))
    # street address + postal code
    url %>%
      rvest::html_nodes(".address-container") %>%
      rvest::html_text() %>%
      stringr::str_remove_all(., "\\\n|\\\\n|\\\r") %>%
      textclean::add_comma_space() %>%
      stringr::str_replace("British Columbia", "British Columbia ") %>%
      stringr::str_squish() -> address
    
    # home price
    url %>%
      rvest::html_nodes(".price") %>%
      .[[1]] %>%
      rvest::html_text() %>%
      readr::parse_number() -> price
    
    bath <- bb(url, path =  ".ic-baths")
    bed <- bb(url, path = ".ic-beds")
    sqft <- bb(url, path = ".ic-sqft")
    
    # characteristics
    url %>%
      rvest::html_nodes(".details-charcs") %>%
      rvest::html_nodes("dt") %>%
      rvest::html_text() -> col_names
    
    url %>%
      rvest::html_nodes(".details-charcs") %>%
      rvest::html_nodes("dd") %>%
      rvest::html_text() %>%
      stringr::str_remove_all(pattern = "\r\n") %>%
      stringr::str_squish() -> characteristics
    
    characteristics %>%
      purrr::set_names(col_names) %>%
      dplyr::bind_rows() -> characteristics
    
    # characteristics
    url %>%
      rvest::html_nodes(".description-full-cnt") %>%
      rvest::html_text() %>%
      stringr::str_remove_all(., '\\\n|\\\\n|\\\r|"') %>%
      stringr::str_squish() %>%
      stringr::str_to_lower() %>%
      gsub("[[:punct:]]+", " ", .) %>%
      stringr::str_squish() -> description
    
    data.frame(address = address,
               price = price,
               sqft = sqft,
               bed = bed,
               bath = bath,
               description = description,
               url = all_links_p2h[[i]]) %>%
      dplyr::bind_cols(characteristics) -> df
    
  }, error = function(e) {cat("ERROR :", conditionMessage(e), "\n")} )
  
  df_all <- df_all %>%
    dplyr::bind_rows(df)
  
  readr::write_csv(df_all, here::here(paste0("data/uncleaned_data/", 
                                             lubridate::floor_date(Sys.Date(), unit = "month"), 
                                             "/", 
                                             Sys.Date(),
                                             "/p2h.csv")))
  
}

df_all %>%
  dplyr::as_tibble() %>%
  janitor::clean_names() %>% 
  dplyr::select(-c(type, sub_type, neighborhood, style,
                   association_fee, mls_number, title)) %>% 
  dplyr::rename("type" = building_type) %>%
  na.omit() -> df_all

address <- df_all$address
ggmap::register_google(config$google_api_key)
lat_lon <- vector(mode = "list", length = length(address))
for(i in 1:length(address)) {
  
  Sys.sleep(0.5)
  print(i)
  lat_lon[[i]] <- ggmap::geocode(address[[i]],
                                 output = "more")
  
}

lat_lon %>%
  dplyr::bind_rows() %>%
  dplyr::select(lon,lat) %>%
  dplyr::bind_cols(df_all) %>% 
  readr::write_csv(here::here(paste0("data/cleaned_data/", 
                                     lubridate::floor_date(Sys.Date(), unit = "month"), 
                                     "/", 
                                     Sys.Date(),
                                     "/p2h.csv")))

