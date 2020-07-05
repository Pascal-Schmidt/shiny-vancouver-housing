library(tidyverse)
library(rvest)
library(httr)
library(ggmap)
library(config)

Sys.setenv(R_CONFIG_ACTIVE = "default")
config <- config::get(file = "google_api_key.yml")
# create new folder for web scraping
source(here::here("scripts/helper_fn/create_folder.R"))
create_folders()

base <- "https://vancouver.craigslist.org/search/apa"
pages <- c(base, paste0(base, "?s=", seq(from = 120, to = 2880, by = 120)))

all_links <- list()
for(i in 1:length(pages)) {
  
  print(i)
  pages[[i]] %>%
    xml2::read_html() %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href") %>%
    
    # only get listings from Vancouver
    .[stringr::str_detect(., "van/apa")] -> all_links[[i]]
  
  Sys.sleep(runif(1, min = 2, max = 4))
  
}

# save all links as csv
all_links %>%
  purrr::flatten_chr() %>%
  unique() %>%
  {dplyr::tibble(url = .)} %>%
  readr::write_csv(here::here(paste0("data/links/", 
                                     lubridate::floor_date(Sys.Date(), unit = "month"), 
                                     "/", 
                                     Sys.Date(),
                                     "/craigslist.csv")))

# read in links
all_links <- readr::read_csv(here::here(paste0("data/links/", 
                                               lubridate::floor_date(Sys.Date(), unit = "month"), 
                                               "/", 
                                               Sys.Date(),
                                               "/craigslist.csv"))) %>%
  dplyr::pull()


# base directory links
directory <- paste0(here::here(), "/data/links/")

# list all craigslist links previoulsy scraped
directory %>%
  list.files(recursive = TRUE) %>%
  .[stringr::str_detect(., "craigslist")] %>%
  .[stringr::str_detect(., as.character(Sys.Date()), negate = TRUE)] -> previous_links
  
# list all links scraped today
directory %>%
  list.files(recursive = TRUE) %>%
  .[stringr::str_detect(., "craigslist")] %>%
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

# df_all <- readr::read_csv(here::here("data/uncleaned_data/2020-06-01/2020-06-06/craigslist.csv"))
df_all <- data.frame()
for(i in 1:length(all_links)) {
  
  # progress
  print(i)
  beepr::beep()
  
  tryCatch({
    url <- xml2::read_html(all_links[[i]])
    
    
    Sys.sleep(runif(1, min = 5, max = 12))
    
    url %>%
      rvest::html_nodes(".price") %>%
      rvest::html_text() -> price
    
    # bed, bath, sqft
    url %>%
      rvest::html_nodes(".shared-line-bubble") %>%
      rvest::html_text() %>%
      stringr::str_split("/") %>%
      purrr::flatten_chr() %>%
      stringr::str_squish() %>%
      purrr::set_names(stringr::str_remove_all(., "[0-9]+")) %>%
      dplyr::bind_rows() -> bb_sqft
    
    # address
    url %>%
      rvest::html_nodes(".mapaddress") %>%
      rvest::html_text() %>%
      stringr::str_remove_all('(\\n|\\")') %>%
      stringr::str_squish() %>%
      { ifelse(length(.) == 0, NA, .[[1]]) } -> address
    
    # furnished and type
    url %>%
      rvest::html_nodes(".attrgroup") %>%
      rvest::html_text() -> temp
    
    if(length(temp) == 0) {
      
      furnished = NA
      
    } else {
      
      temp %>%
        purrr::when(length(.) == 1 ~ .[[1]],
                    ~ .[c(2:length(.))]) %>%
        stringr::str_split("\\\n") %>%
        purrr::flatten_chr() %>%
        stringr::str_remove_all('//"') %>%
        stringr::str_squish() %>%
        .[. != ""] %>%
        stringr::str_to_lower() -> f_t
      
      # furnished
      f_t %>%
        .[which(stringr::str_detect(., "(\\bfurnished\\b|\\bunfurnished\\b)"))] %>%
        { ifelse(length(.) == 0, NA, .) } -> furnished
      
      # housing type
      f_t %>%
        .[which(stringr::str_detect(., "(open|date|dates)", negate = TRUE))] %>%
        .[which(stringr::str_detect(., "(\\bloft\\b|\\bduplex\\b|\\bflat\\b|\\btownhouse\\b|\\bhouse\\b|\\bcondo\\b|\\bapartment\\b)"))] %>%
        { ifelse(length(.) == 0, NA, .) } -> type
      
    }
    
    
    # description
    url %>%
      rvest::html_nodes("#postingbody") %>%
      rvest::html_text() %>%
      stringr::str_remove_all("\\\n") %>%
      stringr::str_squish() %>%
      textclean::replace_url() %>%
      stringr::str_to_lower() %>%
      gsub("[[:punct:]]+", " ", .) %>% 
      gsub("qr code link to this post", "", .) %>%
      stringr::str_squish() -> description
    
    df_all %>%
      dplyr::bind_rows(
        data.frame(price = price, address = address, description = description, 
                   type = type, furnished = furnished, url = all_links[[i]], data = Sys.Date()) %>%
          dplyr::bind_cols(bb_sqft) 
      ) -> df_all
    
  }, error = function(e) {cat("ERROR :", conditionMessage(e), "\n")} )
  
  readr::write_csv(df_all, here::here(paste0("data/uncleaned_data/", 
                                             lubridate::floor_date(Sys.Date(), unit = "month"), 
                                             "/", 
                                             Sys.Date(),
                                             "/craigslist.csv")))
  
}

# uncleaned data set from web scraper
df_all <- readr::read_csv(here::here(paste0("data/uncleaned_data/", 
                                            lubridate::floor_date(Sys.Date(), unit = "month"), 
                                            "/", 
                                            Sys.Date(),
                                            "/craigslist.csv"))) 

df_all %>%
  
  dplyr::filter(!is.na(ft)) %>%
  dplyr::filter(!is.na(address)) %>%
  dplyr::filter(address != "(google map)") %>% 
  dplyr::select(-c(dplyr::contains("available"))) -> df_all
  
bath_names <- colnames(df_all[, (which(colnames(df_all) == "ft") + 1):length(df_all)])

df_all %>%
  tidyr::unite(col = bath_2, bath_names, sep = "") %>% 
  dplyr::mutate(bath_2 = stringr::str_remove_all(bath_2, "NA")) -> df_all

df_all %>%
  
  # some data cleaning
  dplyr::mutate(Ba = ifelse(is.na(Ba), bath_2, Ba)) %>% 
  dplyr::select(-bath_2) %>% 
  
  # coarce to number
  dplyr::mutate_at(vars(ft, BR, Ba, price), ~ readr::parse_number(.)) %>% 
  
  # get furnished or unfurnished information from description
  dplyr::mutate(furnished = ifelse(stringr::str_detect(description, "\\b.*unfurnished.*\\b") & is.na(furnished),
                                   "unfurnished", furnished)) %>%
  dplyr::mutate(furnished = ifelse(stringr::str_detect(description, "\\b.*furnished.*\\b") & is.na(furnished),
                                   "furnished", furnished)) %>% 
  dplyr::mutate(furnished = ifelse(is.na(furnished), "unfurnished", furnished)) %>% 
  
  # rename columns
  dplyr::rename("bed" = BR, "bath" = Ba, "sqft" = ft) %>%
  
  # only keep distinct rows
  dplyr::distinct(price, address, type, furnished, bed, bath, sqft, .keep_all = T) %>% 
  
  dplyr::filter(type != "open house dates") %>%
  
  na.omit() -> df_all
#-----------------------------------------------------------------------------------------------------

# geocoding process
address <- df_all$address %>%
  paste0(", Vancouver, BC")

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
  dplyr::select(lon,lat, address) %>%
  dplyr::bind_cols(df_all) %>% 
  dplyr::select(-address1) %>% 
  readr::write_csv(here::here(paste0("data/cleaned_data/", 
                                     lubridate::floor_date(Sys.Date(), unit = "month"), 
                                     "/", 
                                     Sys.Date(),
                                     "/craigslist.csv")))
