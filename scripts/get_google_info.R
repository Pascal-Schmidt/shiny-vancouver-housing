library(tidyverse)
library(googleway)
library(sf)
library(config)

Sys.setenv(R_CONFIG_ACTIVE = "default")
config <- config::get(file = "google_api_key.yml")
api_key <- config$google_api_key
neighborhoods <- sf::read_sf("data/shapefiles/local-area-boundary.shp") %>%
  dplyr::pull(name)

# function to extract data frame "results" from list and
# select specific columns
get_google_info <- function(df) {
  
  df %>%
    purrr::pluck("results", "geometry", "location") %>%
    dplyr::bind_cols(
      purrr::pluck(df, "results") %>%
        dplyr::select(rating, user_ratings_total, name, formatted_address) 
    ) -> google
  
  return(google)
  
}

# function to get places
get_google_places <- function(query, api_key, safe_to) {
  
  place <- query
  name <- vector(mode = "list", length = length(place))
  for(i in seq_along(place)) {
    
    name_1 <- google_places(search_string = place[[i]],
                            key = api_key)
    
    Sys.sleep(2)
    name_2 <- google_places(search_string = place[[i]],
                            page_token = purrr::pluck(name_1, "next_page_token"),
                            key = api_key)
    
    Sys.sleep(2)
    name_3 <- google_places(search_string = place[[i]],
                            page_token = purrr::pluck(name_2, "next_page_token"),
                            key = api_key)
    
    list(name_1, name_2, name_3) %>%
      purrr::map(~ get_google_info(.)) %>%
      dplyr::bind_rows(.id = "id") %>%
      cbind(data.frame(neighborhood = neighborhoods[[i]])) -> name[[i]]
    
  }
  
  name %>%
    dplyr::bind_rows() %>% 
    dplyr::arrange(desc(user_ratings_total, rating)) %>% 
    readr::write_csv(safe_to)
  
}



# get all skytrain stations in vancouver
skytrain_1 <- google_places(search_string = "skytrain station vancouver",
                            key = "AIzaSyAui-Y39y609FrTUC2dNuYagemigg4iHSo")

Sys.sleep(2)
skytrain_2 <- google_places(search_string = "skytrain station vancouver",
                            page_token = skytrain_1$next_page_token,
                            key = "AIzaSyAui-Y39y609FrTUC2dNuYagemigg4iHSo")

list(skytrain_1, skytrain_2) %>%
  purrr::map(~ get_google_info(.)) %>%
  dplyr::bind_rows() %>% 
  dplyr::distinct(name, .keep_all = TRUE) %>% 
  dplyr::filter(stringr::str_detect(formatted_address, pattern = "Vancouver")) %>%
  dplyr::arrange(desc(user_ratings_total, rating)) %>% 
  dplyr::filter(!(name %in% c("Waterfront Canada Line Station", "Burrard Skytrain Station",
                            "Burrard & Robson", "Waterfront station", "Granville Station"))) %>%
  readr::write_csv("data/google_places/skytrain.csv")
#---------------------------------------------------------------------------------------

# get beaches
beaches_1 <- google_places(search_string = "vancouver beaches",
                           key = "AIzaSyAui-Y39y609FrTUC2dNuYagemigg4iHSo")

beaches_2 <- google_places(search_string = "vancouver beaches",
                           page_token = beaches_1$next_page_token,
                           key = "AIzaSyAui-Y39y609FrTUC2dNuYagemigg4iHSo")

beaches_3 <- google_places(search_string = "vancouver beaches",
                           page_token = beaches_2$next_page_token,
                           key = "AIzaSyAui-Y39y609FrTUC2dNuYagemigg4iHSo")

list(beaches_1, beaches_2, beaches_3) %>%
  purrr::map(~ get_google_info(.)) %>%
  dplyr::bind_rows(.id = "id") %>% 
  dplyr::arrange(desc(user_ratings_total, rating)) %>% 
  dplyr::filter(stringr::str_detect(formatted_address, pattern = "Vancouver")) %>% 
  dplyr::distinct(name, .keep_all = TRUE) %>%
  readr::write_csv("data/google_places/beach.csv")
#-------------------------------------------------------------------------------

# get shopping malls
place <- c(paste0("shopping mall near ", neighborhoods, " Vancouver, BC"), "shopping mall near Vancouver")
safe_to <- "data/google_places/mall.csv"
get_google_places(search_string, api_key, safe_to)
#----------------------------------------------------------------------


# get parks
place <- paste0("park near ", neighborhoods, " Vancouver, BC")
safe_to <- "data/google_places/parks.csv"
get_google_places(search_string, api_key, safe_to)
#----------------------------------------------------------------


# get grocery stores 
place <- paste0("grocery stores near ", neighborhoods, " Vancouver, BC")
safe_to <- "data/google_places/grocery_stores.csv"
get_google_places(search_string, api_key, safe_to)
#---------------------------------------------------------------------------------


# get schools 
search_string <- paste0("schools near ", neighborhoods, " Vancouver, BC")
safe_to <- "data/google_places/schools.csv"
get_google_places(search_string, api_key, safe_to)
#---------------------------------------------------------------------------------


# get bars
search_string <- paste0("bars near ", neighborhoods, " Vancouver, BC")
safe_to <- "data/google_places/bars.csv"
get_google_places(search_string, api_key, safe_to)

readr::read_csv(safe_to) %>% 
  dplyr::distinct(lat, lng, user_ratings_total, name) %>% 
  dplyr::filter(user_ratings_total > 10) %>%
  readr::write_csv(safe_to)
#---------------------------------------------------------------------------------


# get restaurants
search_string <- paste0("restaurants near ", neighborhoods, " Vancouver, BC")
safe_to <- "data/google_places/restaurants.csv"
get_google_places(search_string, api_key, safe_to)

readr::read_csv(safe_to) %>% 
  dplyr::distinct(lat, lng, user_ratings_total, name) %>% 
  dplyr::filter(user_ratings_total > 10) %>%
  readr::write_csv(safe_to)
#---------------------------------------------------------------------------------


# get coffee shops
search_string <- paste0("coffee shops near ", neighborhoods, " Vancouver, BC")
safe_to <- "data/google_places/coffee.csv"
get_google_places(search_string, api_key, safe_to)

readr::read_csv(safe_to) %>% 
  dplyr::distinct(lat, lng, user_ratings_total, name) %>% 
  dplyr::filter(user_ratings_total > 10) %>%
  readr::write_csv(safe_to)
#---------------------------------------------------------------------------------