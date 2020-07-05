library(tidyverse)
library(geosphere)

# read in all cleaned housing data
source("scripts/combining_dfs.R")
table(combined_all$type)

# read in google maps data
list.files("data/google_places") %>%
  purrr::map(~ cbind(readr::read_csv(paste0("data/google_places/", .)), 
                     data.frame(place = paste0(stringr::str_remove(., "\\.csv"))))) %>%
  dplyr::bind_rows() %>%
  dplyr::select(-c(id, rating, neighborhood, formatted_address)) -> google_places

# keep ratings that have more than 10 reviews
google_places %>%
  dplyr::mutate(name = stringr::str_to_lower(name)) %>%
  dplyr::filter(!(place == "beach" & stringr::str_detect(name, "\\bbeach\\b", negate = TRUE))) %>% 
  dplyr::filter(user_ratings_total > 10) -> google_places

# list of big supermarkets in Canada
big_supermarkets <- c("superstore", "t&t", "safeway", "walmart", "whole foods", "save-on-foods", "loblaws")
#------------------------------------------------------------------------------------

gp <- google_places %>%
  dplyr::filter(!place %in% c("bars", "coffee", "restaurant"))

dist_place <- vector(mode = "list", length = nrow(combined_all))
for(i in 1:nrow(combined_all)) {
  
  print(i)
  # get lat lng from housing data
  combined_all %>%
    dplyr::as_tibble() %>%
    dplyr::select(lon, lat) %>%
    as.matrix() -> ll_df
  
  # get lat lng from google places
  gp %>%
    dplyr::as_tibble() %>%
    dplyr::select(lng, lat) %>%
    as.matrix() -> ll_google
  
  # calculate distance in km and extract nearest distance of each place (mall, skytrain etc.)
  # in google places df
  geosphere::distm(ll_google, ll_df[i, ], fun = distHaversine) %>% 
    dplyr::as_tibble() %>%
    purrr::set_names("km") %>% 
    dplyr::mutate(km = km / 1000) %>% 
    dplyr::bind_cols(gp %>%
                       dplyr::select(-c(lat, lng))) %>%
    dplyr::group_by(place) %>%
    dplyr::arrange(km) %>% 
    dplyr::distinct(place, .keep_all = TRUE) %>%
    dplyr::select(km, user_ratings_total, place) %>%
    pivot_wider(names_from = "place", values_from = c("km", "user_ratings_total")) %>% 
    dplyr::select(-c(user_ratings_total_skytrain, user_ratings_total_schools)) -> nearest
  #--------------------------------------------------------------------------------------------
  
  # find nearest distance to highly valued places for malls, beaches, supermarkets, and grocery stores
  gp %>%
    dplyr::filter(place == "grocery_stores" & stringr::str_detect(name, paste0(big_supermarkets, collapse = "|")) |
                    place != "grocery_stores") %>% 
    dplyr::filter(place == "mall" & user_ratings_total > 1000 | place != "mall") %>%
    dplyr::filter(place == "beach" & user_ratings_total > 500 | place != "beach") %>%
    dplyr::filter(place == "parks" & user_ratings_total > 500 | place != "parks") -> nearest_big_place
  
  nearest_big_place %>% 
    dplyr::as_tibble() %>%
    dplyr::filter(place != "skytrain") %>%
    dplyr::filter(place != "schools") %>%
    dplyr::select(lng, lat) %>%
    as.matrix() -> ll_google
  
  geosphere::distm(ll_google, ll_df[i, ], fun = distHaversine) %>% 
    dplyr::as_tibble() %>%
    purrr::set_names("km") %>% 
    dplyr::mutate(km = km / 1000) %>% 
    dplyr::bind_cols(nearest_big_place %>%
                       dplyr::select(-c(lat, lng)) %>%
                       dplyr::filter(place != "skytrain") %>%
                       dplyr::filter(place != "schools")) %>%
    dplyr::group_by(place) %>%
    dplyr::arrange(km) %>% 
    dplyr::distinct(place, .keep_all = TRUE) %>%
    dplyr::select(km, user_ratings_total, place) %>%
    pivot_wider(names_from = "place", values_from = c("km", "user_ratings_total")) %>% 
    purrr::set_names(paste0("big_", names(.))) %>% 
    dplyr::bind_cols(nearest) -> nearest_place
  
  dist_place[[i]] <- nearest_place
  
}

dist_place %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(ID = dplyr::row_number()) %>% 
  dplyr::bind_cols(combined_all) -> combined_all
#------------------------------------------------------------------------------------------

# get number of restaurants, bars, and coffee shops that fall into
# a radius of 750m of an address 
google_places %>%
  dplyr::filter(place %in% c("bars", "restaurants","coffee")) -> gastronomy_places

gast_points <- gastronomy_places %>%
  dplyr::select(lng, lat) %>%
  as.matrix()

counts <- vector(mode = "integer", length = nrow(combined_all))
for(i in seq_along(counts)) {

  print(i)  
  geosphere::distHaversine(
    gastronomy_places %>%
      dplyr::select(lng, lat), 
    c(combined_all$lon[[i]], combined_all$lat[[i]])
  ) < 750 -> x
  
  counts[[i]] <- sum(x)
  
}


combined_all %>%
  dplyr::mutate(number_venues = counts) -> combined_all
combined_all %>%
  dplyr::filter_at(vars(lat, lon, bed, bath, sqft), any_vars(!is.na(.))) -> combined_all

readr::write_csv(combined_all, "final.csv")


