library(tidyverse)
library(sf)

boundaries <- read_sf("data/shapefiles/local-area-boundary.shp")

list.files(paste0("data/cleaned_data/",
                  lubridate::floor_date(Sys.Date(), unit = "month"),
                  "/",
                  Sys.Date()-1)) %>%
  paste0(paste0("data/cleaned_data/",
                lubridate::floor_date(Sys.Date(), unit = "month"),
                "/",
                Sys.Date()-1,
                "/"), .) %>%
  purrr::map(~ cbind(readr::read_csv(.),
                     dplyr::tibble(website = .))) %>%
  dplyr::bind_rows() %>% 
  dplyr::mutate_at(vars(description, type, furnished), ~ stringr::str_to_lower(.)) %>% 
  dplyr::mutate(website = stringr::str_remove_all(website, paste0("(data/cleaned_data/",
                                                                  lubridate::floor_date(Sys.Date(), unit = "month"),
                                                                  "/",
                                                                  Sys.Date()-1,
                                                                  "/", "|\\.csv)"))) %>% 
  dplyr::mutate(year_built = ifelse(website == "rew", 2020 - as.numeric(year_built), year_built)) %>% 
  dplyr::select(-c(contains("taxes"), data, date)) -> combined_df

combined_df %>%
  dplyr::mutate_at(vars(price, lat, lon, bed, bath, sqft, year_built),
                   ~ as.numeric(.)) %>% 
  dplyr::filter_at(vars(lat, lon, bed, bath, sqft), any_vars(!is.na(.))) -> combined_df

# prepare for joining
sf_point_df <- sf::st_as_sf(x = combined_df %>%
                              dplyr::filter(!is.na(lon) | !is.na(lat)), 
                            coords = c("lon", "lat"),
                            crs = st_crs(boundaries))

# fill out missing values in the district column where latitude and 
# longitude points fall into the polygon
combined_df <- sf::st_join(sf_point_df, boundaries, join = st_intersects) 

combined_df %>%
  mutate(lon = unlist(purrr::map(.$geometry, 1)),
         lat = unlist(purrr::map(.$geometry, 2))) %>%
  dplyr::select(-c(name, geometry)) -> combined_df

boundaries %>%
  dplyr::as_tibble() %>%
  dplyr::inner_join(combined_df, by = "mapid") %>%
  dplyr::distinct(price, address, sqft, .keep_all = TRUE) %>%
  sf::st_sf(sf_column_name = 'geometry.x') %>%
  dplyr::select(-geometry.y) %>%
  dplyr::rename("geometry" = geometry.x) -> combined_all
