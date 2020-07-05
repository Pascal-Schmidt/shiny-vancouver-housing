augment_ml_df <- function(df, google_places = google_places, gp = gp, 
                          nearest_big_place = nearest_big_place, 
                          gast_points = gast_points) {
  
  # list of big supermarkets in Canada
  big_supermarkets <- c("superstore", "t&t", "safeway", "walmart", "whole foods", "save-on-foods", "loblaws")
  
  # get lat lng 
  df %>%
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
  geosphere::distm(ll_google, ll_df, fun = distHaversine) %>%
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
  
  nearest_big_place %>%
    dplyr::as_tibble() %>%
    dplyr::filter(place != "skytrain") %>%
    dplyr::filter(place != "schools") %>%
    dplyr::select(lng, lat) %>%
    as.matrix() -> ll_google
  
  geosphere::distm(ll_google, ll_df, fun = distHaversine) %>%
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
  
  nearest_place %>%
    dplyr::bind_cols(df) -> df
  #------------------------------------------------------------------------------------------
  
  # get number of restaurants, bars, and coffee shops that fall into
  # a radius of 750m of an address
  geosphere::distHaversine(
    gast_points %>%
      dplyr::select(lng, lat),
    c(df$lon, df$lat)
  ) < 750 -> x
  
  counts <- sum(x)
  
  df %>%
    dplyr::mutate(number_venues = counts) -> df
  
  return(df)
  
}
