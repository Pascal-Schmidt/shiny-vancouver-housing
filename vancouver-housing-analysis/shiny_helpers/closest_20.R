closest_20 <- function(df, user_input) {
  
  # get lat and lon from user input
  lat_lon <- ggmap::geocode(user_input) %>%
    as.matrix()
  
  df %>%
    dplyr::select(lon, lat) %>%
    as.matrix() -> all_homes
  
  # get distance and keep 20 closest ones
  geosphere::distm(all_homes, lat_lon, fun = distHaversine) %>%
    purrr::set_names(1:length(.)) %>%
    sort(decreasing = F) %>%
    .[1:20] %>%
    names() %>%
    df[., ] -> closest_20
  
}