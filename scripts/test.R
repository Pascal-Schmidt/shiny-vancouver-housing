library(tidyverse)

# load geo functions
source("scripts/helper_fn/geo_functions.R")

# sotheby
df <- readr::read_csv("data/cleaned_data/sotheby_homes.csv")
coord <- get_lat_lon(df$address)

coord %>%
  dplyr::bind_rows() %>%
  dplyr::bind_cols(df) %>%
  readr::write_csv("data/cleaned_data/sotheby_homes.csv")

# point 2 home
df <- readr::read_csv("data/cleaned_data/point_2_home.csv")
coord <- get_lat_lon(df$address)

coord %>%
  dplyr::bind_rows() %>%
  dplyr::bind_cols(df) %>%
  readr::write_csv("data/cleaned_data/point_2_home.csv")
