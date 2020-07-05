library(here)
library(lubridate)

create_folders <- function() {
  
  file_structure <- list(
    here::here(paste0("data/uncleaned_data/", lubridate::floor_date(Sys.Date(), unit = "month"))),
    here::here(paste0("data/cleaned_data/", lubridate::floor_date(Sys.Date(), unit = "month"))),
    here::here(paste0("data/links/", lubridate::floor_date(Sys.Date(), unit = "month")))
  )
  
  # if directory already exists then we move on
  if(!dir.exists(file_structure[[1]])) {
    
    # create a new folder for every new month and year 
    dir.create(file_structure[[1]])
    dir.create(file_structure[[2]])
    dir.create(file_structure[[3]])
    
  }
  
  # if directory already exists then we move on
  if(!dir.exists(paste0(file_structure[[1]], "/", Sys.Date()))){
    
    # create a new folder for the new data that will be scraped
    dir.create(paste0(file_structure[[1]], "/", Sys.Date()))
    dir.create(paste0(file_structure[[2]], "/", Sys.Date()))
    dir.create(paste0(file_structure[[3]], "/", Sys.Date()))
    
  }
  
}