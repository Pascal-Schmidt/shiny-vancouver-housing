# read in mongo data
read_mongo <- function(db = "vancouver-housing", collection = "van", 
                       host,
                       username,
                       password) {
  
  # connect to mongo database
  mongolite::mongo(
    collection = collection,
    url        = stringr::str_glue("mongodb+srv://{username}:{password}@{host}/{db}")
  ) -> mongo_connection
  
  # get all user data
  mongo_data <- mongo_connection$find() %>% as_tibble()
  
  # transform user data column to tibble
  mongo_data$data_user %>%
    purrr::map(~ purrr::map_dfr(., ~ unlist(.))) -> mongo_data$data_user
  
  # mongo_data into global environment
  mongo_data <<- mongo_data
  
  # disconnect from mongo database
  mongo_connection$disconnect()
  
}

# update mongo data
update_mongo <- function(db = "vancouver-housing", collection = "van", 
                         host,
                         username,
                         password,
                         user, password1, mongo_data) {
  
  # connect to mongo database
  mongolite::mongo(
    collection = collection,
    url        = stringr::str_glue("mongodb+srv://{username}:{password}@{host}/{db}")
  ) -> mongo_connection
  
  query <- stringr::str_c('{"user": "', user, '", "password": "', password1, '"}')
  
  # append rows to tibble
  mongo_data$data_user %>%
    purrr::pluck(1) %>%
    list() -> mongo_data$data_user
  
  mongo_data %>%
    dplyr::select(data_user) %>%
    rjson::toJSON() -> updated_tibble
  
  mongo_connection$update(query = query,
                          update = stringr::str_c('{"$set": ', updated_tibble, '}'))
  
  mongo_connection$disconnect()
  
}
x
