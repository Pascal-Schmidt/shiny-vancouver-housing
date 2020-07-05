library(shiny)
library(leaflet)
library(dplyr)
library(purrr)
library(sf)
library(readr)
library(data.table)
library(shinyWidgets)
library(ranger)
library(recipes)
library(yardstick)
library(parsnip)
library(workflows)
library(dials)
library(geosphere)
library(googleway)
library(tidyr)
library(RColorBrewer)
library(scales)
library(fresh)
library(shinyjs)
library(ggmap)
library(shinyauthr)
library(config)
library(mongolite)
library(rjson)
# mongodb+srv://Pascal28:Sasch07Pas93@cluster0-2a6ug.mongodb.net/<dbname>?retryWrites=true&w=majority
# initial_values <- dplyr::tibble()

Sys.setenv(R_CONFIG_ACTIVE = "default")
config <- config::get(file = "mongo_configs.yml")

# 1.0 Data, Functions, and Model loading ----

# get Google maps api key
ggmap::register_google(key = config$google_api_key)


# 1.1 Load Google Data ----
google_places <- readr::read_csv("data/google_places.csv")
gp <- readr::read_csv("data/gp.csv")
nearest_big_place <- readr::read_csv("data/nearest_big_place.csv")
gast_points <- readr::read_csv("data/gast_points.csv")

# 1.2 Load Housing Data ----
df <- readr::read_csv("data/df.csv") %>%
  dplyr::mutate(date = Sys.Date()) %>%
  dplyr::mutate(price_to_rent = round(price_to_rent, 1)) %>%
  dplyr::mutate(url = ifelse(stringr::str_detect(url, "point2homes"), "", url))

# 1.3 Load RF Models ----
homes_rf <- readRDS("ml-models-data/homes_rf.rds")
rentals_rf <- readRDS("ml-models-data/rentals_rf.rds")
homes_rentals_rf <- readRDS("ml-models-data/homes_rentals_rf.rds")
rentals_homes_rf <- readRDS("ml-models-data/rentals_homes_rf.rds")

# 1.4 Source Functions ----
# read data augmentation function for predictions
source("shiny_helpers/modeling.R")
# read leaflet map function
source("shiny_helpers/base_map.R")
# read 20 closest homes to address
source("shiny_helpers/closest_20.R")
# read in ui info card function
source("shiny_helpers/info_card.R")
# mongo functions
source("shiny_helpers/mongo_functions.R")

current_properties <- dplyr::tibble()

# 2.0 Start UI ----
ui <- bootstrapPage(
  
  shinyjs::useShinyjs(),
  
  div(id = "login_ui",
    shinyauthr::loginUI(id = "login",
                        title = h1(class = "text-center", "Vancouver Housing Market"))
  ),
  
  div(id = "website",
  # Initialize shinyJS
  
  # 2.1 Choose Shiny Theme ----
  # choose shiny theme
  use_theme(create_theme(
    theme = "flatly",
    bs_vars_panel(
      bg = "#0006",
      default_heading_bg = "black",
      default_text = "white"
    )
  )),
  
  navbarPage(
    title = "Vancouver Housing Market",
    
    # 2.2 Tab Panel MAP ----
    tabPanel("Map",
             div(class = "outer",
                 
                 tags$head(
                   # Include our custom CSS
                   includeCSS("styles.css")
                 ),
                 
                 # leaflet map 
                 leafletOutput("map", width = "100%", height = "100%"),
                 
                 # 2.2.1 Absolute panel ----
                 absolutePanel(
                   style     = "top:20px;left:20px;",
                   draggable = FALSE, 
                   div(
                     id = "controls",
                     class = "panel panel-default",
                     div(
                       class="panel-body",
                       style = "padding-bottom: 0px;",
                       # text address input
                       div(
                         shiny::textInput(
                           inputId = "street_address", 
                           value   = NULL, 
                           label   = "Street Address",
                           placeholder = "Address, City, State, Zip"
                           )
                       ),
                       
                       
                       # 2.2.1.1 Action Buttons ----
                       div(
                         shiny::actionButton(
                           inputId = "submit",
                           label   = "Search", 
                           class   = "btn-default",
                           icon    = shiny::icon("search") 
                         ),
                         # search for housing or rentals
                         shiny::checkboxGroupInput("type", "Homes | Rentals",
                                                   choiceNames =
                                                     list(
                                                       shiny::icon("home"),
                                                       shiny::icon("city")
                                                     ),
                                                   choiceValues = list("buying", "rental"),
                                                   selected = c("buying", "rental"),
                                                   inline = TRUE
                         ) %>% div(class="pull-right")
                       )
                     ),
                       
                       div(
                         class = "panel-body",
                         style = "padding-top: 0px;",
                         shiny::actionButton(
                           inputId = "toggle",
                           label   = NULL,
                           class   = "btn-default",
                           icon    = shiny::icon("cog") 
                         ),
                         shiny::actionButton(
                           inputId = "refresh",
                           label   = NULL,
                           class   = "btn-default",
                           icon    = shiny::icon("refresh") 
                         ) %>% div(class="pull-right")
                     ),
                     
                     # 2.2.1.2 Hidden Sliders ----
                     div(
                       id = "show",
                       style = "color:#f5f5f5; padding-top: 15px; padding-bottom: 15px; padding-right: 15px; padding-left: 15px;",
                       shinyWidgets::chooseSliderSkin("Modern", color = "LightSlateGrey"),

                       
                       shiny::sliderInput("ptr", "Price to Rent",
                                          min = round(min(df$price_to_rent, na.rm = TRUE), 1),
                                          max = round(max(df$price_to_rent, na.rm = TRUE), 1),
                                          value = c(
                                            min(df$price_to_rent, na.rm = TRUE),
                                            max(df$price_to_rent, na.rm = TRUE)
                                          )
                       ),
                       
                       shiny::sliderInput("Price", "Price of Properties",
                                          min = min(df$price, na.rm = TRUE),
                                          max = max(df$price, na.rm = TRUE),
                                          value = c(
                                            min(df$price, na.rm = TRUE),
                                            max(df$price, na.rm = TRUE)
                                          )
                                          
                       )
                       
                     ) %>%
                       shinyjs::hidden()
                   )
                 )
    )
    ),
    
    
    # 2.3 Tab Panel PREDICTIONS ----
    tabPanel(
      "Real Estate Evaluation",
      
      # 2.3.1 Jumbotron Image ----
      column(
        width = 12,
        div(
          class = "container",
          div(
            class = "jumbotron",
            style = "background-image:url('vancouver-1.jpg'); padding-bottom: 350px; padding-top:75",
            div(
              class = "jumbotron-ui-box text-default bg-primary",
              style = "background-color: rgba(0, 0, 0, 0.7); text-align:center; padding:25px",
              h1("Vancouver Housing Predictions")
            )
          )
        )
        
      ),
      
      # 2.3.2 Intro Text ----
      div(
        class = "container",
        id = "header",
        h1(class = "page-header", "Vancouver Housing Predictions", tags$small("by Pascal Schmidt")),
        p("Looking at a property but you are not sure how it compares to other listings
          in Vancouver? Input your data and you'll see for how much your property would
                                          sell for on average in Vancouver. It also shows if a property you want to purchase is above or below market value. \n
                                          Also, you'll get back information about the predicted monthly rental price and
          about the price to rent ratio. \n
          Try it out and input your property data in the labeled fields.")
      ),
      
      br(),
      br(),
      
      
      div(class = "container",
          shiny::uiOutput("clear")),
      
      div(class = "container",
          shiny::fluidRow(
            column(
              width = 12,
              shiny::uiOutput("show_info_cards")
            )
          )
      ),
      
      br(),
      br(),
      
      
      # user_input_ui_mod(id = "user_input"),
      # # 2.3.3 User Input Housing ----
      div(
        class = "container",
        shiny::fluidRow(
          column(
            4,
            shinyWidgets::textInputAddon(inputId = "price_input",
                                         label   = "Price",
                                         addon   = shiny::icon("money"))
          ),
          column(
            4,
            div(
              shinyWidgets::textInputAddon(inputId = "bed",
                                           label   = "# of Beds",
                                           addon   = shiny::icon("bed"))
            )
          ),
          column(
            4,
            shinyWidgets::textInputAddon(inputId = "bath", 
                                         label   = "# of Baths",
                                         addon   = shiny::icon("bath"))
          )
        ),
        
        shiny::fluidRow(
          column(
            4,
            shinyWidgets::textInputAddon(inputId = "sqft",
                                         label   = "# of Square Feet",
                                         addon   = shiny::icon("paw"))
          ),
          column(
            4,
            shinyWidgets::textInputAddon(inputId = "address_input",
                                         label   = "Address",
                                         addon   = shiny::icon("map"))
          ),
          column(
            4,
            shinyWidgets::textInputAddon(inputId = "year_built", 
                                         label   = "Year Built",
                                         addon   = shiny::icon("calendar"))
          )
        ),
        shiny::fluidRow(
          column(
            4,
            shiny::selectInput(inputId = "neighborhood",
                               label   = "Neighborhood",
                               choices = unique(df$name))
          ),
          column(
            4,
            shiny::selectInput(inputId = "choice", 
                               label   = "House Purchase",
                               choices = c("Buying")
            )
          ),
          column(
            4,
            shiny::selectInput(inputId = "prop_type",
                               label   = "Property Type",
                               choices = c("Condo", "Duplex", "House", "Townhouse")
            )
          )
        )
      ),
      
      # 2.3.4 Action Button Load ----
      div(
        class = "container",
        fluidRow(
          column(
            12,
            actionButton("load_inputs", "Load inputs")
          )
        )
      ),
      
      
      
      br(),
      
      # 2.3.5 Data Tables ----
      div(
        class = "container",

        # tables ----
        fluidRow(
          column(
            12,
            DT::dataTableOutput("inputs")
          )
        ),

        fluidRow(
          column(
            12,
            DT::dataTableOutput("ml_data")
          )
        )
      ),
      
      br(),
      
      # 2.3.6 Action Button Heart
      div(class = "container",
          shiny::fluidRow(
            column(
              width = 3,
              shiny::actionButton(
                inputId = "heart",
                label   = NULL,
                class   = "btn-danger",
                icon    = shiny::icon("heart"),
                `data-toggle` = "popover",
                `title` = "Add to Favourites"
                
              )
            )
          )
      ),
      
      br(),
      br()
  
    )
  )
) %>% shinyjs::hidden()
)

# 3.0 Server ----
server <- function(input, output, session) {
  
  # enable shinyjs
  shinyjs::useShinyjs()
  
  # 1.0 Mongo Data ----
  
  # read in mongo database into global environment
  # data frame is called mongo_data
  read_mongo(db         = "vancouver-housing", 
             collection = "van",
             host       = config$cluster,
             username   = config$username,
             password   = config$password)
  
  # 1.1 Log In ----
  
  # call log in module supplying data frame, user and password cols
  # and reactive trigger
  credentials <- callModule(module   = shinyauthr::login,
                            id       = "login",
                            data     = mongo_data,
                            user_col = user,
                            pwd_col  = password,
                            log_out  = shiny::reactive(logout_init()))

  # call the log out module with reactive trigger to hide/show
  logout_init <- callModule(module = shinyauthr::logout,
                            id     = "logout",
                            active = shiny::reactive(credentials()$user_auth))
  
  # when user has logged in filter the data set for that particular user
  shiny::observe({
    
    if(credentials()$user_auth) {
      
      mongo_data <<- mongo_data %>%
        dplyr::filter(user     == credentials()$info$user,
                      password == credentials()$info$password)
      
    }
    
  })
  
  # 1.2 Render Website ----
  
  # Render website after successful log in from user
  shiny::observe({

    if(credentials()$user_auth) {
      shinyjs::show("website")
      shinyjs::hide("login_ui")
    } else {
      shinyjs::show("login_ui")
      shinyjs::hide("website")
      
    }

  })
  
  # store user input for predictions and more
  reactive_user_inputs <- shiny::reactiveValues()
  # reactive_user_inputs$reactive_properties <- initial_values

  # when user has successfully logged in, then get data frame
  # for a particular user with property information
  shiny::observe({

    if(credentials()$user_auth) {
                                                  # credentials()$info$data_user[[1]]
      reactive_user_inputs$reactive_properties <- mongo_data$data_user[[1]] 

    }

  })
  
  # 3.1 MAP ----
  # 3.1.1 Show/Hide Sliders ----
  shiny::observeEvent(input$toggle, {
    
    shinyjs::toggle("show", anim = TRUE)
    
  })
  
  # 3.1.2 Filtering Data ----
  # 3.1.2.1 House | Rent
  filteredData <- shiny::reactive({
    
    # filter for home purchases ----
    df <- df %>%
      dplyr::filter(website %in% input$type)
    
  })
  
  # 3.1.2.2 Price and Price to Rent ----
  filteredData2 <- shiny::reactive({
    
    # filter for price to rent ratios
    df <- filteredData() %>%
      dplyr::filter(price_to_rent >= input$ptr[1] & price_to_rent <= input$ptr[2])
    
    df <- df %>%
      dplyr::filter(price >= input$Price[1] & price <= input$Price[2])
    
  })
  
  # 3.1.2.3 Closest 20 Homes ----
  filteredData3 <- shiny::eventReactive(input$submit, {
    
    closest_20(df = filteredData2(), user_input = input$street_address)
    
  })
  
  # 3.1.3 Updating Sliders ----
  observe({
    if (length(input$type) > 0) {
      shiny::updateSliderInput(session, "ptr",
                               min = min(filteredData()$price_to_rent, na.rm = TRUE),
                               max = max(filteredData()$price_to_rent, na.rm = TRUE),
                               value = c(
                                 min(filteredData()$price_to_rent, na.rm = TRUE),
                                 max(filteredData()$price_to_rent, na.rm = TRUE)
                               )
      )
    }
  })
  
  observe({
    shiny::updateSliderInput(session, "Price",
                             min = min(filteredData()$price, na.rm = TRUE),
                             max = max(filteredData()$price, na.rm = TRUE),
                             value = c(
                               min(filteredData()$price, na.rm = TRUE),
                               max(filteredData()$price, na.rm = TRUE)
                             )
    )
  })
  
  # 3.1.4 Leaflet Map
  output$map <- renderLeaflet({
    
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically
    leaflet::leaflet(df,
                     options = leafletOptions(zoomControl = FALSE)) %>%
      leaflet::addProviderTiles(providers$CartoDB.Positron, group = "Map 1") %>%
      leaflet::addProviderTiles(providers$Esri.WorldImagery, group = "Map 2") %>%
      leaflet::addProviderTiles(providers$OpenStreetMap, group = "Map 3") %>%
      leaflet::fitBounds(~ -123.269772, ~49.194051, ~ -123.007321, ~49.301576)
  })
  
  # 3.1.4.1 Map filtered general ----
  shiny::observe({
    
    if(credentials()$user_auth & input$refresh == 0) {
      shinyjs::click("refresh")
      base_map(filtered_df = filteredData2(), df = df)
    } else {
      base_map(filtered_df = filteredData2(), df = df)
    }
    
  })
  
  # 3.1.4.2 Map Closest 20 ----
  shiny::observeEvent(input$submit, {
    
    base_map(filtered_df = filteredData3(), df = df)
    
  })
  
  # 3.1.4.3 Map refresh ----
  shiny::observeEvent(input$refresh, {
    
    base_map(filtered_df = filteredData2(), df = df)
    
  })
  
  # 3.2 Prediction Tab ----
  # 3.2.1 User Input ----
  user_inputs <- shiny::eventReactive(input$load_inputs, {

    c(
      Price = input$price_input,
      `Property Type` = as.character(input$prop_type) %>%
        stringr::str_to_lower(),
      `# of Beds` = input$bed,
      `# of Baths` = input$bath,
      `# of Square Feet` = input$sqft,
      Neighborhood = input$neighborhood,
      `Year Built` = input$year_built,
      Address = input$address_input,
      ID = 1
    )

  })
  
  # shiny::callModule(module = "user_input_server_mod",
  #                   id     = "user_input",
  #                   action = shiny::reactive(input$load_inputs)) -> user_inputs
  
  # reactive_user_inputs <- shiny::reactiveValues()
  # reactive_user_inputs$reactive_properties <- initial_values
  # 3.2.2 Condition Modals ----
  # 3.2.2.1 Conditions ----
  # numeric condotional
  condition_numeric <- shiny::eventReactive(input$load_inputs, {
    
    # get all numeric inputs
    user_inputs()[names(user_inputs()) %in% c("# of Beds", 
                                              "# of Baths", 
                                              "# of Square Feet",
                                              "Year Built")] -> all_numeric_inputs
    
    # if not numeric, character will be coerced to NA value
    all_numeric_inputs %>%
      as.numeric() %>%
      purrr::set_names(c("# of Beds", 
                         "# of Baths", 
                         "# of Square Feet",
                         "Year Built")) -> non_numeric_inputs 
    
    condition_numeric <- sum(non_numeric_inputs %in% NA) > 0
    
  })
  
  # Blank Condition Modal 
  condition_blank <- shiny::eventReactive(input$load_inputs, {
    
    condition_blank <- sum(nchar(user_inputs()[-1]) == 0) > 0
    
  })

  # 3.2.2.2 Observe Conditions
  shiny::observe({
    
    # check if input is blank
    if(condition_blank()) {
      
      # check which inputs are blank
      fields_empty <- which(nchar(user_inputs()[-1]) == 0) %>%
        names() %>%
        paste0(collapse = ", ")
      
      # show box with statements
      shiny::modalDialog(
        title = "Fill Out All Fields",
        easyClose = TRUE,
        p(base::ifelse(grepl(",", fields_empty),
                       paste0("Fields ", fields_empty, " are empty. Please fill them out."),
                       paste0("Field ", fields_empty, " is empty. Please fill it out.")))
      ) %>% shiny::showModal()
      
      # check if input is non numeric
    } else if(condition_numeric()) {
      
      # get all numeric inputs
      user_inputs()[names(user_inputs()) %in% c("# of Beds",
                                                "# of Baths",
                                                "# of Square Feet",
                                                "Year Built")] -> all_numeric_inputs
      
      # if not numeric, character will be coerced to NA value
      all_numeric_inputs %>%
        as.numeric() %>%
        purrr::set_names(c("# of Beds",
                           "# of Baths",
                           "# of Square Feet",
                           "Year Built")) -> non_numeric_inputs
      
      
      # get which user inputs are not numeric
      which(is.na(non_numeric_inputs)) %>%
        names() %>%
        paste0(collapse = ", ") -> non_numeric
      
      # show box with statements
      shiny::modalDialog(
        title = "Non Numeric Input",
        easyClose = TRUE,
        p(ifelse(grepl(",", non_numeric),
                 paste0("Fields ", non_numeric, " are not numeric. Please make sure they are numeric inputs."),
                 paste0("Field ", non_numeric, " is not numeric. Please make sure it is a numeric input.")))
      ) %>% shiny::showModal()
      
    }
    
  })

  # 3.2.3 Predictions ----
  ml_output <- shiny::reactive({
    
    if(!condition_numeric() & !condition_blank()) {
      
      api_key <- "AIzaSyAui-Y39y609FrTUC2dNuYagemigg4iHSo"
      # turn named vector into data frame
      inputs_df <- user_inputs() %>%
        dplyr::bind_rows() %>%
        dplyr::mutate_at(vars(Price, `# of Beds`, `# of Baths`, `# of Square Feet`), ~ as.numeric(.))
      
      lat_lon <- googleway::google_geocode(
        address = inputs_df$Address,
        key = api_key,
        simplify = TRUE
      )
      
      
      df <- lat_lon$results$geometry$location %>%
        dplyr::rename("lon" = lng) %>%
        dplyr::bind_cols(inputs_df %>%
                           purrr::set_names(c(
                             "price", "type", "bed",
                             "bath", "sqft", "name",
                             "year_built", "address", "ID"
                           )))
      
      ml_output <- augment_ml_df(df,
                                 google_places = google_places, gp = gp,
                                 nearest_big_place = nearest_big_place,
                                 gast_points = gast_points
      )
      
      # use different models based on purchasing or renting a place
      if (input$choice == "Buying") {
        
        # predict rental price for hosuing data
        rentals_homes_rf %>%
          stats::predict(ml_output) %>%
          
          # rename prediction for rental income
          dplyr::transmute(pred_rentals = round(.pred, 1)) %>%
          dplyr::bind_cols(ml_output, .) -> ml_output
        
        # market value for homes
        homes_rf %>%
          stats::predict(ml_output) %>%
          dplyr::transmute(pred_homes = round(.pred^2, 1)) %>%
          dplyr::bind_cols(ml_output, .) -> ml_output
        
        ml_output %>%
          dplyr::mutate(
            `Market Rate` = pred_homes,
            `Predicted Rental Income` = pred_rentals,
            `Price to Rent Ratio` = round(price / (12 * `Predicted Rental Income`), 1)
          ) %>%
          dplyr::select(`Market Rate`, `Predicted Rental Income`, `Price to Rent Ratio`) %>%
          dplyr::mutate_at(vars(`Market Rate`, `Predicted Rental Income`), ~ scales::dollar(.)) -> ml_output
      } else {
        
        # predict house prices for rental data using housing random forest
        homes_rentals_rf %>%
          stats::predict(ml_output) %>%
          
          # price to the power because of previous transformation
          dplyr::transmute(pred_homes = round(.pred^2, 1)) %>%
          dplyr::pull() -> ml_output
        
        rentals_rf %>%
          stats::predict(ml_output) %>%
          dplyr::transmute(pred_rentals = round(.pred, 1)) %>%
          dplyr::bind_cols(ml_output, .) -> ml_output
        
        ml_output %>%
          dplyr::mutate(
            `Market Rate` = pred_rentals %>%
              scales::dollar(),
            `Predicted Home Price` = pred_homes %>%
              scales::dollar(),
            `Price to Rent Ratio` = round(`Predicted Home Price` / (12 * price), 1)
          ) %>%
          dplyr::select(`Market Rate`, `Predicted Home Price`, `Price to Rent Ratio`) -> ml_output
      }
    }
  })
  
  #####################################################################################
  # reactive_user_inputs <- shiny::reactiveValues()
  # reactive_user_inputs$reactive_properties <- dplyr::as_tibble()
  
  # 3.2.4 Add URL ----
  shiny::observeEvent(input$heart, {
    
    # show box with statements
    shiny::modalDialog(
      title = "Add URL",
      easyClose = TRUE,
      footer = NULL,
      div(
        shiny::textInput("add_url", "Add URL")
      ),
      div(
        shiny::actionButton("click_url", "Add", class = "btn-success")
      )
    ) %>% shiny::showModal()
    
  })
  
  shiny::observeEvent(input$click_url, {
    
    user_inputs()[names(user_inputs()) %in% c("Price", "# of Beds", "# of Baths",
                                              "# of Square Feet", "Address")] %>%
      dplyr::bind_rows() %>%
      dplyr::bind_cols(ml_output() %>%
                         dplyr::select(`Market Rate`, `Price to Rent Ratio`, `Predicted Rental Income`)) %>%
      dplyr::mutate(link = input$add_url) -> new_data
    
    reactive_user_inputs$reactive_properties %>%
      dplyr::bind_rows(new_data) %>%
      dplyr::distinct() -> reactive_user_inputs$reactive_properties
    
    update_mongo(db         = "vancouver-housing", 
                 collection = "van",
                 host       = config$cluster,
                 username   = config$username,
                 password   = config$password,
                 user       = credentials()$info$user,
                 password1  = credentials()$info$password,
                 mongo_data = list(reactive_user_inputs$reactive_properties) %>%
                   dplyr::tibble(data_user = .)
    )
    
  })
  
  # 3.2.5 Data Tables ----
  output$inputs <- DT::renderDataTable({
    
    if(!condition_numeric() & !condition_blank()) {
      DT::datatable(user_inputs() %>%
                      dplyr::bind_rows() %>%
                      dplyr::mutate(Price = as.numeric(Price) %>%
                                      scales::dollar()),
                    rownames = FALSE, options = list(dom = 't',ordering = F))
    }
  })
  
  output$ml_data <- DT::renderDataTable({
    if(!condition_numeric() & !condition_blank()) {
      DT::datatable(ml_output(),
                    rownames = FALSE, options = list(dom = 't', ordering = F))
    }
  })
  
  
  shiny::observe({
    
    shinyjs::hide("heart")
    if(!condition_numeric() & !condition_blank()) {
      
      shinyjs::delay(2000, shinyjs::show("heart"))
      
    }
    
  })
  
  output$clear <- shiny::renderUI({
    
    if(nrow(reactive_user_inputs$reactive_properties) > 0) {
      
      div(
        h3(class = "pull-left", "Favourites"),
        shiny::actionButton("clear", "Clear Favourites", class = "pull-right")
      )
  
    }
    
  })
  
  shiny::observeEvent(input$clear, {
    
    # show box with statements
    shiny::modalDialog(
      title = "Clear Favourites",
      easyClose = TRUE,
      footer = NULL,
      div(
        shiny::selectInput("select_clear", "Select Properties to Clear", multiple = TRUE,
                           choices = reactive_user_inputs$reactive_properties$Address)
      ),
      div(
        shiny::actionButton("clear_all", "Clear Selected", class = "btn-danger")
      )
    ) %>% shiny::showModal()
    
  })
  
  shiny::observeEvent(input$clear_all, {
    
    reactive_user_inputs$reactive_properties %>%
      dplyr::filter(!(Address %in% input$select_clear)) -> reactive_user_inputs$reactive_properties
    
    update_mongo(host       = config$cluster,
                 username   = config$username,
                 password   = config$password,
                 user       = credentials()$info$user,
                 password1  = credentials()$info$password,
                 mongo_data = list(reactive_user_inputs$reactive_properties) %>%
                   dplyr::tibble(data_user = .)
    )
    
  })
  
  # add favourite cards ----
  output$show_info_cards <- shiny::renderUI({
    
    reactive_user_inputs$reactive_properties %>%
      dplyr::mutate(ID = dplyr::row_number()) %>%
      dplyr::group_split(ID) %>%
      purrr::map(~ info_card(price = .x$Price,
                             bed = .x$`# of Beds`,
                             bath = .x$`# of Baths`,
                             sqft = .x$`# of Square Feet`,
                             address = .x$Address,
                             p_to_r = .x$`Price to Rent Ratio`,
                             off = abs(as.numeric(.x$Price) - readr::parse_number(.x$`Market Rate`)),
                             color = ifelse(as.numeric(.x$Price) < readr::parse_number(.x$`Market Rate`),
                                            "success",
                                            "danger"),
                             arrow = ifelse(as.numeric(.x$Price) < readr::parse_number(.x$`Market Rate`),
                                            "arrow-down",
                                            "arrow-up"),
                             predicted = .x$`Market Rate`,
                             link = .x$link))
    
  })
  
}

shinyApp(ui, server)
