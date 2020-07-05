# 1.0 UI ----
user_input_ui_mod <- function(id) {
  
  ns <- shiny::NS(id)
  # neighborhood_choice <- unique(df$name)
  
  div(
    class = "container",
    shiny::fluidRow(
      column(
        4,
        shinyWidgets::textInputAddon(inputId = ns("price_input"),
                                     label   = "Price",
                                     addon   = shiny::icon("money"))
      ),
      column(
        4,
        div(
          shinyWidgets::textInputAddon(inputId = ns("bed"),
                                       label   = "# of Beds",
                                       addon   = shiny::icon("bed"))
        )
      ),
      column(
        4,
        shinyWidgets::textInputAddon(inputId = ns("bath"), 
                                     label   = "# of Baths",
                                     addon   = shiny::icon("bath"))
      )
    ),
    
    shiny::fluidRow(
      column(
        4,
        shinyWidgets::textInputAddon(inputId = ns("sqft"),
                                     label   = "# of Square Feet",
                                     addon   = shiny::icon("paw"))
      ),
      column(
        4,
        shinyWidgets::textInputAddon(inputId = ns("address_input"),
                                     label   = "Address",
                                     addon   = shiny::icon("map"))
      ),
      column(
        4,
        shinyWidgets::textInputAddon(inputId = ns("year_built"), 
                                     label   = "Year Built",
                                     addon   = shiny::icon("calendar"))
      )
    ),
    shiny::fluidRow(
      column(
        4,
        shinyWidgets::pickerInput(inputId = ns("neighborhood"),
                                     label   = "Neighborhood",
                                     choices = unique(df$name))
        )
      ),
      column(
        4,
        shinyWidgets::pickerInput(inputId = ns("choice"), 
                                     label   = "House Purchase",
                                     choices = c("Buying")
        )
      ),
      column(
        4,
        shinyWidgets::pickerInput(inputId = ns("prop_type"),
                                     label   = "Property Type",
                                     choices = c("Condo", "Duplex", "House", "Townhouse")
        )
      )
    )
  
}

# 2.0 Server ----
user_input_server_mod <- function(input, output, session, action) {
  
  shiny::eventReactive(action(), {
    
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
  
}