#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)

ui <- bootstrapPage(
  
  navbarPage(
    title = "Vancouver Housing Market",
    
    tabPanel("Map",
             div(class = "outer",
                 
                 tags$head(
                   # Include our custom CSS
                   includeCSS("styles.css")
                 ),
                 
                 # leaflet map
                 leafletOutput("map", width = "100%", height = "100%"),
                 
                 shiny::absolutePanel(
                   
                   id = "controls", class = "panel panel-default", fixed = TRUE,
                   draggable = FALSE, top = 60, left = "auto", right = 20, bottom = "auto",
                   width = "auto", height = "auto",
                   
                   # search for housing or rentals
                   shiny::checkboxGroupInput("type", "Homes | Rentals",
                                             choiceNames =
                                               list(
                                                 shiny::icon("home"),
                                                 shiny::icon("city")
                                               ),
                                             choiceValues = list("buying", "rental"),
                                             selected = c("buying", "rental"),
                                             inline = TRUE)
                 )
             )
    )
  )
)

server <- function(input, output, session) {
  
  output$map <- renderLeaflet({

    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        setView(lng = -93.85, lat = 37.45, zoom = 4)
    })
    
  })
  
}

shinyApp(ui, server)

