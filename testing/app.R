library(shiny)
library(leaflet)
library(dplyr)
library(purrr)
library(sf)
library(readr)
library(data.table)
library(shinyWidgets)
library(ggplot2)

ui <- bootstrapPage(
  
  navbarPage(title = "Vancouver Housing Market",
             
             tabPanel("Test",
                      style = "height:500px;",
                      
                      shiny::absolutePanel(
                        
                        # define location of inputs
                        top = 80, left = 75, draggable = FALSE, fixed = TRUE,
                        width = "20%", style = "z-index:500; min-width: 150px;",
                        
                        # search for housing or rentals
                        shiny::checkboxGroupInput("type", "cyl",
                                                  choices = c(6, 4, 8),
                                                  selected = c(6, 4, 8),
                                                  inline = TRUE),
                        
                        
                        # gear shows up when filtering for rentals or homes only
                        shinyWidgets::chooseSliderSkin("Modern", color = "LightSlateGrey"),
                        shiny::uiOutput("gear_1")
                        
                      )
                      
             )
             
  )
)

server <- function(input, output, session) {
  
  output$gear_1 <- renderUI({
    
    if(length(input$type) < 3) {
      
      shiny::sliderInput("gear_2", "gear to Rent",
                         min = min(mtcars$gear, na.rm = TRUE),
                         max = max(mtcars$gear, na.rm = TRUE),
                         value = c(min(mtcars$gear, na.rm = TRUE),
                                   max(mtcars$gear, na.rm = TRUE)))
      
    }
    
  })
  
  ### reactively filter data for inputs by the user ###
  
  filteredData <- shiny::reactive({
    
    if(length(input$type) < 3) {
      df <- mtcars %>%
        dplyr::filter(gear <= input$gear_2[1] & gear >= input$gear[2])
    }
    
  })
  
  observe ({
    
    ggplot2::ggplot(filteredData(), aes(x = gear, y = mpg, col = gear)) +
      geom_boxplot()
    
  })
  
  
}
shinyApp(ui, server)

