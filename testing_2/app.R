ui <- fluidPage(
  textInput("divID", "Enter an ID for the custom area:", ""),
  helpText("Leave the text input blank for automatically unique IDs."),
  actionButton("isrt", "Add a button"), 
  tags$div(id = "placeholder")
)

server <- function(input, output, session) {
  rv <- reactiveValues()
  
  # take a dependency on `isrt` button
  observeEvent(input$isrt, {
    
    # handle the case when user does not provide ID
    divID <- if (input$divID == "") gsub("\\.", "", format(Sys.time(), "%H%M%OS3")) 
    else input$divID
    btnID <- paste0(divID, "rmv")
    
    # only create button if there is none
    if (is.null(rv[[divID]])) {
      
      insertUI(
        selector = "#placeholder",
        ui = tags$div(id = divID, 
                      paste0("Welcome, ", divID, "!"),
                      actionButton(btnID, "Remove this area", 
                                   class = "pull-right btn btn-danger"),
                      style = "background-color: #e0cda7;
                               height: 50px;
                               margin: 10px;
                               padding: 5px;
                               display: block;
                               border-radius: 5px;
                               border: 2px solid #2a334f;"
        )
      )
      
      # make a note of the ID of this section, so that it is not repeated accidentally
      rv[[divID]] <- TRUE
      print("created")
      
      # create a listener on the newly-created button that will
      # remove it from the app when clicked
      observeEvent(input[[btnID]], {
        removeUI(selector = paste0("#", divID))
        
        rv[[divID]] <- NULL
        print("destroyed")
        
      }, ignoreInit = TRUE, once = TRUE)
      
      # otherwise, print a message to the console
    } else {
      message("The button has already been created!")
    }
  })
}

shinyApp(ui = ui, server = server)