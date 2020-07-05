library(shiny)

info_card <- function(df, link) {
  
  price <- df$price
  predicted <- readr::parse_number(df$`Market Rate`)
  off <- abs(price - predicted)
  bed <- df$bed
  bath <- df$bath
  sqft <- df$sqft
  rental <- readr::parse_number(df$`Predicted Rental Income`)
  p_to_r <- round(price / (12*rental), 1)
  address <- df$address
  
  color <- base::ifelse(predicted < price, "success", "danger")
  arrow <- base::ifelse(predicted < price, "arrow-down", "arrow-up")
  
  div(
    class = "panel panel-default",
    div(
      div(
        img(class="img-responsive", src = "vancouver-1.jpg")
      )
    ),
    div(
      style = "margin: 15px;",
      
      # header
      h3(shiny::icon("sign"), address),
      
      # first row
      p(shiny::icon("bed"), bed) %>%
        div(class = "pull-right"),
      p(shiny::icon("money"), price %>%
          scales::dollar()),
      
      #second row
      p(shiny::icon("bath"), bath) %>%
        div(class = "pull-right"),
      p(shiny::icon("chart-line"), predicted %>%
          scales::dollar()),
      
      # third row
      p(shiny::icon("paw"), sqft) %>%
        div(class = "pull-right"),
      p(class = stringr::str_glue("text-{color}"),
        shiny::icon(arrow), off),
      
      # forth row
      p(a(class = "btn-small btn-primary", href = link, 
          target = "_blank", "To Property",
          style = "padding-top: 3px;
          padding-left: 3px;
          padding-right: 3px;
          padding-bottom: 3px;")) %>%
        div(class = "pull-right"),
      p(shiny::icon("percentage"), p_to_r)
    )
  )
  
}

# Define UI for application that draws a histogram
ui <- bootstrapPage(
  
  df <- data.frame(price = 1,
                   bed = 1,
                   bath = 1,
                   sqft = 1,
                   `Market Rate` = "$1",
                   `Predicted Rental Income` = "$1",
                   link = "a")
  
  info_card(df = df, link = "a")
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

