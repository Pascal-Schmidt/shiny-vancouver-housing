info_card <- function(price, predicted, off, bed,
                      bath, sqft, p_to_r, address,
                      color, arrow, link) {
  
  div(
    column(
      width = 4,
      div(
        class = "panel panel-default",
        id = "info_card",
        style = "background-color:#FFFFFF;",
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
          p(shiny::icon("money"), as.numeric(price) %>%
              scales::dollar()),
          
          #second row
          p(shiny::icon("bath"), bath) %>%
            div(class = "pull-right"),
          p(shiny::icon("chart-line"), predicted),
          
          # third row
          p(shiny::icon("paw"), sqft) %>%
            div(class = "pull-right"),
          p(class = stringr::str_glue("text-{color}"),
            shiny::icon(arrow), off %>%
              scales::dollar()),
          
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
    )
  )
  
}
