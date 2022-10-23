app_server <- function(input, output, session) { # nocov start
  #Sys.sleep(5)
  output$distPlot <- renderPlot({
    graphics::hist(stats::rnorm(input$obs))
  })
} # nocov end
