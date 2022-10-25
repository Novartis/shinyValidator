app_server <- function(input, output, session) { # nocov start
  output$distPlot <- renderPlot({
    #Sys.sleep(10)
    graphics::hist(stats::rnorm(input$obs))
  })
} # nocov end
