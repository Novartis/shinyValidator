app_server <- function(input, output, session) { # nocov start
  output$distPlot <- renderPlot({
    graphics::hist(stats::rnorm(input$obs))
  })
} # nocov end
