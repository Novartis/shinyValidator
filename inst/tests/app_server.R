#' @export
app_server <- function(input, output, session) {
  output$distPlot <- renderPlot({
    graphics::hist(stats::rnorm(input$obs))
  })
}
