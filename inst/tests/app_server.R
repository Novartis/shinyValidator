#' Main server function
#'
#' @param input Shiny input.
#' @param output Shiny output.
#' @param session Shiny session.
#'
#' @import shiny
#' @importFrom echarts4r renderEcharts4r
app_server <- function(input, output, session) {
  output$distPlot <- renderPlot({
    make_hist(input$obs)
  })
  output$echarts_plot <- renderEcharts4r({
    make_echart(input$var)
  })
}
