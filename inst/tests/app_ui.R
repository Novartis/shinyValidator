#' Title
#'
#' @return
#' @export
#'
#' @import shiny
#' @importFrom echarts4r echarts4rOutput
app_ui <- function() {
  fluidPage(
    selectInput(
      "var",
      "Select var",
      choices = colnames(echarts_df)[-1],
      selected = "Population"
    ),
    echarts4rOutput("echarts_plot"),
    sliderInput(
      "obs",
      "Number of observations:",
      min = 0,
      max = 1000,
      value = 500
    ),
    plotOutput("distPlot")
  )
}
