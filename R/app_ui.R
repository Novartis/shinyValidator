app_ui <- function() {
  fluidPage(
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
