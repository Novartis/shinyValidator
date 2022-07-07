app_ui <- function() { # nocov start
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
} # nocov end
