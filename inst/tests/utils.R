#' Make histogram
#'
#' @param val Value to plot
#' @export
make_hist <- function(val) {
  graphics::hist(stats::rnorm(val))
}

#' Make echarts
#'
#' @param var Variable...
#'
#' @importFrom echarts4r e_charts e_line_
#' @importFrom magrittr %>%
#' @export
make_echart <- function(var) {
  # add the same id as plot output outside shiny
  # to avoid random snapshot id issue
  id <- if (!shiny::isRunning()) "echarts_plot" else NULL
  State <- NULL;
  echarts_df %>%
    e_charts(x = State, elementId = id) %>% # initialize and set x
    e_line_(serie = var) # add a line
}

#' Test tibble
#'
#' @export
echarts_df <- state.x77 %>%
  as.data.frame() %>%
  tibble::rownames_to_column("State")
