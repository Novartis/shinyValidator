#' Run the Shiny Application
#' @export
run_app <- function() { # nocov start
  shinyApp(app_ui, app_server)
} # nocov end

globalVariables(c("app_ui", "app_server"))
