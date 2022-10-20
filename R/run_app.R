#' Run example app for audit
#'
#' @return A shiny app object
#' @export
#' @import shiny
run_app_audit <- function() { # nocov start
  # serve js tools for Monkey test (in case proxy blocks external scripts)
  addResourcePath("gremlins", system.file("shinyValidator-js", package = "shinyValidator"))
  # DON'T CHANGE (INTERNAL TO SHINYVALIDATOR)
  p <- parent.frame(1)
  .enable_reactlog <- p[[".enable_reactlog"]]
  .profile_code <- p[[".profile_code"]]
  if (is.null(.enable_reactlog)) .enable_reactlog <- FALSE
  if (is.null(.profile_code)) .profile_code <- FALSE

  if (.enable_reactlog || .profile_code) {
    tmp <- body(app_server)
    start <- length(tmp) + 1 # start just before the closing }
    body(app_server)[[start]] <- substitute(
      onSessionEnded(function() {
        message("CI/CD callback: APP successfully stopped by chrome")
        stopApp(reactlog())
      })
    )
  }

  runApp(
    shinyApp(app_ui, app_server),
    test.mode = TRUE
  )
} # nocov end

#' Run the Shiny Application
run_app <- function() { # nocov start
  shinyApp(app_ui, app_server)
} # nocov end

globalVariables(c("app_ui", "app_server"))
