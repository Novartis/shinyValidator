#' Run example app
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @return A shiny app object
#' @export
#' @import shiny
#' @importFrom golem with_golem_options
run_app_audit <- function(onStart = NULL, # nolint
                    options = list(),
                    enableBookmarking = NULL, # nolint
                    uiPattern = "/", # nolint
                    ...) {
  # serve js tools for Monkey test (in case proxy blocks external scripts)
  addResourcePath("gremlins", "inst/shinyValidator-js")
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
        message("\n---- CI/CD callback: APP successfully stopped by chrome ---- \n")
        stopApp(reactlog())
      })
    )
  }

  runApp(
    with_golem_options(
      app = shinyApp(
        ui = app_ui,
        server = app_server,
        onStart = onStart,
        options = options,
        enableBookmarking = enableBookmarking,
        uiPattern = uiPattern
      ),
      golem_opts = list(...)
    ),
    test.mode = TRUE
  )
}
