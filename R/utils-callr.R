#' Start run_app as background process
#'
#' Required by \link{start_r_bg}.
#' @inheritParams start_r_bg
#'
#' @keywords internal
shiny_bg <- function(...) {
  options(shiny.port = 3515)
  pkgload::load_all()
  start_app_with_parms(...)
}

#' Start shinyloadtest recorder in the background
#'
#' Required by \link{start_r_bg}.
#'
#' @param shiny_port Port where runs the shiny apps. This is automatically
#' detected in \link{start_r_bg}.
#'
#' @keywords internal
recorder_bg <- function(shiny_port) {
  shinyloadtest::record_session(
    target_app_url = sprintf("http://127.0.0.1:%s", shiny_port),
    host = "127.0.0.1",
    port = 8600,
    output_file = "recording.log",
    open_browser = FALSE
  )
}

#' Start Shiny + profvis recorder in the background
#'
#' Required by \link{start_r_bg}.
#' @inheritParams start_r_bg
#'
#' @keywords internal
profile_bg <- function(...) {
  options(keep.source = TRUE, shiny.port = 3515)
  pkgload::load_all()
  .profile_code <- TRUE
  profvis::profvis(
    {
      profvis::pause(0.2)
      start_app_with_parms(...)
    },
    simplify = FALSE,
    split = "v"
  )
}

#' Start run_app as background process
#'
#' Also enables reactlog.
#'
#' Required by \link{start_r_bg}.
#'
#' @inheritParams start_r_bg
#'
#' @keywords internal
reactlog_bg <- function(...) {
  options("shiny.port" = 3515)
  pkgload::load_all()
  .enable_reactlog <- TRUE
  reactlog::reactlog_enable()
  start_app_with_parms(...)
}

#' Start background R process
#'
#' Start process in the background. Required by
#' \link{record_app}, ...
#'
#' @param fun Passed to \link[callr]{r_bg}.
#' @param ... Pass extra parameters to run_app. This is useful
#' if you work with packages like golem. 
#'
#' @return Process or error
#' @keywords internal
start_r_bg <- function(fun, ...) {

  func_name <- deparse(substitute(fun))
  parms <- if (func_name == "recorder_bg") {
    list(shiny_port = 3515)
  } else {
    list()
  }

  port <- if (func_name == "recorder_bg") 8600 else 3515

  process <- callr::r_bg(
    func = fun,
    stderr= "",
    stdout = "",
    args = c(parms, ...)
  )

  while (any(is.na(pingr::ping_port("127.0.0.1", port)))) {
    message("Waiting for Shiny app to start...")
    Sys.sleep(0.1)
  }

  if (!process$is_alive()) stop("Unable to launch the subprocess")

  process
}


#' Start Shiny app with list of parameters
#'
#' If not parms are provided, the app is started as usual. Otherwise,
#' the app is called with \code{do.call} with the provided set of parameters.
#' Useful for \link{shiny_bg}, \link{profile_bg} and \link{reactlog_bg}.
#'
#' @param ... Pass extra parameters to run_app. This is useful
#' if you work with packages like golem. 
#'
#' @return Starts a Shiny app.
#' @keywords internal
start_app_with_parms <- function(...) {
  parms <- list(...)
  if (length(parms) > 0 ) {
    do.call(run_app, parms)
  } else {
    run_app()
  }
}