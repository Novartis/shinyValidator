#' Start run_app_audit as background process
#'
#' Required by \link{start_r_bg}.
#' @inheritParams start_r_bg
#'
#' @keywords internal
shiny_bg <- function(...) {
  options(shiny.port = 3515)
  pkgload::load_all()
  parms <- list(...)
  if (length(parms) > 0) {
    do.call(run_app_audit, parms)
  } else {
    run_app_audit()
  }
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
      parms <- list(...)
      if (length(parms) > 0) {
        do.call(run_app_audit, parms)
      } else {
        run_app_audit()
      }
    },
    simplify = FALSE,
    split = "v"
  )
}

#' Start run_app_audit as background process
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
  parms <- list(...)
  if (length(parms) > 0) {
    do.call(run_app_audit, parms)
  } else {
    run_app_audit()
  }
}

#' Start background R process
#'
#' Start process in the background. Required by
#' \link{record_app}, ...
#'
#' @param fun Passed to \link[callr]{r_bg}.
#' @param ... Pass extra parameters to run_app_audit. This is useful
#' if you work with packages like golem.
#'
#' @return Process or error
#' @keywords internal
start_r_bg <- function(fun, ...) {

  golem_pars <- list(...)

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
    args = if (func_name == "recorder_bg") parms else golem_pars
  )

  wait_for_app_start(port)

  if (!process$is_alive()) stop("Unable to launch the subprocess")

  process
}

wait_for_app_start <- function(port) {
  wait_for_app_action("start", port)
}

wait_for_app_stop <- function(port) {
  wait_for_app_action("stop", port)
}

#' Wait for application action
#'
#' Ping the application port. If action is start,
#' loop until the port is available, and inversely if action is stop.
#'
#' @param action Either "start" or "stop".
#' @param port Running port to ping.
#' @keywords internal
wait_for_app_action <- function(action = c("start", "stop"), port) {
  action <- match.arg(action)
  cond <- switch(action,
    "start" = quote(is.na(pingr::ping_port("127.0.0.1", port))),
    "stop" = quote(!is.na(pingr::ping_port("127.0.0.1", port)))
  )

  while (any(eval(cond))) {
    message(sprintf("Waiting for Shiny app to %s ...", action))
    Sys.sleep(0.3)
  }
}
