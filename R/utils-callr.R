#' Start run_app_audit as background process
#'
#' Required by \link{start_r_bg}.
#' @inheritParams start_r_bg
#' @inheritParams recorder_bg
#'
#' @keywords internal
shiny_bg <- function(shiny_port, ...) {
  pkgload::load_all()
  parms <- list(...)
  options(shiny.port = shiny_port)
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
#' @inheritParams recorder_bg
#'
#' @keywords internal
profile_bg <- function(shiny_port, ...) {
  options(keep.source = TRUE, shiny.port = shiny_port)
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
#' @inheritParams recorder_bg
#'
#' @keywords internal
reactlog_bg <- function(shiny_port, ...) {
  pkgload::load_all()
  app_options <- list(port = shiny_port)
  .enable_reactlog <- TRUE
  reactlog::reactlog_enable()
  options(shiny.port = shiny_port)
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
#' @param port Port to start the app, default to \code{httpuv::randomPort()}.
#' @param ... Pass extra parameters to run_app_audit. This is useful
#' if you work with packages like golem.
#'
#' @return Process or error
#' @keywords internal
start_r_bg <- function(fun, port, ...) {

  parms <- list(shiny_port = port)

  func_name <- deparse(substitute(fun))
  if (func_name != "recorder_bg") {
    parms <- c(parms, list(...))
  }

  process <- callr::r_bg(
    func = fun,
    stderr= "",
    stdout = "",
    args = parms
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
    if (action == "start") Sys.sleep(0.5) else Sys.sleep(4)
  }
}

#' Kill ports on exit
#'
#' Avoid to have port taken when audit fails
#' for any reasons
#'
#' @param bg_app Shiny app background process.
#' @param chrome Chrome background process.
#' @param recorder Recorder app background process. For shinyloadtest...
#
#' @keywords internal
cleanup_on_exit <- function(bg_app, chrome, recorder = NULL) {
  on.exit({
    bg_app$kill()
    #chrome$stop() TO DO: find way to clean chrome
    if (!is.null(recorder)) recorder$kill()
    message("SESSION CLEANED")
  }, add = TRUE)
}
