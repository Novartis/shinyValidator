#' Generate Shiny app reactlog
#'
#' Runs app in subprocess, controls it with headless browser
#' and generate reactlog file for GitLab CI/CD.
#'
#' @inheritParams audit_app
#' @inheritParams start_r_bg
#'
#' @export
upload_reactlog <- function(timeout = 5, headless_actions = NULL, ...) {
  message("\n---- BEGIN REACTLOG ---- \n")
  reactlog_app <- start_r_bg(reactlog_bg, ...)

  chrome <- shinytest2::AppDriver$new(
    "http://127.0.0.1:3515",
    load_timeout = timeout * 1000
  )
  if (!is.null(headless_actions)) {
    run_monkey_test(chrome, headless_actions, screenshot = FALSE)
  }
  # shutdown
  chrome$stop()
  Sys.sleep(1) # required so that we can get_result()
  # move reactlog artifacts
  process_reactlog(reactlog_app)
}



#' Extract and process reactlog
#'
#' Used by \link{upload_reactlog}.
#'
#' @param app Background app.
#'
#' @keywords internal
process_reactlog <- function(app) {
  appLog <- app$get_result()
  appLog <- reactlog::reactlog_render(appLog)
  report_root <- paste(utils::head(strsplit(appLog, "/")[[1]], -1), collapse = "/")
  tmp <- c(appLog, file.path(report_root, "reactlogAsset"))

  # move reactlog artifacts
  system(sprintf("mv %s public/reactlog.html", tmp[1]))
  system(sprintf("mv %s public/", tmp[2]))
}
