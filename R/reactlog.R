#' Generate Shiny app reactlog
#'
#' Runs app in subprocess, controls it with headless browser
#' and generate reactlog file for GitLab CI/CD.
#'
#' @inheritParams audit_app
#' @inheritParams start_r_bg
#'
#' @export
upload_reactlog <- function(headless_actions = NULL, timeout = NULL,
                            port = httpuv::randomPort(max = 3500), ...) {
  message("\n---- BEGIN REACTLOG ---- \n")

  if (is.null(timeout)) {
    timeout <- if (on_ci()) 20 else 10
  }

  tryCatch({
    bg_app <- start_r_bg(reactlog_bg, port, ...)
    chrome <- shinytest2::AppDriver$new(
      sprintf("http://127.0.0.1:%s", port),
      load_timeout = timeout * 1000
    )

    if (!is.null(headless_actions)) {
      run_monkey_test(
        chrome,
        headless_actions,
        screenshot = FALSE,
        path = "public/crash-test"
      )
    }

    # shutdown
    chrome$stop()
    # required so that we can get_result()
    wait_for_app_stop(3515)
    # move reactlog artifacts
    process_reactlog(bg_app)

    message("\n---- END REACTLOG ---- \n")
  }, error = function(e) {
    cleanup_on_exit(bg_app, chrome)
  })
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
