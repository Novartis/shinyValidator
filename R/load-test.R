#' Record a Shiny session
#'
#' Start Shiny app as local R subprocess. Connect the
#' shinyloadtest recorder and connect Chrome to the recorder.
#' Manipulate the headless Chrome and close connection. shinycannon
#' replays
#'
#' @inheritParams audit_app
#' @inheritParams start_r_bg
#'
#' @export
record_app <- function(headless_actions = NULL, timeout = NULL, workers = 5,
                       port = randomPort(max = 3500), ...) {
  message("\n---- BEGIN LOAD-TEST ---- \n")

  if (is.null(timeout)) {
    timeout <- if (on_ci()) 20 else 10
  }

  tryCatch({
    # start app + recorder
    bg_app <- start_r_bg(shiny_bg, port, ...)
    recorder <- start_r_bg(recorder_bg, port)

    # start headless chrome (points to recorder!).
    # AppDriver also support remote urls.
    chrome <- shinytest2::AppDriver$new(
      "http://127.0.0.1:8600",
      load_timeout = timeout * 1000,
      view = FALSE
    )

    # In case we don't call run_audit but
    # record_app directly... we need
    # to substitute since it is made in run_audit
    if (sys.nframe() == 1) {
      headless_actions <- substitute(headless_actions)
    }

    run_monkey_test(
      chrome,
      headless_actions,
      screenshot = FALSE,
      path = "public/crash-test"
    )

    # clean
    chrome$stop()
    # needed to avoid
    # java.lang.IllegalStateException: last event in log not a
    # WS_CLOSE (did you close the tab after recording?)
    Sys.sleep(2)

    # shinycannon (maybe expose other params later ...)
    target_url <- sprintf("http://127.0.0.1:%s", port)
    system(
      sprintf(
        "shinycannon recording.log %s --workers %s --loaded-duration-minutes 2 --output-dir run1",
        target_url, workers
      )
    )

    bg_app$kill()

    # Treat data and generate report
    df <- shinyloadtest::load_runs("run1")
    shinyloadtest::shinyloadtest_report(
      df,
      "public/load-test.html",
      self_contained = FALSE,
      open_browser = FALSE
    )

    message("\n---- END LOAD-TEST ---- \n")
  }, error = function(e) {
    cleanup_on_exit(bg_app, chrome, recorder)
  })
}
