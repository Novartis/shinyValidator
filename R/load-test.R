#' Record a Shiny session
#'
#' Start Shiny app as local R subprocess. Connect the
#' shinyloadtest recorder and connect Chrome to the recorder.
#' Manipulate the headless Chrome and close connection. shinycannon
#' replays
#'
#' @inheritParams audit_app
#'
#' @export
record_app <- function(timeout = 5, headless_actions = NULL, workers = 5) {
  message("\n---- BEGIN LOAD-TEST ---- \n")
  # start app + recorder
  target <- start_r_bg(shiny_bg)
  recorder <- start_r_bg(recorder_bg)

  # start headless chrome (points to recorder!).
  # AppDriver also support remote urls.
  chrome <- shinytest2::AppDriver$new(
    "http://127.0.0.1:8600",
    load_timeout = timeout * 1000
  )

  run_monkey_test(chrome, headless_actions, screenshot = FALSE)

  # clean
  chrome$stop()
  # needed to avoid
  # java.lang.IllegalStateException: last event in log not a
  # WS_CLOSE (did you close the tab after recording?)
  Sys.sleep(2)

  # shinycannon (maybe expose other params later ...)
  target_url <- "http://127.0.0.1:3515"
  system(
    sprintf(
      "shinycannon recording.log %s --workers %s --loaded-duration-minutes 2 --output-dir load-run",
      target_url, workers
    )
  )

  target$kill()

  # Treat data and generate report
  df <- shinyloadtest::load_runs("load-run")
  shinyloadtest::shinyloadtest_report(
    df,
    "public/load-test.html",
    self_contained = FALSE,
    open_browser = FALSE
  )
}
