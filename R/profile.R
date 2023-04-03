#' Profile a shiny app
#'
#' Start Shiny + profvis in the background. Chrome connects
#' to the app and then closes to interrupt profvis.
#' The profile report is then saved and exported in the public
#' folder needed for CI/CD.
#'
#' @inheritParams audit_app
#' @inheritParams start_r_bg
#'
#' @return Write a .Rprof file to be reused by CI/CD to publish the report on GitLab pages
#' @export
profile_app <- function(headless_actions = NULL, timeout = NULL,
                        port = randomPort(max = 3500), ...) {
  message("\n---- BEGIN CODE PROFILE ---- \n")

  if (is.null(timeout)) {
    timeout <- if (on_ci()) 20 else 10
  }

  tryCatch({
    bg_app <- start_r_bg(profile_bg, port, ...)
    # chrome is just needed to trigger onSessionEnded callback from app_server
    chrome <- shinytest2::AppDriver$new(
      sprintf("http://127.0.0.1:%s", port),
      load_timeout = timeout * 1000,
      view = FALSE
    )

    # In case we don't call run_audit but
    # profile_app directly... we need
    # to substitute since it is made in run_audit
    if (sys.nframe() == 1) {
      headless_actions <- substitute(headless_actions)
    }

    if (!is.null(headless_actions)) {
      run_monkey_test(
        chrome,
        headless_actions,
        screenshot = FALSE,
        path = "public/crash-test"
      )
    }

    chrome$stop()
    # required so that we can get_result()
    wait_for_app_stop(port)

    message("Saving profile report ... this may take a while")
    htmlwidgets::saveWidget(bg_app$get_result(), "public/code-profile.html")

    message("\n---- END CODE PROFILE ---- \n")
  }, error = function(e) {
    message(e)
    cleanup_on_exit(bg_app, chrome)
    stop("\n---- FOUND ERROR(S) WHILE PROFILING THE APP... ---- \n")
  })
}
