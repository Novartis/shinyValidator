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
profile_app <- function(headless_actions = NULL, timeout = NULL, ...) {
  message("\n---- BEGIN CODE PROFILE ---- \n")

  if (is.null(timeout)) {
    timeout <- if (on_ci()) 20 else 10
  }

  prof_app <- start_r_bg(profile_bg, ...)
  # chrome is just needed to trigger onSessionEnded callback from app_server
  chrome <- shinytest2::AppDriver$new(
    "http://127.0.0.1:3515",
    load_timeout = timeout * 1000
  )

  chrome$wait_for_idle()

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
  wait_for_app_stop(3515)

  htmlwidgets::saveWidget(prof_app$get_result(), "public/code-profile.html")

  message("\n---- END CODE PROFILE ---- \n")
}
