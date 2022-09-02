#' Run Shiny app validation tools in your project
#'
#' Run all specified tools and requirements to validate Shiny apps project.
#'
#' @param cran Whether to apply as CRAN check. Defaults to FALSE.
#' @param vignettes Whether to build vignettes. Defaults to FALSE.
#' @param error_on When to raise an error. Possible choices:
#' \code{c("never", "error", "warning", "note")}. Defaults to never.
#' @param timeout Time to wait after starting the subprocess (s). Useful is you know
#' how much time the app takes to load. Defaults to 10 seconds locally and 20 seconds
#' on CI/CD.
#' @param headless_actions Custom code passed as a string to manipulate the app with headless
#' web browser, for instance
#' \code{"headless_app$set_inputs(obs = 200); headless_app$run_js('1+1');"}.
#' If NULL, the app will undergo a classic random Monkey test session.
#' @param workers Number of workers for shinycannon. Default to 5.
#' @param scope Project scope. Accepted values \code{c("manual", "DMC", "POC")}.
#' @param output_validation Whether to compare output snapshots for
#' plots and htmlwidgets. Default to TRUE.
#' @param coverage Whether to perform coverage report. Default to TRUE.
#' @param load_testing Whether to perform load test. Default to TRUE.
#' @param profile_code Whether to profile R code. Default to TRUE.
#' @param check_reactivity Whether to check reactivity log. Default to TRUE.
#' @param flow Whether to display project overview. Default to TRUE.
#' @param debug Special mode during which unit tests are skipped for faster output.
#' @inheritParams start_r_bg
#'
#' @export
audit_app <- function(
  cran = FALSE,
  vignettes = FALSE,
  error_on = "never",
  timeout = NULL,
  headless_actions = NULL,
  workers = 5,
  scope = c("manual", "DMC", "POC"),
  output_validation = FALSE,
  coverage = TRUE,
  load_testing = TRUE,
  profile_code = TRUE,
  check_reactivity = TRUE,
  flow = FALSE,
  debug = FALSE,
  ...
) {

  if (is.null(timeout)) {
    timeout <- if (on_ci()) 20 else 10
  }

  # Technical requirements
  check_audit_requirements()

  # Scope
  scope <- match.arg(scope)
  process_scope(scope)

  # Run check
  tab_check <- check_package(cran, vignettes, error_on, debug)
  # Run crash test
  tab_crash_test <- run_crash_test(timeout, headless_actions, ...)
  # Output validation
  if (debug) output_validation <- FALSE
  tab_output_validation <- if (output_validation) {
    validate_outputs()
  } else {
    NULL
  }
  # Load test, profiling, reactlog
  if (load_testing) record_app(timeout, headless_actions, workers, ...)
  if (profile_code) profile_app(timeout, headless_actions, ...)
  if (check_reactivity) upload_reactlog(timeout, headless_actions, ...)
  if (coverage) covr::gitlab(quiet = FALSE, file = "public/coverage.html")
  if (flow) {
    pkgload::load_all()
    flow::flow_view_shiny(run_app, out = "public/flow.html")
  }

  message("\n---- BEGIN REPORT GENERATION ---- \n")

  # Generate report with tabs
  create_audit_report(
    output_validation,
    coverage,
    load_testing,
    profile_code,
    check_reactivity,
    flow,
    tab_output_validation = tab_output_validation,
    package_name = tab_check$package_name,
    package_version = sprintf(
      "v: %s -- commit: %s", 
      tab_check$package_version
      system("git rev-parse --short HEAD", intern = TRUE)
    ),
    tab_package_check = tab_check$tab_package_check,
    tab_crash_test = tab_crash_test
  )

  message("\n---- ALL GOOD ---- \n")
}

#' Checks to run before running the audit tools
#'
#' Useful for \link{audit_app}. Briefly, we expect the user
#' to have shinycannon and Chrome installed since they are required
#' for loadtest, headless testing ...
#'
#' @return Error if any of the requirement is not met.
#' @keywords internal
check_audit_requirements <- function() {
  message("Checking technical requirements ...")

  if (length(system("which shinycannon", intern = TRUE)) == 0) {
    stop("Missing shinycannon: https://github.com/rstudio/shinycannon")
  }

  has_web_browser <- suppressWarnings(
    length(system("which google-chrome", intern = TRUE)) +
    length(system("which chromium", intern = TRUE))
  )
  if (has_web_browser == 0) {
    stop("Missing Chrome browser ...")
  }

  message("Requirements: DONE ...")
}
