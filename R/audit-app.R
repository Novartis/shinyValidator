#' Run Shiny app validation tools in your project
#'
#' Run all specified tools and requirements to validate Shiny apps project.
#'
#' @param headless_actions Custom code passed as an expression to manipulate the app with headless
#' web browser, for instance
#' \code{app$set_inputs(obs = 200); app$run_js('1+1');}.
#' See \url{https://rstudio.github.io/shinytest2/reference/AppDriver.html} to
#' get all available methods.
#' If NULL, the app will undergo a classic random Monkey test session, taking a screenshot
#' right after loading and after the monkey test. We do not recommend running a monkey test
#' if your app is pointing to a database, unless the pipeline CI/CD runs in a dedicated test environment.
#' @param timeout Time to wait after starting the subprocess (s). Useful is you know
#' how much time the app takes to load. Defaults to 10 seconds locally and 20 seconds
#' on CI/CD.
#' @param scope Project scope. Accepted values \code{c("manual", "DMC", "POC")}.
#' @param workers Number of workers for shinycannon. Default to 5.
#' @param cran Whether to apply as CRAN check. Defaults to FALSE.
#' @param vignettes Whether to build vignettes. Defaults to FALSE.
#' @param error_on When to raise an error. Possible choices:
#' \code{c("never", "error", "warning", "note")}. Defaults to never.
#' @param output_validation Whether to compare output snapshots for
#' plots and htmlwidgets. Default to TRUE.
#' @param coverage Whether to perform coverage report. Default to TRUE.
#' @param load_testing Whether to perform load test. Default to TRUE.
#' @param profile_code Whether to profile R code. Default to TRUE.
#' @param check_reactivity Whether to check reactivity log. Default to TRUE.
#' @param flow Whether to display project overview. Default to TRUE.
#' @param debug Special mode during which unit tests are skipped for faster output.
#' @param r_version R version supported by your IT.
#' @param locked_deps List of packages supported by your IT. For instance you
#' can pass a dataframe like
#' \code{available.packages(repos = "https://cran.microsoft.com/snapshot/2017-01-19/")},
#' or read a csv file with the same structure.
#' @inheritParams start_r_bg
#'
#' @export
audit_app <- function(
    headless_actions = NULL,
    cran = FALSE,
    vignettes = FALSE,
    error_on = "never",
    timeout = NULL,
    workers = 5,
    scope = c("manual", "DMC", "POC"),
    output_validation = FALSE,
    coverage = TRUE,
    load_testing = TRUE,
    profile_code = TRUE,
    check_reactivity = TRUE,
    flow = FALSE,
    debug = FALSE,
    r_version = NULL,
    locked_deps = NULL,
    port = httpuv::randomPort(min = 3000, max = 3500),
    ...
) {

  if (is.null(timeout)) {
    timeout <- if (on_ci()) 20 else 10
  }

  # Capture user defined expression if provided
  headless_actions <- substitute(headless_actions)

  # Technical requirements
  check_audit_requirements()

  # Scope
  scope <- match.arg(scope)
  process_scope(scope)

  # Run check
  tab_check <- check_package(cran, vignettes, error_on, debug)
  # Run crash test
  tab_crash_test <- run_crash_test(headless_actions, timeout, port, ...)
  # Output validation
  if (debug) output_validation <- FALSE
  tab_output_validation <- if (output_validation) {
    validate_outputs()
  } else {
    NULL
  }
  # Load test, profiling, reactlog
  if (load_testing) record_app(headless_actions, timeout, workers, port, ...)
  if (profile_code) profile_app(headless_actions, timeout, port, ...)
  if (check_reactivity) upload_reactlog(headless_actions, timeout, port, ...)
  if (coverage) covr::gitlab(quiet = FALSE, file = "public/coverage.html")
  if (flow) {
    flow_widget <- flow::flow_view_shiny(run_app)
    # see https://rstudio.github.io/nomnoml/reference/nomnoml.html:
    # svg does not seem to work well. We can either act on the JS side with this:
    # $(function() {
    #   var iframe = $("#flow-iframe");
    #   var HTMLWidgets = $(iframe)[0].contentWindow.HTMLWidgets;
    #   var flowWidget = $(iframe).contents().find("#flow-widget");
    # });
    # However I don't like this code. R approach is better.
    flow_widget$x$svg <- FALSE
    flow_widget$width <- "100%"
    flow_widget$height <- "100%"
    # Give it an id just in case ...
    flow_widget$elementId <- "flow-widget"
    htmlwidgets::saveWidget(
      flow_widget,
      "public/flow.html"
    )
  }
  tab_deps <- check_dependencies(r_version, locked_deps)

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
      tab_check$package_version,
      system("git rev-parse --short HEAD", intern = TRUE)
    ),
    tab_package_check = tab_check$tab_package_check,
    tab_crash_test = tab_crash_test,
    tab_deps = tab_deps
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
