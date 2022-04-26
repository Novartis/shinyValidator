#' Run Shiny app validation tools in your project with CI/CD
#'
#' Run all specified tools and requirements to validate Shiny apps project.
#'
#' @param cran Whether to apply as CRAN check. Defaults to FALSE.
#' @param vignettes Whether to build vignettes. Defaults to FALSE.
#' @param error_on When to raise an error. Possible choices:
#' \code{c("never", "error", "warning", "note")}. Defaults to never.
#' @param timeout Time to wait after starting the subprocess (s). Useful is you know
#' how much time the app takes to load.
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
#'
#' @export
audit_app <- function(
  cran = FALSE,
  vignettes = FALSE,
  error_on = "never",
  timeout = 5,
  headless_actions = NULL,
  workers = 5,
  scope = c("manual", "DMC", "POC"),
  output_validation = TRUE,
  coverage = TRUE,
  load_testing = TRUE,
  profile_code = TRUE,
  check_reactivity = TRUE,
  flow = TRUE
) {

  # Technical requirements
  check_audit_requirements()

  # Scope
  scope <- match.arg(scope)
  process_scope(scope)

  # Run check
  tab_check <- check_package(cran, vignettes, error_on)
  # Run crash test
  tab_crash_test <- run_crash_test(timeout, headless_actions)
  # Output validation
  tab_output_validation <- if (output_validation) {
    validate_outputs()
  } else {
    NULL
  }
  # Load test, profiling, reactlog
  if (load_testing) record_app(timeout, headless_actions, workers)
  if (profile_code) profile_app(timeout, headless_actions)
  if (check_reactivity) upload_reactlog(timeout, headless_actions)
  if (coverage) covr::gitlab(quiet = FALSE, file = "public/coverage.html")
  if (flow) {
    pkgload::load_all()
    flow::flow_view_shiny(run_app, out = "public/flow.html")
  }

  message("\n---- BEGIN REPORT GENERATION ---- \n")

  # Generate report with tabs
  create_report_tabs(
    output_validation,
    coverage,
    load_testing,
    profile_code,
    check_reactivity,
    flow,
    tab_output_validation = tab_output_validation,
    package_name = tab_check$package_name,
    package_version = tab_check$package_version,
    tab_package_check = tab_check$tab_package_check,
    tab_crash_test = tab_crash_test
  )

  message("\n---- ALL GOOD ---- \n")
}


#' Run Shiny app validation tools in your project locally with docker
#'
#' Run all specified tools and requirements to validate Shiny apps project.
#'
#' @param container_name Container name for docker.
#' @param container_cache renv cache location inside the container.
#' Default to "renv/cache".
#' @param shinyValidator_tag Allow to get specific flavor of shinyValidator.
#' @param port Port where apache2 server will serve the audit HTML report.
#' @param open Whether to browse to the apache2 local server url.
#' @export
audit_app_docker <- function(
  container_name,
  container_cache = "/renv/cache",
  shinyValidator_tag = NULL,
  port = 80,
  open = TRUE
) {
  # Set env vars for cache
  Sys.setenv(
    "RENV_PATHS_CACHE_HOST" = get_renv_cache_path(),
    "RENV_PATHS_CACHE_CONTAINER" = container_cache
  )
  # Build docker image
  system("docker build -t shinyvalidator-local:latest .")


  apache2_cmd <- paste(
    "rm /var/www/html/index.html",
    "mv ./public/* /var/www/html/",
    "apache2ctl -D FOREGROUND",
    sep = " && "
  )

  # Start container
  system(
    sprintf(
      "docker run --name %s \
        -p %s:%s \
        -e \"RENV_PATHS_CACHE=${RENV_PATHS_CACHE_CONTAINER}\" \
        -v \"${RENV_PATHS_CACHE_HOST}:${RENV_PATHS_CACHE_CONTAINER}\" \
        shinyvalidator-local:latest \
        R --vanilla -s -e 'source(\"renv/activate.R\");
          renv::restore();
          devtools::install_github(\"Novartis/shinyValidator@%s\");
          shinyValidator::lint_code();
          shinyValidator::audit_app(flow = FALSE, output_validation = FALSE, load_testing = FALSE);
          system(%s);'
      ",
      container_name,
      port, port,
      shinyValidator_tag,
      apache2_cmd
    )
  )

  if (open) {
    browseUrl("http://0.0.0.0:80")
  }
  print(system("docker ps"))
  #docker stop <ID> && docker rm <ID>
}


#' Checks if docker is installed on the local machine
#'
#' @return Boolean.
#' @keywords internal
is_docker_installed <- function() {
  length(system("docker --version", intern = TRUE)) > 0
}

#' Get system OS
#'
#' @return Character with system OS.
#' @keywords internal
get_sysname <- function() {
  sys_info <- Sys.info()
  sys_info[["sysname"]]
}

#' Get renv cache location
#'
#' Specific to OS.
#' Taken from \url{https://rstudio.github.io/renv/articles/renv.html#cache}.
#'
#' @return Character with renv path cache location.
#' @keywords internal
get_renv_cache_path <- function() {
  switch(get_sysname(),
    "Darwin" = "~/Library/Application Support/renv",
    "Linux" = "~/.local/share/renv",
    "Windows" = "%LOCALAPPDATA%/renv"
  )
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
