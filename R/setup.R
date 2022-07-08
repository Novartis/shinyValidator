#' Include Shiny app validation tools in your project
#'
#' Setup all necessary tools and requirements to validate Shiny apps project.
#' This function is the first to be called.
#'
#' @param cicd_platform CI/CD engine. GitLab or GitHub Actions.
#' When gitlab-docker is selected, we leverage docker executor.
#' The provided CI/CD template will pull docker image from
#' \url{https://hub.docker.com/repository/docker/divadnojnarg/shinyvalidator-docker},
#' which provides installation of R, shinycannon, Chrome, necessary to
#' run the pipeline without issue. This is typically the easiest setup since
#' the classic GitLab CI/CD templates assumes that your GitLab runner has
#' everything installed, which is not necessarily the case.
#'
#' @details By default, the package is checked, built and test are run.
#' Also, we quickly check if the Shiny application is able to start and run
#' without crashing.
#' @export
use_validator <- function(cicd_platform = c("github", "gitlab-docker", "gitlab")) {
  cicd_platform <- match.arg(cicd_platform)
  # setup prerequisites
  check_setup_requirements(cicd_platform)

  # CI/CD
  initialize_cicd(cicd_platform)

  # Add lintr
  file.copy(system.file("lintr/.lintr", package = "shinyValidator"), ".")

  # Add gremlins.js assets
  add_gremlins_assets()

  # Copy R/run_app.R
  copy_app_file()

  # treat .Rbuildignore
  edit_buildignore(cicd_platform)

  # Add suggested pkgs to DESCRIPTION + install them in renv library
  add_suggested_packages()

  message("Don't forget to call renv::snapshot() and restart R")
}
