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
#' @param local Whether to setup the project for local audit with docker.
#' Default to FALSE.
#'
#' @details By default, the package is checked, built and test are run.
#' Also, we quickly check if the Shiny application is able to start and run
#' without crashing.
#' @export
use_validator <- function(cicd_platform = c("github", "gitlab-docker", "gitlab"), local = FALSE) {
  cicd_platform <- match.arg(cicd_platform)
  # setup prerequisites
  check_setup_requirements(cicd_platform, local)

  # CI/CD
  if (!local) {
    initialize_cicd(cicd_platform)
  } else {
    file.copy(
      from = system.file(
        "workflows/docker/local/Dockerfile",
        package = "shinyValidator"
      ),
      to = "./Dockerfile"
    )
  }

  # Add lintr
  file.copy(system.file("lintr/.lintr", package = "shinyValidator"), ".")

  # Copy R/run_app.R
  copy_app_file()

  # treat .Rbuildignore
  edit_buildignore(cicd_platform, local)

  # Add suggested pkgs to DESCRIPTION + install them in renv library
  add_suggested_packages()

  # Add gremlins.js assets
  add_gremlins_assets()

  message("Don't forget to call renv::snapshot() and restart R")
}
