#' Include Shiny app validation tools in your project
#'
#' Setup all necessary tools and requirements to validate Shiny apps project.
#' This function is the first to be called.
#'
#' @details By default, the package is checked, built and test are run.
#' Also, we quickly check if the Shiny application is able to start and run
#' without crashing.
#' @export
use_validator <- function() {

  # prerequisites
  check_requirements()

  # CI/CD
  initialize_cicd()

  # Add lintr
  file.copy(system.file("lintr/.lintr", package = "shinyValidator"), ".")

  # Copy R/run_app.R
  copy_app_file()

  # treat .Rbuildignore
  edit_buildignore()

  # Add suggested pkgs to DESCRIPTION + install them in renv library
  add_suggested_packages()

  # Add gremlins.js assets
  add_gremlins_assets()

  message("Don't forget to call renv::snapshot() and restart R")
}
