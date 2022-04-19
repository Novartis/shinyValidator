#' Checks to run before
#'
#' Useful for \link{use_validator}
#'
#' @return Error if any if the requirement is not met.
#' @keywords internal
check_requirements <- function() {
  message("Checking requirements ...")

  check_if_validator_installed()

  if (R.version$major != "4") {
    stop("{shinyValidator} only works for R >= 4")
  }
  if (!file.exists("DESCRIPTION")) {
    stop("{shinyValidator} only works inside R packages.")
  }
  if (!file.exists("renv.lock")) {
    stop("Please setup {renv} to manage dependencies in your project.")
  }

  if (length(system("which shinycannon", intern = TRUE)) == 0) {
    stop("Missing shinycannon: https://github.com/rstudio/shinycannon")
  }

  if (!file.exists("./R/app_server.R")) {
    stop("Project must contain: app_ui.R and app_server.R like in {golem}, {creatr}.")
  }

  message("Requirements: DONE ...")
}

#' Process scope for project
#'
#' @param scope Current project scope
#'
#' @return Reassign parms based on the scope
#' @keywords internal
process_scope <- function(scope) {
  switch(scope,
         "manual" = NULL,
         "DMC" = apply_dmc_scope(),
         "POC" = apply_poc_scope()
  )
}

#' Apply DMC scope
#'
#' DMC are the most critical applications
#'
#' @keywords internal
apply_dmc_scope <- function() {

}

#' Apply POC scope
#'
#' POC apps just need basic check: lint, style + crash test
#'
#' @keywords internal
apply_poc_scope <- function() {
  # need to modify env in grand parent
  # (apply_poc_scope -- process_scope -- use_validator)
  p <- parent.frame(n = 2)
  p[["output_validation"]] <- FALSE
  p[["coverage"]] <- FALSE
  p[["load_testing"]] <- FALSE
  p[["profile_code"]] <- FALSE
  p[["check_reactivity"]] <- FALSE
  p[["flow"]] <- FALSE
}

#' Checks if the validator is already installed
#'
#' @return Boolean.
#' @keywords internal
check_if_validator_installed <- function() {
  if (file.exists("./.gitlab-ci.yml")) {
    tmp <- readLines("./.gitlab-ci.yml")
    sum(grep("### <shinyValidator template DON'T REMOVE> ###", tmp) == 1)
    stop("Validator already installed! Aborting ...")
  } else {
    FALSE
  }
}

#' Copy and rename app helpers
#'
#' @keywords internal
copy_app_file <- function() {
  message("Copying run_app.R and archive old run_app.R function")
  file.rename("./R/run_app.R", "./R/run_app-old.R")
  file.copy(system.file("run-app/run_app.R", package = "shinyValidator"), "./R/run_app.R")
}

#' Edit .Rbuildignore file
#'
#' @return Edit existing .Rbuidignore file with additional entries
#' @keywords internal
edit_buildignore <- function() {
  usethis::use_build_ignore(
    c(
      ".gitlab-ci.yml",
      ".Rprofile",
      ".lintr"
    )
  )
}

suggested_pkgs_names <- c(
  "DT",
  "testthat",
  "shinyValidator",
  "pkgload",
  "lubridate",
  "rmarkdown",
  "vdiffr",
  "withr"
)

shinyValidator_suggested_pkgs <- data.frame(
  name = suggested_pkgs_names,
  type = rep("suggests", length(suggested_pkgs_names)),
  min_version = c("", "3.1.2", rep("", 5), "2.4.3")
)

globalVariables("shinyValidator_suggested_pkgs")

#' Add suggested pkgs to DESCRIPTION
#'
#' These pkgs are required to perform the CICD job
#'
#' @return Edit the current Suggests DESCRIPTION fields
#' @keywords internal
add_suggested_packages <- function() {
  apply(
    shinyValidator_suggested_pkgs,
    1,
    function(pkg) {
      usethis::use_package(
        pkg[["name"]],
        type = pkg[["type"]],
        min_version = if (nchar(pkg[["min_version"]] == 0)) {
          NULL
        } else {
          pkg[["min_version"]]
        }
      )
    }
  )
}

#' Add local copy of gremlins.js
#'
#' Useful if running behind a corporate proxy.
#'
#' @keywords internal
add_gremlins_assets <- function() {
  if (!dir.exists("inst")) {
    dir.create("inst")
  }
  dir.create("inst/shinyValidator-js")
  file.copy(
    system.file("shinyValidator-js/gremlins.min.js", package = "shinyValidator"),
    "inst/shinyValidator-js/gremlins.min.js"
  )
}

#' Initialize CI/CD template
#'
#' @return A yml file with CI/CD steps.
#' @keywords internal
initialize_cicd <- function() {
  if (!file.exists("./.gitlab-ci.yml")) {
    message("Initialized GitLab CI/CD template")
    file.copy(
      from = system.file("workflows/.gitlab-ci.yml", package = "shinyValidator"),
      to = "./.gitlab-ci.yml"
    )
  }
}
