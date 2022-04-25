#' Checks to run before initializing the shinyValidator template
#'
#' Useful for \link{use_validator}
#' 
#' @inheritParams use_validator
#'
#' @return Error if any of the requirement is not met.
#' @keywords internal
check_setup_requirements <- function(cicd_platform) {
  message("Checking requirements ...")

  check_if_validator_installed(cicd_platform)

  if (R.version$major < "4") {
    message("Note: {shinyValidator} works better with R >=4.")
  }
  if (!file.exists("DESCRIPTION")) {
    stop("{shinyValidator} only works inside R packages.")
  }
  if (!file.exists("renv.lock")) {
    stop("Please setup {renv} to manage dependencies in your project.")
  }
  if (!file.exists("./R/app_server.R")) {
    stop("Project must contain: app_ui.R and app_server.R like in {golem} templates.")
  }
  if (!dir.exists("tests")) {
    message("No unit tests found. call usethis::use_testthat(); ...")
  }
  # Requires gh-pages branch if GitHub
  if (!is_git_repository()) {
    stop("Project is not under version control: run 'git init' ...")
  } else {
    initialize_gh_pages(cicd_platform)
  }
  message("Requirements: DONE ...")
}


#' Check if git is initialized locally
#'
#' @return Boolean.
#' @keywords internal
is_git_repository <- function() {
  dir.exists(".git")
}

#' Checks if gh_pages branch exists
#' 
#' Creates gh_pages branch if not
#'
#' @inheritParams use_validator
#' @keywords internal
initialize_gh_pages <- function(cicd_platform) {
  if (cicd_platform == "github") {
    has_gh_pages <- length(
      suppressWarnings(
        system("git rev-parse --verify gh-pages", intern = TRUE)
      )
    )
    if (has_gh_pages == 0) {
      message("Missing 'gh-pages' branch. Creating new branch ...")
      system(
        "git checkout --orphan gh-pages;
        git reset --hard;
        git commit --allow-empty -m 'fresh and empty gh-pages branch';
        git push origin gh-pages;",
        intern = TRUE
      )
      message("gh-pages: DONE ...")
    } else {
      message("gh-pages already exists. Nothing to do ...")
    }
  }
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
#' @inheritParams use_validator
#'
#' @return Boolean.
#' @keywords internal
check_if_validator_installed <- function(cicd_platform) {

  file_name <- switch(cicd_platform,
    "gitlab" = "./.gitlab-ci.yml",
    "gitlab-docker" = ".gitlab-ci.yml",
    "github" = "./.github/workflows/shiny-validator.yaml"
  )

  if (file.exists(file_name)) {
    tmp <- readLines(file_name)
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
  file.copy(
    system.file("run-app/run_app.R", package = "shinyValidator"),
    "./R/run_app.R"
  )
  # Comment out old files
  old_file <- readLines("./R/run_app-old.R")
  write(
    unlist(lapply(old_file, sub, pattern = "^", replacement = "# ")),
    "./R/run_app-old.R"
  )
}

#' Edit .Rbuildignore file
#' @inheritParams use_validator
#' @return Edit existing .Rbuidignore file with additional entries
#' @keywords internal
edit_buildignore <- function(cicd_platform) {

  cicd_ignore <- switch(cicd_platform,
    "gitlab" = ".gitlab-ci.yml",
    "gitlab-docker" = ".gitlab-ci.yml",
    "github" = ".github"
  )

  usethis::use_build_ignore(
    c(
      cicd_ignore,
      ".Rprofile",
      ".lintr",
      # if audit_app is run locally
      "public",
      "recording.log"
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
#' @inheritParams use_validator
#' @return A yml file with CI/CD steps.
#' @keywords internal
initialize_cicd <- function(cicd_platform) {
  message(sprintf("Initialized %s CI/CD template", cicd_platform))
  file_name <- switch(cicd_platform,
    "gitlab" = ".gitlab-ci.yml",
    "gitlab-docker" = "docker/.gitlab-ci.yml",
    "github" = "shiny-validator.yaml"
  )

  if (cicd_platform == "github") {
    # directory may already exist if user has GA setup
    if (!dir.exists(".github/workflows")) {
      dir.create(".github/workflows", recursive = TRUE)
    }
  }

  file.copy(
    from = system.file(
      sprintf("workflows/%s", file_name),
      package = "shinyValidator"
    ),
    to = if (cicd_platform %in% c("gitlab", "gitlab-docker")) {
      "./.gitlab-ci.yml"
    } else if (cicd_platform == "github") {
      ".github/workflows/shiny-validator.yaml"
    }
  )
}
