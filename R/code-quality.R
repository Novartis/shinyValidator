#' Lint R code within project
#'
#' Uses the `{lintr}` package to check all R sources in provided paths.
#' To fine tune the lintr behavior, edit the `.lintr` file.
#'
#' @param paths Paths to check.
#' @param tolerance Errors to allow. Default to 0.
#' @export
lint_code <- function(paths = "R", tolerance = 0) {

  lints <- unlist(lapply(paths, lintr::lint_dir))
  errors <- length(lints)

  if (errors > tolerance) {
    print(lints)
    stop(sprintf("Number of style errors: %s.", errors))
  }
}

#' Build and check package
#'
#' @inheritParams audit_app
#'
#' @return Build and check package. Results are inserted into the
#' main HTML report.
#' @importFrom shiny tags HTML tagList
#' @export
check_package <- function(cran = FALSE, vignettes = FALSE, error_on = "never") {
  tmp_chk <- rcmdcheck::rcmdcheck(
    args = c(
      if (!vignettes) "--ignore-vignettes",
      "--no-manual",
      if (cran) "--as-cran"
    ),
    build_args = c(if (!vignettes) "--no-build-vignettes"),
    error_on = error_on,
    check_dir = "public"
  )

  check_res <- rcmdcheck::check_details(tmp_chk)

  # Avoids any error if test folder does not exist.
  # It is possible that people don't have tests at the
  # begining but still want to run loadtest and profiling ...
  if (dir.exists("tests")) {
    out_tmp <- readLines(
      file.path(
        sprintf(
          "public/%s.Rcheck/tests/testthat.Rout",
          check_res$package
        )
      )
    )
  }

  tests_out <- if (dir.exists("tests")) {
    HTML(paste(out_tmp, collapse = "\n"))
  } else {
    "No tests available."
  }

  steps <- list(
    "Building" = HTML(check_res$install_out),
    "Checking" = tmp_chk,
    "Testing" = tests_out
  )

  n_errors <- length(check_res$errors)
  n_warnings <- length(check_res$warnings)
  n_notes <- length(check_res$notes)
  install_status <- !grepl("had non-zero exit status", check_res$install_out)

  # count failed tests if tests exist ...
  n_failed_test <- if (dir.exists("tests")) {
    as.numeric(
      strsplit(
        trimws(
          strsplit(
            out_tmp[grep("FAIL", out_tmp)[1]],
            "FAIL"
          )[[1]][2]),
        "|"
      )[[1]][1]
    )
  } else {
    0
  }

  # Prepare check tab UI
  package_check_tab_ui <- create_tab_content(
    tags$div(
      class = "ui mini steps",
      lapply(seq_along(steps), function(i) {
        tags$div(
          class = "link step",
          tags$i(
            class = paste(
              if (names(steps)[[i]] == "Building" && install_status) {
              "green check"
            } else if (names(steps)[[i]] %in% c("Building", "Checking") && n_errors == 0 && n_warnings == 0) {
              "green check"
            } else if (names(steps)[[i]] == "Testing" && n_failed_test == 0) {
              "green check"
            } else {
              "red times"
            },
              "icon"
            )
          ),
          tags$div(
            class = "content",
            tags$div(class = "title", names(steps)[[i]]),
            tags$div(
              class = "description",
              style="white-space: pre-line;",
              if (names(steps)[[i]] == "Checking") {
                tagList(
                  create_message_div(check_res$errors),
                  create_message_div(check_res$warnings),
                  create_message_div(check_res$notes)
                )
              } else {
                steps[[i]]
              }
            )
          )
        )
      })
    ),
    tab_name = "check",
    title = "Project check"
  )

  list(
    package_name = check_res$package,
    package_version = check_res$version,
    tab_package_check = package_check_tab_ui
  )
}

#' Create message container
#'
#' Useful to display R CMD check elements like errors
#'
#' @param el Element such as list of errors, warnings...
#'
#' @keywords internal
create_message_div <- function(el) {
  tmp_type <- as.character(substitute(el))[3]
  color <- switch(tmp_type,
    "errors" = "red",
    "warnings" = "orange",
    "notes" = "blue"
  )
  if (length(el) > 0) {
    lapply(seq_along(el), function(i) {
      tags$div(
        class = sprintf("ui %s message", color),
        el[[i]]
      )
    })
  }
}
