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

  tests_out <- readLines(
    file.path(
      getwd(),
      sprintf("public/%s.Rcheck/tests/testthat.Rout", check_res$package)
    )
  )

  steps <- list(
    "Building" = HTML(check_res$install_out),
    "Checking" = tmp_chk,
    "Testing" = HTML(paste(tests_out, collapse = "\n"))
  )

  n_errors <- length(check_res$errors)
  n_warnings <- length(check_res$warnings)
  n_notes <- length(check_res$notes)

  # count failed tests
  n_failed_test <- as.numeric(
    strsplit(
      trimws(
        strsplit(
          tests_out[which(grepl("FAIL", tests_out, perl = TRUE) == TRUE)],
          "FAIL"
        )[[1]][2]),
      "|"
    )[[1]][1]
  )

  # Prepare check tab UI
  package_check_tab_ui <- create_tab_content(
    tags$div(
      class = "ui mini steps",
      lapply(seq_along(steps), function(i) {
        tags$div(
          class = "link step",
          tags$i(
            class = paste(
              if (n_errors == 0 &&
                  n_warnings == 0 &&
                  n_failed_test == 0) {
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
