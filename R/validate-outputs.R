#' Create output HTML tab
#'
#' Useful for \link{validate_outputs}.
#'
#' @param done Boolean. Internal to \link{validate_outputs}.
#'
#' @return A shiny HTML tag
#' @keywords internal
create_output_tab <- function(done) {
  tmp_html <- if (done) {

    output_snaps <- list.files("public/outputs", pattern = ".html$", full.names = TRUE)

    tags$div(
      class = "ui equal width grid",
      lapply(seq_along(output_snaps), function(i) {
        tags$div(
          class = "eight wide column",
          tags$iframe(
            src = sprintf("./%s", strsplit(output_snaps[[i]], "public/")[[1]][2]),
            frameborder = "0",
            scrolling = "yes",
            width = "100%",
            height = "770px"
          )
        )
      })
    )

  } else {
    tags$h1(class = "ui header", "No visual change to review")
  }

  create_tab_content(
    tmp_html,
    tab_name = "output",
    title = "Output validation"
  )
}

#' Validate plot outputs
#'
#' @return For each snapshot folder found in tests/testthat/_snaps
#' save a standalone htmlwidget html page to be included in the final report.
#' @export
validate_outputs <- function() {
  message("\n---- BEGIN COMPARE OUTPUTS ---- \n")

  if (!dir.exists("tests/testthat/_snaps")) {
    stop(
      "No snapshot folder found. Make sure to use expect_snapshot_file within
         your testthat unit tests.
      ")
  }

  outputs <- list.dirs("tests/testthat/_snaps", recursive = FALSE)
  done <- FALSE

  # loop over all existing snapshots
  lapply(outputs, function(output) {

    tmp_file <- utils::tail(strsplit(output, "/")[[1]], n = 1)
    old <- grep("^.(?!.*new)", list.files(output), perl = TRUE, value = TRUE)
    # handle htmlwidget vs plots
    ext <- if (grepl(".svg$", old)) "svg" else "html"
    new <- list.files(output, pattern = sprintf("new.%s$", ext))

    # Don't do anything if there is nothing to review
    if (length(new) == 1) {
      if (!dir.exists("public/outputs")) dir.create("public/outputs")

      target_file <- sprintf("public/outputs/%s-validation.html", tmp_file)

      htmlwidgets::saveWidget(
        diffviewer::visual_diff(
          sprintf("%s/%s", output, old),
          sprintf("%s/%s", output, new)
        ),
        target_file
      )

      done <<- TRUE

    } else {
      message(
        sprintf("No visual change to review in the %s folder", output)
      )
    }
  })

  # Tag to display in the report
  create_output_tab(done)

  message("\n---- END COMPARE OUTPUTS ---- \n")
}
