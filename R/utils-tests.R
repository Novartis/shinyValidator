find_pkg_suggests <- function(path = "./DESCRIPTION") {
  desc <- readLines(path)
  suggests_start <- grep("^Suggests: (.*)", desc)
  if (length(suggests_start) == 0) {
    stop("This package does not have any 'Suggests' field.")
  }
  suggests_end <- NULL
  for (i in seq_along(desc)) {
    if (suggests_start + i <= length(desc)) {
      tmp <- grepl("    ", desc[[suggests_start + i]])
      if (!tmp) {
        suggests_end <- i + suggests_start - 1
        break
      }
    } else {
      suggests_end <- i + suggests_start - 1
      break
    }
  }
  suggests_start <- suggests_start + 1
  trimws(gsub(",", "", desc[suggests_start:suggests_end]))
}

copy_shiny_app_files <- function() { # nocov start
  if (!dir.exists("R")) dir.create("R")
  file.copy(
    from = system.file("tests/app_server.R", package = "shinyValidator"),
    to = "./R/app_server.R"
  )
  file.copy(
    from = system.file("tests/app_ui.R", package = "shinyValidator"),
    to = "./R/app_ui.R"
  )
  file.copy(
    from = system.file("run-app/run_app_audit.R", package = "shinyValidator"),
    to = "./R/run_app_audit.R"
  )
  file.copy(
    from = system.file("tests/utils.R", package = "shinyValidator"),
    to = "./R/utils.R"
  )
} # nocov end
