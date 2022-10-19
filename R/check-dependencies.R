#' Check package dependencies
#'
#' Check package dependencies against a list of
#' predefined packages. This is useful in locked
#' environments where package versions are restricted.
#'
#' @inheritParams audit_app
#'
#' @return An HTML tag
#' @export
check_dependencies <- function(r_version = NULL, locked_deps = NULL) {

  package_names <- find_imports()
  package_data <- get_package_data(
    package_names,
    r_version,
    as.data.frame(locked_deps)
  )

  create_tab_content(
    tags$h2(
      class = "ui header",
      if (!is.null(locked_deps)) {
        "Package versions are compared against a locked source"
      } else {
        "Package versions are not subject to any restriction"
      }
    ),
    tags$div(
      class = "ui basic cards",
      package_data[[2]]
    ),
    tab_name = "dependencies",
    title = "Dependencies"
  )
}

#' Find package import
#'
#' @keywords internal
find_imports <- function() {
  deps <- desc::desc_get_deps()
  deps[deps$type == "Imports",
    "package"
  ]
}

#' Get package metadata from renv lock
#'
#' Also build a card tag for each package.
#'
#' @param package_names Obtained after running \link{find_imports}.
#' @inheritParams audit_app
#'
#' @return An HTML tag.
#' @keywords internal
get_package_data <- function(package_names, r_version, locked_deps) {
  renv_data <- jsonlite::fromJSON("renv.lock")
  renv_r_version <- renv_data$R$version

  renv_package_data <- lapply(package_names, function(pkg) {
    tmp <- renv_data$Packages[[pkg]]

    # This will show red/green if package is not/is in the provided
    # list
    card_status <-  if (nrow(locked_deps) > 0) {
      tmp_locked <- locked_deps[locked_deps$Package == tmp$Package, ]
      # Check if package is in the provided list
      if (nrow(tmp_locked) > 0) {
        if (tmp_locked$Version == tmp$Version) "green" else "red"
      } else {
        "red"
      }
    } else {
      "grey"
    }

    # Avoid to capture elements like base R, utils, ...
    if (!is.null(tmp)) {
      tags$div(
        class = paste("card", card_status),
        tags$div(
          class = "content",
          tags$div(class = "header", tmp$Package),
          tags$div(
            class = "meta",
            sprintf("Current: %s", tmp$Version),
            if (card_status == "red") {
              if (nrow(tmp_locked) == 0) {
                # Package may not be available in snapshot
                sprintf("Not available")
              } else {
                sprintf("Expected: %s", tmp_locked$Version)
              }
            } else if (card_status == "green") {
              "All good"
            }
          ),
          tags$div(
            class = "description",
            if (tmp$Source == "Repository") {
              sprintf(
                "Package taken from %s with hash code: %s",
                tmp$Repository,
                tmp$Hash
              )
            } else {
              sprintf(
                "Package taken from %s (%s/%s) with hash code: %s",
                tmp$Source,
                tmp$RemoteUsername,
                tmp$RemoteRepo,
                tmp$RemoteSha
              )
            }
          )
        )
      )
    }
  })

  list(renv_r_version, renv_package_data)
}
