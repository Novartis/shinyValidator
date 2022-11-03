on_ci <- function() {
  isTRUE(as.logical(Sys.getenv("CI")))
}

# Useful for testing code profile on laggy code
slow_func <- function(n) {
  vec <- NULL # Or vec = c()
  for (i in seq_len(n))
    vec <- c(vec, i)
  vec
}


is_windows <- function() .Platform$OS.type == "windows"

is_mac     <- function() Sys.info()[['sysname']] == 'Darwin'

is_linux   <- function() Sys.info()[['sysname']] == 'Linux'


#' Find path to Chrome or Chromium browser
#'
#' Used by \link{check_audit_requirements}
#'
#' @author RStudio/Posit
#' @keywords internal
find_chrome <- function() {
  if (Sys.getenv("CHROMOTE_CHROME") != "") {
    return(Sys.getenv("CHROMOTE_CHROME"))
  }

  path <- NULL

  if (is_mac()) {
    path <- "/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome"

  } else if (is_windows()) {
    tryCatch(
      {
        path <- utils::readRegistry("SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\chrome.exe\\")
        path <- path[["(Default)"]]
      },
      error = function(e) {
        message("Error trying to find path to Chrome")
        path <<- NULL
      }
    )

  } else if (is_linux()) {
    path <- Sys.which("google-chrome")
    if (nchar(path) == 0) {
      path <- Sys.which("chromium-browser")
    }
    if (nchar(path) == 0) {
      path <- Sys.which("chromium")
    }
    if (nchar(path) == 0) {
      message("`google-chrome` and `chromium-browser` were not found. Try setting the CHROMOTE_CHROME environment variable or adding one of these executables to your PATH.")
      path <- NULL
    }

  } else {
    message("Platform currently not supported")
  }

  path
}
