on_ci <- function() {
  isTRUE(as.logical(Sys.getenv("CI")))
}
