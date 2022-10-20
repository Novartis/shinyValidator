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
