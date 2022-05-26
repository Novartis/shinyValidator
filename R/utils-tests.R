find_pkg_suggests <- function(path = "./DESCRIPTION") {
  desc <- readLines(path)
  suggests_start <- grep("^Suggests: (.*)", desc)
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
