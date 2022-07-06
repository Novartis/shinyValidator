test_that("find_pkg_suggests works", {
  # Must fail if no Suggests
  expect_error(find_pkg_suggests(system.file("tests/DESCRIPTION", package = "shinyValidator")))

  # Add 2 new packages. Must return 2
  file.copy(
    from = system.file("tests/DESCRIPTION", package = "shinyValidator"),
    to = "DESCRIPTION"
  )
  desc <- readLines("DESCRIPTION")
  new_desc <- c(desc, c("Suggests: ", "    golem", "    shiny"))
  writeLines(new_desc, "DESCRIPTION")
  tmp <- find_pkg_suggests("DESCRIPTION")
  print(tmp)
  expect_length(tmp, 2)
  file.remove("DESCRIPTION")
})
