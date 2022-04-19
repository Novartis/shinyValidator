path <- file.path(tempdir(), "mypkg")
dir.create(path)

# use withr to change directory
withr::with_dir(path, {
  test_that("Requirements", {
    print(packageVersion("testthat"))
    R.version$major <- 2
    print(sprintf("Changing R major to %s", R.version$major))
    expect_error(use_validator())
    R.version$major <- 4
    print(sprintf("Restoring R major to %s", R.version$major))

    # Missing description
    expect_error(use_validator())
    file.copy(
      from = system.file("tests/DESCRIPTION", package = "shinyValidator"),
      to = "./DESCRIPTION"
    )
    # Missing renv (maybe to remove...)
    expect_error(use_validator())
    file.create("renv.lock")
    # Missing app_server.R
    expect_error(use_validator())
    dir.create("R")
    file.copy(
      from = system.file("tests/app_server.R", package = "shinyValidator"),
      to = "./R/app_server.R"
    )
    file.create("./R/run_app.R")

    # Create structure
    use_validator()

    # Check up initialize_cicd() -> will change if we add GitHub...
    expect_true(file.exists("./.gitlab-ci.yml"))
    expect_length(readLines("./.gitlab-ci.yml"), 56)

    # check lintr assets
    expect_true(file.exists("./.lintr"))
    expect_length(readLines("./.lintr"), 10)

    # app files
    expect_true(file.exists("./R/run_app.R"))
    expect_true(file.exists("./R/run_app-old.R"))

    # buildignore
    expect_true(
      grepl(
        "(lintr|Rprofile|gitlab-ci)",
        paste(readLines(".Rbuildignore"), collapse = ""),
        perl = TRUE
      )
    )

    # Suggested pkgs TODO

    # gremlins
    expect_true(file.exists("inst/shinyValidator-js/gremlins.min.js"))

  })
})

# cleanup
unlink(path)
