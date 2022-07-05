path <- tempfile(pattern = "setup")
dir.create(path)

# use withr to change directory
withr::with_dir(path, {
  test_that("Requirements", {
    # Missing description
    expect_error(use_validator())
    file.copy(
      from = system.file("tests/DESCRIPTION", package = "shinyValidator"),
      to = "./DESCRIPTION"
    )
    file.create("./.Rbuildignore")
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
    file.copy(
      from = system.file("tests/app_ui.R", package = "shinyValidator"),
      to = "./R/app_ui.R"
    )
    file.copy(
      from = system.file("run-app/run_app.R", package = "shinyValidator"),
      to = "./R/run_app.R"
    )

    # Git does not exist
    expect_error(use_validator())
    system("git init")

    # Create structure
    use_validator(cicd_platform = "gitlab")

    # Check up initialize_cicd() -> will change if we add GitHub...
    expect_true(file.exists("./.gitlab-ci.yml"))
    expect_length(readLines("./.gitlab-ci.yml"), 55)

    # check lintr assets
    expect_true(file.exists("./.lintr"))
    expect_length(readLines("./.lintr"), 10)

    # app files
    expect_true(file.exists("./R/run_app.R"))
    expect_true(file.exists("./R/run_app-old.R"))

    # buildignore
    expect_true(file.exists("./.Rbuildignore"))
    expect_true(
      grepl(
        "(lintr|Rprofile|gitlab-ci)",
        paste(readLines("./.Rbuildignore"), collapse = ""),
        perl = TRUE
      )
    )

    # Suggested pkgs
    system("cat DESCRIPTION")
    suggests <- find_pkg_suggests()
    suggests_test <- sum(suggested_pkgs_names %in% suggests)
    expect_true(length(suggests) == suggests_test)

    # gremlins
    expect_true(file.exists("inst/shinyValidator-js/gremlins.min.js"))
  })

  test_that("Crash test works", {
    devtools::document()
    devtools::load_all()
    crash_test_tab <- run_crash_test()

    # Check that returned content is HTML
    screenshots <- list.files("public/crash-test")
    expect_length(screenshots, 2)
    expect_s3_class(crash_test_tab, "shiny.tag")
    expect_equal(crash_test_tab$attribs$class, "ui tab")

    # Check that nothing runs anymore
    # run_crash_test is supposed to cleanup
    print(system("netstat -plnt", intern = TRUE))
    expect_warning(system("netstat -plnt | grep ':3515'", intern = TRUE))
    # TO DO: check why chrome is still alive despite being killed...
    #chrome_process <- system("netstat -plnt | grep 'google-chrom'", intern = TRUE)
    #expect_true(length(chrome_process) == 0)

    # Check that screenshots are taken
    expect_true(file.exists("public/crash-test/1-init-crash.png"))
    expect_true(file.exists("public/crash-test/2-gremlins.png"))
  })
})

# cleanup
unlink(path)
