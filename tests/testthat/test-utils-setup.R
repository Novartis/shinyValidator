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

    # Prepare for next steps
    devtools::document()
    devtools::load_all()
  })

  test_that("Check package works", {
    tmp <- check_package()
    expect_equal(tmp$package_name, "Dummy")
    expect_equal(tmp$package_version, "0.0.0.9000")
    expect_s3_class(tmp$tab_package_check, "shiny.tag")
  })

  test_that("Crash test works", {
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

  test_that("Load test work", {
    record_app()

    # Check that nothing runs anymore
    print(system("netstat -plnt", intern = TRUE))
    expect_warning(system("netstat -plnt | grep ':3515'", intern = TRUE))
    expect_warning(system("netstat -plnt | grep ':8600'", intern = TRUE))

    # Check that load test artifacts are produces
    expect_true(dir.exists("run1"))
    expect_true(file.exists("recording.log"))
    expect_true(file.exists("public/load-test.html"))
  })

  test_that("Profiling works", {
    profile_app()
    expect_true(file.exists("public/code-profile.html"))
  })

  test_that("Reactlog works", {
    upload_reactlog()
    expect_true(file.exists("public/reactlog.html"))
  })

  test_that("Validate output works", {
    # Error if no snapshots
    expect_error(validate_outputs())
    usethis::use_testthat()
    file.copy(
      system.file("tests/testthat", package = "shinyValidator"),
      "tests",
      recursive = TRUE
    )
    output_tab_tag <- validate_outputs()
    expect_s3_class(output_tab_tag, "shiny.tag")
  })

  test_that("Has git remote works", {
    # No remote is currently setup
    expect_length(has_git_remote(), 1)
  })

  test_that("Find main branch works", {
    # maybe not always TRUE ... depending on people's config
    expect_length(find_main_branch(), 0)
  })

  test_that("initialize_gh_pages works", {
    # No remote
    expect_error(initialize_gh_pages("github"))
  })

  test_that("check_if_validator_installed works", {
    expect_error(check_if_validator_installed("gitlab"))
  })

  test_that("audit app works", {
    # cleanup for audit_app
    system("rm -rf tests public run1")
    file.remove("recording.log")

    audit_app(
      output_validation = FALSE,
      coverage = TRUE,
      load_testing = TRUE,
      profile_code = TRUE,
      check_reactivity = TRUE,
      flow = FALSE
    )

    expect_true(file.exists("public/index.html"))
    # Check that 4 static docs exist
    expect_true(file.exists("public/load-test.html"))
    expect_true(file.exists("public/coverage.html"))
    expect_true(file.exists("public/reactlog.html"))
    expect_true(file.exists("public/code-profile.html"))

    # Inspect report
    tmp <- readLines("public/index.html")
    check_tab <- grep("data-tab=\"check\"", tmp)
    crash_tab <- grep("data-tab=\"crash-test\"", tmp)
    coverage_tab <- grep("data-tab=\"coverage\"", tmp)
    load_test_tab <- grep("data-tab=\"load-test\"", tmp)
    profile_tab <- grep("data-tab=\"code-profile\"", tmp)
    reactivity_tab <- grep("data-tab=\"reactlog\"", tmp)
    # Must match the tab menu item + tab content + the JS helper
    expect_length(check_tab, 3)
    expect_length(crash_tab, 3)
    expect_length(coverage_tab, 3)
    expect_length(load_test_tab, 3)
    expect_length(profile_tab, 3)
    expect_length(reactivity_tab, 3)

    # TO DO: more checks ... Maybe convert to Shiny tags and inspect structure ...
  })
})

# cleanup
unlink(path)
