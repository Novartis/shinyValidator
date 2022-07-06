path <- tempfile(pattern = "callr")
dir.create(path)

# use withr to change directory
withr::with_dir(path, {
  test_that("Start R bg works", {
    # Copy assets
    file.copy(
      from = system.file("tests/DESCRIPTION", package = "shinyValidator"),
      to = "./DESCRIPTION"
    )
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
    add_gremlins_assets()

    devtools::document()
    devtools::load_all()

    # Recorder needs a running shiny app in the background
    # on the provided port
    expect_error(recorder_bg("3515"))
    shiny_app <- start_r_bg(shiny_bg)
    expect_s3_class(shiny_app, c("r_process", "process", "R6"))
    shiny_process <- system("netstat -plnt | grep ':3515'", intern = TRUE)
    expect_true(length(shiny_process) > 0)

    recorder_app <- start_r_bg(recorder_bg)
    recorder_process <- system("netstat -plnt | grep ':8600'", intern = TRUE)
    expect_true(length(recorder_process) > 0)

    # cleanup
    shiny_app$kill()
    recorder_app$kill()
    file.remove("recording.log")
  })
})
