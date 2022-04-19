#' Crash test for Shiny app
#'
#' Function required to perform
#' crash test for a Shiny app. Test whether the app starts and
#' whether it stays alive after a series of random clicks (monkey test)
#' or targeted actions.
#'
#' @note If the Shiny app takes time to load, you may pass load_timeout
#' parameter with duration in ms. For instance load_timeout = 5000,
#' would wait 5 seconds.
#'
#' @inheritParams audit_app
#'
#' @return Errors if any of the step fails.
#'
#' @export
run_crash_test <- function(timeout = 5, headless_actions = NULL) {
  message("\n---- BEGIN CRASH-TEST ---- \n")
  bg_app <- start_r_bg(shiny_bg)
  chrome <- shinytest2::AppDriver$new(
    "http://127.0.0.1:3515",
    load_timeout = timeout * 1000
  )
  # By default AppDriver$new waits for shiny to be IDLE 200ms
  # after the initial timeout. No need for extra waiting here.
  if (!dir.exists("public/crash-test")) dir.create("public/crash-test", recursive = TRUE)
  chrome$get_screenshot("public/crash-test/1-init-crash.png")
  run_monkey_test(chrome, headless_actions)

  # cleanup ports
  chrome$stop()
  if (bg_app$is_alive()) bg_app$kill()

  screenshots <- list.files("public/crash-test")

  create_tab_content(
    tags$div(
      class = "ui equal width grid",
      lapply(screenshots, function(screenshot) {
        tags$div(
          class = "eight wide column",
          tags$h2(class = "ui header", strsplit(screenshot, "\\.")[[1]][1]),
          tags$img(src = file.path("./crash-test", screenshot), width="100%")
        )
      })
    ),
    tab_name = "crash-test",
    title = "Crash test"
  )
}


#' Inject and call gremlins
#'
#' Internally required by \link{run_monkey_test}. Tries
#' to workaround any proxy settings
#'
#' @inheritParams run_monkey_test
#' @keywords internal
call_gremlins <- function(headless_app, screenshot = TRUE) {
  message("Injecting gremlins.js script")

  # allows flexibility if running behind proxy
  gremlins_path <- if (dir.exists("inst/shinyValidator-js")) {
    "./gremlins/gremlins.min.js"
  } else {
    "https://unpkg.com/gremlins.js"
  }

  headless_app$run_js(
    sprintf("
      var s = document.createElement('script');
      s.src = '%s';
      document.body.appendChild(s);
    ", gremlins_path)
  )

  message("Checking gremlins ...")
  Sys.sleep(1)
  check_gremlins <- headless_app$get_js("typeof window.gremlins")
  if (check_gremlins == "undefined") {
    stop("gremlins are not properly injected. Are you behind a proxy?")
  }

  message("Unleashing gremlins ... This runs about 10 seconds.")

  headless_app$run_js(
    "gremlins.createHorde({
      randomizer: new gremlins.Chance(1234), // repeatable
      species: [
        gremlins.species.clicker(),
        gremlins.species.toucher(),
        gremlins.species.formFiller(),
        gremlins.species.typer()
      ],
      mogwais: [gremlins.mogwais.alert(), gremlins.mogwais.gizmo()],
      strategies: [gremlins.strategies.distribution()]
     }).unleash().then(() => {
      console.log('Gremlins test success')
    });"
  )

  if (screenshot) {
    Sys.sleep(3)
    headless_app$get_screenshot("public/crash-test/2-gremlins.png")
  }
  # Wait remaining 7 seconds so that gremlins are over
  Sys.sleep(7)
}



#' Perform basic monkey testing
#'
#' Internally required by \link{record_app}, \link{run_crash_test}, ... after
#' the headless connection is opened.
#'
#' @param headless_app Headless app R6 instance.
#' @param screenshot Whether to take screenshot. Defaults to TRUE.
#' @inheritParams run_crash_test
#' @keywords internal
run_monkey_test <- function(headless_app, headless_actions, screenshot = TRUE) {
  if (is.null(headless_actions)) {
    call_gremlins(headless_app, screenshot)
  } else {
    # Allow \n in case headless_actions had multiple lines
    headless_actions <- gsub("\n", " ", headless_actions)
    eval(parse(text = headless_actions))
    if (screenshot) {
      headless_app$get_screenshot("public/crash-test/2-gremlins.png")
    }
  }
}
