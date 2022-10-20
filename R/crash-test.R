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
#' @inheritParams start_r_bg
#'
#' @return Errors if any of the step fails.
#'
#' @export
#' @examples
#' \dontrun{
#'  # Assuming run_app_audit is defined ...
#'  run_crash_test(
#'    headless_actions = {
#'     app$set_inputs(obs = 1000)
#'     app$get_screenshot("./plop.png")
#'    }
#'  )
#' }
run_crash_test <- function(headless_actions = NULL, timeout = NULL, ...) {
  message("\n---- BEGIN CRASH-TEST ---- \n")

  if (is.null(timeout)) {
    timeout <- if (on_ci()) 20 else 10
  }

  bg_app <- start_r_bg(shiny_bg, ...)
  chrome <- shinytest2::AppDriver$new(
    "http://127.0.0.1:3515",
    load_timeout = timeout * 1000
  )
  # wait the app to be ready before doing anything
  chrome$wait_for_idle()

  # By default AppDriver$new waits for shiny to be IDLE 200ms
  # after the initial timeout. No need for extra waiting here.
  screenshots_path <- "public/crash-test"
  if (!dir.exists(screenshots_path)) dir.create(screenshots_path, recursive = TRUE)
  chrome$get_screenshot(file.path(screenshots_path, "1-init-crash.png"))
  run_monkey_test(chrome, headless_actions, path = screenshots_path)

  # cleanup ports
  chrome$stop()
  if (bg_app$is_alive()) bg_app$kill()

  message("\n---- END CRASH-TEST ---- \n")

  create_tab_content(
    tags$div(
      class = "ui equal width grid",
      lapply(list.files(screenshots_path), function(screenshot) {
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
call_gremlins <- function(headless_app, screenshot = TRUE, path) {
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
    headless_app$get_screenshot(file.path(path, "2-gremlins.png"), delay = 3)
  }
  # Wait remaining 7 seconds so that gremlins are over
  Sys.sleep(7)
}



#' Perform basic monkey testing
#'
#' Internally required by \link{record_app}, \link{run_crash_test}, ... after
#' the headless connection is opened.
#'
#' @param app Headless app R6 instance.
#' @param screenshot Whether to take screenshot. Defaults to TRUE. Only works
#' if headles_actions is NULL.
#' @param path Screenshot path.
#' @inheritParams run_crash_test
#' @keywords internal
run_monkey_test <- function(app, headless_actions, screenshot = TRUE, path) {
  if (is.null(headless_actions)) {
    call_gremlins(app, screenshot, path)
  } else {
    message("Running custom headless crash test ...")
    # Capture expression and convert to string
    tmp <- deparse(headless_actions)
    # Modify any app$screenshot
    line_to_modify <- grep(
      "app$get_screenshot",
      tmp,
      fixed = TRUE
    )
    tmp <- gsub(
      "app$get_screenshot(",
      "if (screenshot) app$get_screenshot(file.path(path, ",
      tmp,
      fixed = TRUE
    )
    # Add closing ) after for file.path
    for (line in line_to_modify) {
      tmp[[line]] <- paste0(tmp[[line]], ")")
    }
    # Parse + eval
    eval(parse(text = tmp))
  }
}



