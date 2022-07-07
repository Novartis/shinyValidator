#' Edit HTML validation report
#'
#' Useful when items are programmatically generated
#' and the HTML output cannot be determined in advance.
#'
#' @param ... Allow for parameter expansion.
#' @param source Document to edit. Defaults to path to overwrite the
#' existing report. Set to NULL to fallback to the shinyValidator original
#' template.
#' @param path Where to write. Default to the global report in /public.
#' @return Modified HTML report in public dir.
#' @importFrom htmltools renderDocument htmlTemplate
#' @keywords internal
edit_html_report <- function(..., source = path, path = "public/index.html") {
  edited_report <- renderDocument(
    htmlTemplate(
      if (is.null(source)) {
        system.file("report/index.html", package = "shinyValidator")
      } else {
        source
      },
      ...,
      document_ = TRUE
    )
  )
  writeLines(edited_report, path)
}


#' Create a tab content for HTML report
#'
#' Used by \link{check_package}, ...
#'
#' @param ... Tab content.
#' @param tab_name Unique tab id. Must match that of \link{create_audit_report}.
#' @param title Tab title.
#'
#' @return An HTML tag representing a tab element content.
#' @keywords internal
#' @importFrom htmltools tags
create_tab_content <- function(..., tab_name, title) {
  tags$div(
    class = "ui tab",
    `data-tab` = tab_name,
    tags$div(
      class = "ui container",
      style = "margin: auto; width: 85%;",
      tags$div(
        class = "ui raised tall stacked segment",
        tags$a(
          class = "ui primary right ribbon label",
          title,
          tags$i(class = "question circle icon")
        ),
        ...
      )
    )
  )
}

#' Create a tab menu
#'
#' Useful inside \link{create_audit_report}.
#'
#' @param steps Audit steps: coverage, output validation, load testing, ...
#' @param pkg_name Audited package name.
#' @param pkg_version Audited package version.
#'
#' @return A shiny HTML tag
#' @keywords internal
create_tabs_menu <- function(steps, pkg_name, pkg_version) {
  tags$div(
    class = "ui pointing menu",
    lapply(seq_along(steps), function(i) {
      tags$a(class = "item", `data-tab` = steps[[i]], names(steps)[[i]])
    }),
    tags$div(
      class = "right menu",
      tags$a(class = "ui item", pkg_name),
      tags$a(class = "ui tag label", pkg_version)
    )
  )
}

#' Helper to process audit steps
#'
#' Useful for \link{create_audit_report}.
#'
#' @inheritParams use_validator
#'
#' @return A list composed of dynamic and static steps.
#' @keywords internal
create_report_steps <- function(
  output_validation = TRUE,
  coverage = TRUE,
  load_testing = TRUE,
  profile_code = TRUE,
  check_reactivity = TRUE,
  flow = TRUE
) {
  # Steps that are programmatically generated
  dynamic_steps <- c(
    "Package check" = "check",
    "Crash test" = "crash-test",
    "Output validation" = if (output_validation) "output" else NULL
  )

  # Steps for which report is just iframe
  static_steps <- c(
    "Coverage" = if (coverage) "coverage" else NULL,
    "Load test" = if (load_testing) "load-test" else NULL,
    "Reactivity" = if (check_reactivity) "reactlog" else NULL,
    "Code profile" = if (profile_code) "code-profile" else NULL,
    "Project structure" = if (flow) "flow" else NULL
  )

  list(dynamic_steps, static_steps)
}


#' Create static tabs
#'
#' Useful in \link{create_audit_report}.
#'
#' @param static_steps Provided by \link{create_report_steps}.
#'
#' @return A shiny html tag
#' @keywords internal
create_static_tabs <- function(static_steps) {
  lapply(seq_along(static_steps), function(i) {
    create_tab_content(
      if (static_steps[[i]] == "code-profile") tags$br(),
      tags$iframe(
        src = sprintf("./%s.html", static_steps[[i]]),
        frameborder = "0",
        scrolling = "yes",
        width = "100%",
        height = "770px"
      ),
      tab_name = static_steps[[i]],
      title = names(static_steps)[[i]]
    )
  })
}

#' Inject JS code
#'
#' Necessary to control the report interactivity
#'
#' @param steps CI/CD steps
#'
#' @return An HTML tag containing the script to be inserted in the report.
#' @keywords internal
inject_js_helpers <- function(steps) {

  help_scripts <- paste(
    vapply(seq_along(steps), function(i) {
      sprintf(
        "$('[data-tab=\"%s\"] .ribbon')
     .popup({
       title   : '%s',
       content : '%s'
     });",
     steps[[i]],
     names(steps)[[i]],
     gsub("\n ", "", steps_doc[[names(steps)[[i]]]])
      )
    }, FUN.VALUE = character(1)),
    collapse = "\n"
  )

  tags$script(
    HTML(
      sprintf(
        "$('.pointing.menu .item').tab(); // activate navigation
         $('.ui.basic.modal')
           .modal({
               blurring: true
           })
           .modal('show');
           %s
         ",
        help_scripts
      )
    )
  )
}

# Maybe needs to be automated later ...
steps_doc <- c(
  "Package check" = "Package is first built. Any error is shown in case the
  installation fails. Package is checked for consistency
  (style, files, ...). Unit tests are run. Overall you want
  to see all green.",
  "Crash test" = "Crash test ensures the app starts and takes a snapshot
  when the app is loaded. Then, the app is manipulated with headless
  browser and check for alive. Another snapshot is taken.",
  "Output validation" = "The report shows noticed output differences found during unit testing.
  It is the business user responsibility to decide whether to accept the
  difference(s).",
  "Coverage" = "Code coverage shows the amounts of code covered by unit tests.
  The higher the coverage, the higher the package reliability
  (assuming relevant unit tests).",
  "Load test" = "Load testing consists of checking whether the app can support multiple
  simultaneous sessions with reasonable performances.
  If you see a lot of blue area in the session tab, the app is
  likely not very well optimized.",
  "Reactivity" = "{reactlog} represents the reactive graph of the Shiny app.
  It is useful to identify and fix reactivity-related issues.",
  "Code profile" = "{profvis} runs the app and returns the time taken by the R code
  to finish. The lower the result, the higher the performances.
  It is useful to identify bottlenecks.",
  "Project structure" = "{flow} displays the overall project structure."
)

globalVariables("steps_doc")

#' Create a tab menu for HTML report
#'
#' @inheritParams use_validator
#' @param ... To pass extra parameters to \link{edit_html_report}.
#'
#' @return A tab menu used to navigate through \link{create_tab_content}
#' elements.
#' @keywords internal
create_audit_report <- function(
  output_validation = TRUE,
  coverage = TRUE,
  load_testing = TRUE,
  profile_code = TRUE,
  check_reactivity = TRUE,
  flow = TRUE,
  ...
) {

  items <- list(...)

  steps <- create_report_steps(
    output_validation,
    coverage,
    load_testing,
    profile_code,
    check_reactivity,
    flow
  )

  # Return all steps (dynamic steps + static steps)
  all_steps <- unlist(steps)
  static_steps <- steps[[2]]

  tabs_menu_tag <- create_tabs_menu(all_steps, items$package_name, items$package_version)

  # Setup HTML report (needs source = NULL to start from clean report)
  edit_html_report(
    source = NULL,
    tabs_menu = tabs_menu_tag,
    # Inject JS helpers
    js_code = inject_js_helpers(all_steps),
    # passed from top level function (see audit_app)
    ...,
    # Handle tabs that are just iframe (load-test, profile, reactlog, coverage, flow)
    tabs_static = create_static_tabs(static_steps)
  )
}
