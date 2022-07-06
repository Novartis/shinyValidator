dummy_tab <- create_tab_content("blabla", tab_name = "Tabtest", title = "Test")

coverage <- load_testing <- TRUE
output_validation <- check_reactivity <- profile_code <- flow <- FALSE
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

steps <- c(dynamic_steps, static_steps)

test_that("Create tab content works", {
  expect_s3_class(dummy_tab, "shiny.tag")
  # Check tab name
  expect_equal(dummy_tab$attribs$`data-tab`, "Tabtest")
  tag_query <- htmltools::tagQuery(dummy_tab)
  title <- tag_query$
    find("a")$
    selectedTags()[[1]]$
    children[[1]][1]
  expect_equal(title, "Test")
})

test_that("Inject JS helpers works", {
  tmp <- inject_js_helpers(steps)
  expect_s3_class(tmp, "shiny.tag")
  expect_equal(tmp$name, "script")
  expect_s3_class(tmp$children[[1]], c("html", "character"))
})

test_that("Steps number correct", {
  expect_length(steps_doc, 8)
})

test_that("edit_html_report works", {
  if (!dir.exists("public")) dir.create("public")
  # testing default

  tabs_static = lapply(seq_along(static_steps), function(i) {
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

  tabs_menu_tag <- tags$div(
    class = "ui pointing menu",
    lapply(seq_along(steps), function(i) {
      tags$a(class = "item", `data-tab` = steps[[i]], names(steps)[[i]])
    }),
    tags$div(
      class = "right menu",
      tags$a(class = "ui item", "Dummy"),
      tags$a(class = "ui tag label", "0.0.0.9000")
    )
  )

  expect_length(tabs_static, 2)

  edit_html_report(
    source = NULL,
    tabs_menu = tabs_menu_tag,
    tab_package_check = create_tab_content("Check", tab_name = "check", title = "Check"),
    tab_crash_test = create_tab_content("Crash", tab_name = "crash-test", title = "Crash"),
    tab_output_validation = NULL,
    tabs_static = tabs_static,
    js_code = inject_js_helpers(steps)
  )

  expect_true(file.exists("public/index.html"))
  tmp <- readLines("public/index.html")
  check_tab <- grep("data-tab=\"check\"", tmp)
  crash_tab <- grep("data-tab=\"crash-test\"", tmp)
  coverage_tab <- grep("data-tab=\"coverage\"", tmp)
  load_test_tab <- grep("data-tab=\"load-test\"", tmp)
  # Must match the tab menu item + tab content + the JS helper
  expect_length(check_tab, 3)
  expect_length(crash_tab, 3)
  expect_length(coverage_tab, 3)
  expect_length(load_test_tab, 3)

  # check pkg name and version
  expect_length(grep("Dummy", tmp), 1)
  expect_length(grep("0.0.0.9000", tmp), 1)

  file.remove("public/index.html")

  # testing path parameter
  edit_html_report(
    source = NULL,
    tabs_menu = dummy_tab,
    tab_package_check = dummy_tab,
    tab_crash_test = dummy_tab,
    tab_output_validation = dummy_tab,
    tabs_static = dummy_tab,
    js_code = NULL,
    path = "public/test.html"
  )
  expect_true(file.exists("public/test.html"))
  system("rm -rf public/")
})

#test_that("Create report tabs works", {
#  if (!dir.exists("public")) dir.create("public")
#  file.create("public/index.html")
#  tmp <- create_report_tabs(
#    output_validation = FALSE,
#    coverage = FALSE,
#    load_testing = FALSE,
#    profile_code = FALSE,
#    check_reactivity = FALSE,
#    flow = FALSE,
#    tab_package_check = dummy_tab,
#    tab_crash_test = dummy_tab,
#    tab_output_validation = dummy_tab
#  )
#})
