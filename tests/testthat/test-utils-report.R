dummy_tab <- create_tab_content("blabla", tab_name = "Tabtest", title = "Test")
pkg_name <- "Dummy"
pkg_version <- "0.0.0.9000"

steps <- create_report_steps(
  crash_test = TRUE,
  output_validation = FALSE,
  coverage = TRUE,
  load_testing = TRUE,
  profile_code = FALSE,
  check_reactivity = FALSE,
  flow = FALSE
)

all_steps <- unlist(steps)
menu <- create_tabs_menu(all_steps, pkg_name, pkg_version)

static_steps <- steps[[2]]
static_tabs <- create_static_tabs(static_steps)

test_that("create_report_steps works", {
  # full steps
  tmp <- create_report_steps(
    crash_test = TRUE,
    output_validation = TRUE,
    coverage = TRUE,
    load_testing = TRUE,
    profile_code = TRUE,
    check_reactivity = TRUE,
    flow = TRUE
  )
  expect_true(class(tmp) == "list")
  expect_length(tmp[[1]], 4)
  expect_length(tmp[[2]], 5)

  # Disable output
  tmp <- create_report_steps(
    crash_test = FALSE,
    output_validation = TRUE,
    coverage = TRUE,
    load_testing = TRUE,
    profile_code = TRUE,
    check_reactivity = TRUE,
    flow = TRUE
  )
  expect_length(tmp[[1]], 3)
  expect_length(tmp[[2]], 5)

  # Disable coverage
  tmp <- create_report_steps(
    crash_test = FALSE,
    output_validation = FALSE,
    coverage = FALSE,
    load_testing = TRUE,
    profile_code = TRUE,
    check_reactivity = TRUE,
    flow = TRUE
  )
  expect_length(tmp[[1]], 2)
  expect_length(tmp[[2]], 4)

  # Disable load testing
  tmp <- create_report_steps(
    crash_test = FALSE,
    output_validation = FALSE,
    coverage = FALSE,
    load_testing = FALSE,
    profile_code = TRUE,
    check_reactivity = TRUE,
    flow = TRUE
  )
  expect_length(tmp[[1]], 2)
  expect_length(tmp[[2]], 3)

  # Disable profiling
  tmp <- create_report_steps(
    crash_test = FALSE,
    output_validation = FALSE,
    coverage = FALSE,
    load_testing = FALSE,
    profile_code = FALSE,
    check_reactivity = TRUE,
    flow = TRUE
  )
  expect_length(tmp[[1]], 2)
  expect_length(tmp[[2]], 2)

  # Disable reactlog
  tmp <- create_report_steps(
    crash_test = FALSE,
    output_validation = FALSE,
    coverage = FALSE,
    load_testing = FALSE,
    profile_code = FALSE,
    check_reactivity = FALSE,
    flow = TRUE
  )
  expect_length(tmp[[1]], 2)
  expect_length(tmp[[2]], 1)

  # Disable flow
  tmp <- create_report_steps(
    crash_test = FALSE,
    output_validation = FALSE,
    coverage = FALSE,
    load_testing = FALSE,
    profile_code = FALSE,
    check_reactivity = FALSE,
    flow = FALSE
  )
  expect_length(tmp[[1]], 2)
  expect_length(tmp[[2]], 0)
})

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
  tmp <- inject_js_helpers(all_steps)
  expect_s3_class(tmp, "shiny.tag")
  expect_equal(tmp$name, "script")
  expect_s3_class(tmp$children[[1]], c("html", "character"))
})

test_that("Steps number correct", {
  expect_length(steps_doc, 9)
})

test_that("create_tabs_menu works", {
  expect_s3_class(menu, "shiny.tag")
  expect_equal(menu$attribs$class, "ui pointing menu")

  # Check that menu items correspond to steps
  menu_items <- menu$children[[1]]
  invisible(
    lapply(seq_along(all_steps), function(i) {
      expect_equal(menu_items[[i]]$attribs$`data-tab`, all_steps[[i]])
    })
  )

  # Check pkg name and version
  menu_meta <- menu$children[[2]]
  expect_equal(menu_meta$children[[1]]$children[[1]], pkg_name)
  expect_equal(menu_meta$children[[2]]$children[[1]], pkg_version)
})

test_that("create_static_tabs works", {
  # Step is length 2 so should be static tabs
  expect_length(static_tabs, 2)

  # Check tabs correspondence
  invisible(
    lapply(seq_along(static_steps), function(i) {
      expect_equal(static_tabs[[i]]$attribs$`data-tab`, static_steps[[i]])
    })
  )
})
