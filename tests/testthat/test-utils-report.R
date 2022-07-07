dummy_tab <- create_tab_content("blabla", tab_name = "Tabtest", title = "Test")
pkg_name <- "Dummy"
pkg_version <- "0.0.0.9000"

steps <- create_report_steps(
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
    output_validation = TRUE,
    coverage = TRUE,
    load_testing = TRUE,
    profile_code = TRUE,
    check_reactivity = TRUE,
    flow = TRUE
  )
  expect_true(class(tmp) == "list")
  expect_length(tmp[[1]], 3)
  expect_length(tmp[[2]], 5)

  # Disable output
  tmp <- create_report_steps(
    output_validation = FALSE,
    coverage = TRUE,
    load_testing = TRUE,
    profile_code = TRUE,
    check_reactivity = TRUE,
    flow = TRUE
  )
  expect_length(tmp[[1]], 2)
  expect_length(tmp[[2]], 5)

  # Disable coverage
  tmp <- create_report_steps(
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
  expect_length(steps_doc, 8)
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

  # Check tabs correspondance
  invisible(
    lapply(seq_along(static_steps), function(i) {
      expect_equal(static_tabs[[i]]$attribs$`data-tab`, static_steps[[i]])
    })
  )
})

#test_that("edit_html_report works", {
#  if (!dir.exists("public")) dir.create("public")
#  # testing default
#  edit_html_report(
#    source = NULL,
#    tabs_menu = tabs_menu_tag,
#    tab_package_check = create_tab_content("Check", tab_name = "check", title = "Check"),
#    tab_crash_test = create_tab_content("Crash", tab_name = "crash-test", title = "Crash"),
#    tab_output_validation = NULL,
#    tabs_static = tabs_static,
#    js_code = inject_js_helpers(steps)
#  )
#
#  expect_true(file.exists("public/index.html"))
#  tmp <- readLines("public/index.html")
#  check_tab <- grep("data-tab=\"check\"", tmp)
#  crash_tab <- grep("data-tab=\"crash-test\"", tmp)
#  coverage_tab <- grep("data-tab=\"coverage\"", tmp)
#  load_test_tab <- grep("data-tab=\"load-test\"", tmp)
#  # Must match the tab menu item + tab content + the JS helper
#  expect_length(check_tab, 3)
#  expect_length(crash_tab, 3)
#  expect_length(coverage_tab, 3)
#  expect_length(load_test_tab, 3)
#
#  # check pkg name and version
#  expect_length(grep("Dummy", tmp), 1)
#  expect_length(grep("0.0.0.9000", tmp), 1)
#
#  file.remove("public/index.html")
#
#  # testing path parameter
#  edit_html_report(
#    source = NULL,
#    tabs_menu = dummy_tab,
#    tab_package_check = dummy_tab,
#    tab_crash_test = dummy_tab,
#    tab_output_validation = dummy_tab,
#    tabs_static = dummy_tab,
#    js_code = NULL,
#    path = "public/test.html"
#  )
#  expect_true(file.exists("public/test.html"))
#  system("rm -rf public/")
#})
