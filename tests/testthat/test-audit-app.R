test_that("check_audit_requirements works", {
  expect_message(check_audit_requirements())
})

#test_that("Audit app works", {
#  tmp <- audit_app(
#    cran = FALSE,
#    vignettes = FALSE,
#    error_on = "never",
#    timeout = 5,
#    workers = 5,
#    scope = "manual",
#    output_validation = FALSE,
#    coverage = FALSE,
#    load_testing = FALSE,
#    profile_code = FALSE,
#    check_reactivity = FALSE,
#    flow = FALSE
#  )
#
#  file.remove("public")
#})
