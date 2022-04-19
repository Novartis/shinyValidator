# This will only work on the SCC 4.1.0 Gitlab runner.
test_that("App starts and does not crash", {

  # Don't run these tests on the CRAN build servers
  #skip_on_cran()
  run_crash_test()
})
