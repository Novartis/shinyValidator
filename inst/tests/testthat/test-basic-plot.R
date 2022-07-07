test_that("plot works", {
  set.seed(42)
  vdiffr::expect_doppelganger(
    "Base graphics histogram",
    make_hist(500)
  )
})
