test_that("echarts4r ok", {
  snapshot_widget <- function(path) {
    htmlwidgets::saveWidget(make_echart("Income"), path)
    path
  }
  
  expect_snapshot_file(snapshot_widget("tmp_echarts_plot.html"), "echarts_plot.html")
})
