testthat::test_that("export UI includes raw count distribution option", {
  ui <- mod_export_ui("export")
  rendered <- htmltools::renderTags(ui)$html

  testthat::expect_match(rendered, "Raw count distribution \\(HTML\\)")
  testthat::expect_match(rendered, "count_dist_html")
})
