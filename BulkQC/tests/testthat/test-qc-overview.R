testthat::test_that("QC Overview UI exposes expanded metric selector choices", {
  ui <- mod_qc_overview_ui("qc")
  rendered <- htmltools::renderTags(ui)$html

  testthat::expect_match(rendered, "Median Count")
  testthat::expect_match(rendered, "Upper-Quartile Count")
  testthat::expect_match(rendered, "Detected Genes \\(&gt;= 10\\)")
  testthat::expect_match(rendered, "% Counts in Top 100 Genes")
})

