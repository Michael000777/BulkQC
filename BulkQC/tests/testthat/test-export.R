testthat::test_that("export UI includes raw count distribution option", {
  ui <- mod_export_ui("export")
  rendered <- htmltools::renderTags(ui)$html

  testthat::expect_match(rendered, "Raw count distribution \\(HTML\\)")
  testthat::expect_match(rendered, "count_dist_html")
})

testthat::test_that("export metadata header includes title, timestamp, and settings", {
  exported_at <- as.POSIXct("2026-05-25 10:30:00", tz = "UTC")
  header <- bulkqc_export_metadata_header(
    title = "QC metric histogram",
    settings = list("Metric" = "lib_size", "Bins" = 40),
    exported_at = exported_at
  )

  rendered <- htmltools::renderTags(header)$html

  testthat::expect_match(rendered, "QC metric histogram")
  testthat::expect_match(rendered, "Exported from BulkQC on 2026-05-25 10:30:00 UTC")
  testthat::expect_match(rendered, "Metric:")
  testthat::expect_match(rendered, "lib_size")
  testthat::expect_match(rendered, "Bins:")
  testthat::expect_match(rendered, "40")
})

testthat::test_that("export settings evaluator handles optional metadata", {
  testthat::expect_equal(bulkqc_eval_export_settings(NULL), list())
  testthat::expect_equal(
    bulkqc_eval_export_settings(function() list("Coloring factor" = "condition")),
    list("Coloring factor" = "condition")
  )
})
