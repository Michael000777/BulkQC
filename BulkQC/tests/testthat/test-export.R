testthat::test_that("export UI includes configured export options", {
  ui <- mod_export_ui("export")
  rendered <- htmltools::renderTags(ui)$html

  testthat::expect_match(rendered, "QC table \\(CSV\\)")
  testthat::expect_match(rendered, "PCA plot \\(HTML\\)")
  testthat::expect_match(rendered, "QC histogram \\(HTML\\)")
  testthat::expect_match(rendered, "Raw count distribution \\(HTML\\)")

  testthat::expect_match(rendered, "qc_csv")
  testthat::expect_match(rendered, "pca_html")
  testthat::expect_match(rendered, "hist_html")
  testthat::expect_match(rendered, "count_dist_html")
})

testthat::test_that("export defaults are stable", {
  testthat::expect_equal(
    bulkqc_export_default_options(),
    c("qc_csv", "pca_html")
  )
})

testthat::test_that("export filenames match bundle selections", {
  testthat::expect_equal(
    bulkqc_export_filenames(c("qc_csv", "pca_html")),
    c("qc_table.csv", "pca_plot.html")
  )

  testthat::expect_equal(
    bulkqc_export_filenames(c("hist_html", "count_dist_html")),
    c("qc_histogram.html", "count_distribution.html")
  )

  testthat::expect_error(
    bulkqc_export_filenames("not_an_export"),
    "Unknown export option"
  )
})

testthat::test_that("unavailable export messages are explicit", {
  testthat::expect_equal(
    bulkqc_export_unavailable_message("pca_html"),
    "PCA export not wired in server."
  )

  testthat::expect_equal(
    bulkqc_export_unavailable_message("hist_html"),
    "Histogram not available yet: choose a metric and bins first."
  )

  testthat::expect_equal(
    bulkqc_export_unavailable_message("count_dist_html"),
    "Count distribution not available yet: choose a scope and bins first."
  )

  testthat::expect_error(
    bulkqc_export_unavailable_message("qc_csv"),
    "Unknown export option"
  )
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
