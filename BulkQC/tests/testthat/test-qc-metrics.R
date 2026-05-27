testthat::test_that("compute_qc_metrics returns expected shape", {
  counts <- matrix(
    c(1,2,3,4,  5,6,7,8),
    nrow = 4,
    ncol = 2,
    dimnames = list(paste0("g", 1:4), c("s1", "s2"))
  )

  out <- compute_qc_metrics(counts)

  testthat::expect_s3_class(out, "data.frame")
  testthat::expect_equal(nrow(out), ncol(counts))

  expected_cols <- c(
    "sample_id",
    "lib_size",
    "detected_genes",
    "pct_zero",
    "median_count",
    "upper_quartile_count",
    "detected_ge_1",
    "detected_ge_10",
    "pct_counts_top_10",
    "pct_counts_top_50",
    "pct_counts_top_100"
  )
  testthat::expect_true(all(expected_cols %in% names(out)))

})

testthat::test_that("compute_qc_metrics returns deterministic expanded metrics", {
  counts <- matrix(
    c(
      0, 2,
      1, 0,
      10, 20,
      100, 0,
      5, 8
    ),
    nrow = 5,
    ncol = 2,
    byrow = TRUE,
    dimnames = list(paste0("g", 1:5), c("s1", "s2"))
  )

  out <- compute_qc_metrics(counts)

  testthat::expect_equal(out$lib_size, c(116, 30))
  testthat::expect_equal(out$detected_genes, c(4, 3))
  testthat::expect_equal(out$pct_zero, c(20, 40))
  testthat::expect_equal(out$median_count, c(5, 2))
  testthat::expect_equal(out$upper_quartile_count, c(10, 8))
  testthat::expect_equal(out$detected_ge_1, c(4, 3))
  testthat::expect_equal(out$detected_ge_10, c(2, 1))
  testthat::expect_equal(out$pct_counts_top_10, c(100, 100))
  testthat::expect_equal(out$pct_counts_top_50, c(100, 100))
  testthat::expect_equal(out$pct_counts_top_100, c(100, 100))
})

testthat::test_that("compute_qc_metrics supports configurable thresholds", {
  counts <- matrix(
    c(
      0, 2,
      1, 0,
      10, 20,
      100, 0,
      5, 8
    ),
    nrow = 5,
    ncol = 2,
    byrow = TRUE,
    dimnames = list(paste0("g", 1:5), c("s1", "s2"))
  )

  out <- compute_qc_metrics(
    counts,
    detected_thresholds = c(5, 20),
    top_n = c(1, 2)
  )

  testthat::expect_true(all(c("detected_ge_5", "detected_ge_20") %in% names(out)))
  testthat::expect_true(all(c("pct_counts_top_1", "pct_counts_top_2") %in% names(out)))
  testthat::expect_equal(out$detected_ge_5, c(3, 2))
  testthat::expect_equal(out$detected_ge_20, c(1, 1))
  testthat::expect_equal(out$pct_counts_top_1, c(100 / 116 * 100, 20 / 30 * 100))
  testthat::expect_equal(out$pct_counts_top_2, c(110 / 116 * 100, 28 / 30 * 100))
})

testthat::test_that("top-N percentage helper handles zero-library samples", {
  testthat::expect_equal(bulkqc_pct_counts_in_top_n(c(0, 0, 0), 2), 0)
  testthat::expect_equal(bulkqc_pct_counts_in_top_n(c(100, 50, 25, 0), 2), 150 / 175 * 100)
})

testthat::test_that("QC metric choices include expanded metrics", {
  choices <- bulkqc_qc_metric_choices()

  testthat::expect_equal(choices[["Library Size"]], "lib_size")
  testthat::expect_equal(choices[["Median Count"]], "median_count")
  testthat::expect_equal(choices[["Upper-Quartile Count"]], "upper_quartile_count")
  testthat::expect_equal(choices[["Detected Genes (>= 10)"]], "detected_ge_10")
  testthat::expect_equal(choices[["% Counts in Top 100 Genes"]], "pct_counts_top_100")
})
