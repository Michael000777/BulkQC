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

  expected_cols <- c("sample_id", "lib_size", "detected_genes", "pct_zero")
  testthat::expect_true(all(expected_cols %in% names(out)))

})

