testthat::test_that("counts validation accepts valid counts with gene IDs", {
  counts_df <- data.frame(
    gene_id = c("g1", "g2"),
    s1 = c(1, 2),
    s2 = c(3, 4),
    check.names = FALSE
  )

  counts_mat <- bulkqc_counts_df_to_matrix(counts_df, counts_has_gene_id = TRUE)

  testthat::expect_true(is.matrix(counts_mat))
  testthat::expect_equal(rownames(counts_mat), c("g1", "g2"))
  testthat::expect_equal(colnames(counts_mat), c("s1", "s2"))
})

testthat::test_that("counts validation rejects empty sample names", {
  counts_df <- data.frame(
    gene_id = c("g1", "g2"),
    s1 = c(1, 2),
    blank = c(3, 4),
    check.names = FALSE
  )
  names(counts_df)[3] <- ""

  testthat::expect_error(
    bulkqc_counts_df_to_matrix(counts_df, counts_has_gene_id = TRUE),
    "missing or empty sample column names"
  )
})

testthat::test_that("counts validation rejects duplicate sample names", {
  counts_df <- data.frame(
    gene_id = c("g1", "g2"),
    s1 = c(1, 2),
    s1_dup = c(3, 4),
    check.names = FALSE
  )
  names(counts_df)[3] <- "s1"

  testthat::expect_error(
    bulkqc_counts_df_to_matrix(counts_df, counts_has_gene_id = TRUE),
    "duplicate sample column names: s1"
  )
})

testthat::test_that("counts validation rejects duplicate gene IDs", {
  counts_df <- data.frame(
    gene_id = c("g1", "g1"),
    s1 = c(1, 2),
    s2 = c(3, 4),
    check.names = FALSE
  )

  testthat::expect_error(
    bulkqc_counts_df_to_matrix(counts_df, counts_has_gene_id = TRUE),
    "duplicate IDs: g1"
  )
})

testthat::test_that("counts validation rejects nonnumeric and missing counts", {
  counts_df <- data.frame(
    gene_id = c("g1", "g2"),
    s1 = c("1", "bad"),
    s2 = c("3", "4"),
    check.names = FALSE
  )

  testthat::expect_error(
    bulkqc_counts_df_to_matrix(counts_df, counts_has_gene_id = TRUE),
    "nonnumeric or missing values"
  )
})

testthat::test_that("counts validation rejects negative counts", {
  counts_df <- data.frame(
    gene_id = c("g1", "g2"),
    s1 = c(1, -2),
    s2 = c(3, 4),
    check.names = FALSE
  )

  testthat::expect_error(
    bulkqc_counts_df_to_matrix(counts_df, counts_has_gene_id = TRUE),
    "negative values"
  )
})

testthat::test_that("counts validation rejects zero genes and fewer than two samples", {
  zero_gene_df <- data.frame(
    gene_id = character(),
    s1 = numeric(),
    s2 = numeric(),
    check.names = FALSE
  )
  one_sample_df <- data.frame(
    gene_id = c("g1", "g2"),
    s1 = c(1, 2),
    check.names = FALSE
  )

  testthat::expect_error(
    bulkqc_counts_df_to_matrix(zero_gene_df, counts_has_gene_id = TRUE),
    "at least one gene row"
  )
  testthat::expect_error(
    bulkqc_counts_df_to_matrix(one_sample_df, counts_has_gene_id = TRUE),
    "at least two sample columns"
  )
})

testthat::test_that("metadata validation rejects missing sample ID column", {
  meta_df <- data.frame(sample = c("s1", "s2"))

  testthat::expect_error(
    bulkqc_validate_metadata(meta_df, "Sample_id", c("s1", "s2")),
    "Metadata missing column: Sample_id"
  )
})

testthat::test_that("metadata validation rejects duplicate sample IDs", {
  meta_df <- data.frame(Sample_id = c("s1", "s1"))

  testthat::expect_error(
    bulkqc_validate_metadata(meta_df, "Sample_id", c("s1", "s2")),
    "duplicate IDs: s1"
  )
})

testthat::test_that("metadata validation rejects missing count samples", {
  meta_df <- data.frame(Sample_id = c("s1", "s3"))

  testthat::expect_error(
    bulkqc_validate_metadata(meta_df, "Sample_id", c("s1", "s2")),
    "Metadata missing these samples: s2"
  )
})

testthat::test_that("metadata alignment reports extra metadata rows", {
  meta_df <- data.frame(
    Sample_id = c("s1", "s2", "s_extra"),
    condition = c("A", "B", "C")
  )

  aligned <- bulkqc_align_metadata(meta_df, "Sample_id", c("s2", "s1"))

  testthat::expect_equal(aligned$Sample_id, c("s2", "s1"))
  testthat::expect_equal(
    attr(aligned, "bulkqc_extra_metadata_samples", exact = TRUE),
    "s_extra"
  )
})

testthat::test_that("prepare qc data returns aligned valid data and extra metadata samples", {
  counts_df <- data.frame(
    gene_id = c("g1", "g2"),
    s1 = c(1, 2),
    s2 = c(3, 4),
    check.names = FALSE
  )
  meta_df <- data.frame(
    Sample_id = c("s2", "s1", "s_extra"),
    condition = c("B", "A", "C")
  )

  prepared <- bulkqc_prepare_qc_data(counts_df, meta_df)

  testthat::expect_equal(colnames(prepared$counts), c("s1", "s2"))
  testthat::expect_equal(prepared$meta$Sample_id, c("s1", "s2"))
  testthat::expect_equal(prepared$extra_metadata_samples, "s_extra")
})

