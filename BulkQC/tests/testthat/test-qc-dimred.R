testthat::test_that("bulkqc_compute_pca returns scores and named variance", {
  counts <- matrix(
    c(
      10, 20, 30, 40,
      12, 22, 34, 48,
      50, 40, 30, 20,
      5, 10, 15, 20,
      100, 80, 60, 40
    ),
    nrow = 5,
    byrow = TRUE,
    dimnames = list(paste0("g", 1:5), paste0("s", 1:4))
  )

  out <- bulkqc_compute_pca(counts)

  testthat::expect_s3_class(out$scores, "data.frame")
  testthat::expect_equal(out$scores$sample_id, colnames(counts))
  testthat::expect_true(all(c("PC1", "PC2") %in% names(out$scores)))
  testthat::expect_true(all(c("PC1", "PC2") %in% names(out$variance_explained)))
  testthat::expect_equal(out$variable_gene_count, nrow(counts))
})

testthat::test_that("bulkqc_compute_pca rejects too few samples", {
  counts <- matrix(
    c(1, 2, 3),
    nrow = 3,
    ncol = 1,
    dimnames = list(paste0("g", 1:3), "s1")
  )

  testthat::expect_error(
    bulkqc_compute_pca(counts),
    "PCA requires at least two samples"
  )
})

testthat::test_that("bulkqc_compute_pca rejects too few variable genes", {
  counts <- matrix(
    10,
    nrow = 3,
    ncol = 3,
    dimnames = list(paste0("g", 1:3), paste0("s", 1:3))
  )

  testthat::expect_error(
    bulkqc_compute_pca(counts),
    "PCA requires at least two variable genes"
  )
})

testthat::test_that("bulkqc_compute_pca rejects nonnumeric counts", {
  counts <- matrix(
    c("a", "b", "c", "d"),
    nrow = 2,
    ncol = 2,
    dimnames = list(c("g1", "g2"), c("s1", "s2"))
  )

  testthat::expect_error(
    bulkqc_compute_pca(counts),
    "PCA requires numeric counts"
  )
})

testthat::test_that("bulkqc_available_pcs lists score columns only", {
  counts <- matrix(
    c(1, 2, 3, 4, 5, 6, 7, 8, 10, 12, 14, 16),
    nrow = 4,
    ncol = 3,
    dimnames = list(paste0("g", 1:4), paste0("s", 1:3))
  )

  out <- bulkqc_compute_pca(counts)

  testthat::expect_equal(bulkqc_available_pcs(out), c("PC1", "PC2", "PC3"))
})

testthat::test_that("bulkqc_validate_pca_axes accepts available distinct axes", {
  counts <- matrix(
    c(1, 2, 3, 4, 5, 7, 7, 8, 9, 11, 10, 12),
    nrow = 4,
    ncol = 3,
    dimnames = list(paste0("g", 1:4), paste0("s", 1:3))
  )
  out <- bulkqc_compute_pca(counts)

  testthat::expect_true(isTRUE(bulkqc_validate_pca_axes(out, "PC1", "PC2")))
})

testthat::test_that("bulkqc_validate_pca_axes rejects unavailable or duplicate axes", {
  counts <- matrix(
    c(1, 2, 3, 4, 5, 7, 7, 8, 9, 11, 10, 12),
    nrow = 4,
    ncol = 3,
    dimnames = list(paste0("g", 1:4), paste0("s", 1:3))
  )
  out <- bulkqc_compute_pca(counts)

  testthat::expect_error(
    bulkqc_validate_pca_axes(out, "PC1", "PC9"),
    "Selected PCA axis is not available"
  )
  testthat::expect_error(
    bulkqc_validate_pca_axes(out, "PC1", "PC1"),
    "Choose different PCA axes"
  )
})

testthat::test_that("bulkqc_pca_plot_data removes duplicate metadata sample_id", {
  counts <- matrix(
    c(1, 2, 3, 4, 5, 7, 7, 8, 9, 11, 10, 12),
    nrow = 4,
    ncol = 3,
    dimnames = list(paste0("g", 1:4), paste0("s", 1:3))
  )
  pca <- bulkqc_compute_pca(counts)
  meta <- data.frame(
    sample_id = paste0("s", 1:3),
    condition = c("A", "A", "B")
  )

  plot_data <- bulkqc_pca_plot_data(pca, meta)

  testthat::expect_equal(plot_data$sample_id, paste0("s", 1:3))
  testthat::expect_true("condition" %in% names(plot_data))
  testthat::expect_equal(anyDuplicated(names(plot_data)), 0L)
})

testthat::test_that("PCA UI includes dynamic axis controls", {
  ui <- mod_pca_ui("pca")
  rendered <- htmltools::renderTags(ui)$html

  testthat::expect_match(rendered, "pca-pca_factor_picker")
  testthat::expect_match(rendered, "pca-pca_axis_picker")
})
