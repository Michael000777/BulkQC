testthat::test_that("upload UI includes example data control", {
  ui <- mod_upload_ui("upload")
  rendered <- htmltools::renderTags(ui)$html

  testthat::expect_match(rendered, "Load example data")
  testthat::expect_match(rendered, "upload-load_example")
})

testthat::test_that("upload preview tables suppress row names", {
  rendered <- htmltools::renderTags(
    DT::datatable(
      utils::head(data.frame(gene_id = "g1", s1 = 1), 10),
      options = list(scrollX = TRUE),
      rownames = FALSE
    )
  )$html

  testthat::expect_match(rendered, "gene_id", fixed = TRUE)
  testthat::expect_match(rendered, "s1", fixed = TRUE)
  testthat::expect_no_match(rendered, "<th><\\/th>")
})

testthat::test_that("table reader preserves duplicate headers for validation", {
  csv <- tempfile(fileext = ".csv")
  writeLines(c("gene_id,s1,s1", "g1,1,2", "g2,3,4"), csv)

  df <- bulkqc_read_table_any(csv)

  testthat::expect_equal(names(df), c("gene_id", "s1", "s1"))
})

testthat::test_that("packaged example paths exist", {
  paths <- bulkqc_example_paths("csv")

  testthat::expect_true(file.exists(paths$counts))
  testthat::expect_true(file.exists(paths$meta))
})

testthat::test_that("packaged example data loads as raw tables", {
  example <- bulkqc_load_example_data("csv")

  testthat::expect_equal(example$source, "Packaged example data")
  testthat::expect_equal(example$sample_id_col, "sample_id")
  testthat::expect_s3_class(example$counts_df, "data.frame")
  testthat::expect_s3_class(example$meta_df, "data.frame")
  testthat::expect_gt(nrow(example$counts_df), 0)
  testthat::expect_gt(nrow(example$meta_df), 0)
})

testthat::test_that("example data uses the same preparation path as uploads", {
  example <- bulkqc_load_example_data("csv")
  prepared <- bulkqc_prepare_qc_data(
    counts_df = example$counts_df,
    meta_df = example$meta_df,
    counts_has_gene_id = TRUE,
    meta_sample_id_col = example$sample_id_col
  )

  testthat::expect_true(is.matrix(prepared$counts))
  testthat::expect_equal(ncol(prepared$counts), nrow(prepared$meta))
  testthat::expect_equal(prepared$sample_id_col, "sample_id")
  testthat::expect_equal(colnames(prepared$counts), prepared$meta$sample_id)
})

testthat::test_that("example data resolves its packaged sample id column", {
  example <- bulkqc_load_example_data("csv")

  testthat::expect_equal(
    bulkqc_resolve_sample_id_col("Sample_id", example),
    "sample_id"
  )
  testthat::expect_equal(
    bulkqc_resolve_sample_id_col("custom_id", example),
    "custom_id"
  )
  testthat::expect_equal(
    bulkqc_resolve_sample_id_col("Sample_id", list(source = "Uploaded files")),
    "Sample_id"
  )
})
