#' Compute basic sample-level RNA-seq QC metrics
#' @noRd
compute_qc_metrics <- function(counts_mat) {
  # allowing data.frame/tibble input
  if (is.data.frame(counts_mat)) {
    counts_mat <- as.matrix(counts_mat)
  }

  stopifnot(is.matrix(counts_mat))

  # Coercing in case CSV import gave characters
  storage.mode(counts_mat) <- "numeric"

  lib_size <- colSums(counts_mat, na.rm = TRUE)
  detected_genes <- colSums(counts_mat > 0, na.rm = TRUE)
  pct_zero <- colMeans(counts_mat == 0, na.rm = TRUE) * 100

  if (is.null(colnames(counts_mat))) {
    colnames(counts_mat) <- paste0("sample_", seq_len(ncol(counts_mat)))
  }

  tibble::tibble(
    sample_id = colnames(counts_mat),
    lib_size = lib_size,
    detected_genes = detected_genes,
    pct_zero = pct_zero
  )
}
