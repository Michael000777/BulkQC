#' Compute basic sample-level RNA-seq QC metrics
#' @noRd
compute_qc_metrics <- function(counts_mat) {
  stopifnot(is.matrix(counts_mat))

  lib_size <- colSums(counts_mat)
  detected_genes <- colSums(counts_mat > 0)
  pct_zero <- colMeans(counts_mat == 0) * 100

  tibble::tibble(
    sample_id = colnames(counts_mat),
    lib_size = lib_size,
    detected_genes = detected_genes,
    pct_zero = pct_zero
  )
}
