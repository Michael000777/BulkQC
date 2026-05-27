bulkqc_compute_pca <- function(counts_mat, variance_threshold = 1e-8) {
  if (is.data.frame(counts_mat)) {
    counts_mat <- as.matrix(counts_mat)
  }

  if (!is.matrix(counts_mat)) {
    stop("Counts must be a matrix or data frame.", call. = FALSE)
  }

  if (ncol(counts_mat) < 2) {
    stop("PCA requires at least two samples.", call. = FALSE)
  }

  if (nrow(counts_mat) < 2) {
    stop("PCA requires at least two genes.", call. = FALSE)
  }

  suppressWarnings(storage.mode(counts_mat) <- "numeric")
  if (anyNA(counts_mat)) {
    stop("PCA requires numeric counts with no missing values.", call. = FALSE)
  }

  log_counts <- log2(counts_mat + 1)
  gene_variance <- apply(log_counts, 1, stats::var)
  log_counts <- log_counts[gene_variance > variance_threshold, , drop = FALSE]

  if (nrow(log_counts) < 2) {
    stop("PCA requires at least two variable genes after filtering.", call. = FALSE)
  }

  pca_result <- stats::prcomp(t(log_counts), scale. = TRUE)
  variance_explained <- round(100 * (pca_result$sdev^2 / sum(pca_result$sdev^2)), 2)
  names(variance_explained) <- paste0("PC", seq_along(variance_explained))

  scores <- as.data.frame(pca_result$x)
  scores$sample_id <- rownames(scores)

  list(
    scores = scores,
    variance_explained = variance_explained,
    variable_gene_count = nrow(log_counts),
    pca = pca_result
  )
}

bulkqc_available_pcs <- function(pca_result) {
  pcs <- names(pca_result$scores)
  pcs[grepl("^PC[0-9]+$", pcs)]
}

bulkqc_validate_pca_axes <- function(pca_result, x_pc, y_pc) {
  pcs <- bulkqc_available_pcs(pca_result)
  missing <- setdiff(c(x_pc, y_pc), pcs)

  if (length(missing) > 0) {
    stop(
      "Selected PCA axis is not available: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  if (identical(x_pc, y_pc)) {
    stop("Choose different PCA axes for X and Y.", call. = FALSE)
  }

  invisible(TRUE)
}
