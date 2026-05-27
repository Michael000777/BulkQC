#' Compute sample-level RNA-seq QC metrics
#' @noRd
compute_qc_metrics <- function(counts_mat,
                               detected_thresholds = c(1, 10),
                               top_n = c(10, 50, 100)) {
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
  median_count <- apply(counts_mat, 2, stats::median, na.rm = TRUE)
  upper_quartile_count <- apply(
    counts_mat,
    2,
    stats::quantile,
    probs = 0.75,
    na.rm = TRUE,
    names = FALSE
  )

  if (is.null(colnames(counts_mat))) {
    colnames(counts_mat) <- paste0("sample_", seq_len(ncol(counts_mat)))
  }

  detected_by_threshold <- bulkqc_detected_threshold_metrics(counts_mat, detected_thresholds)
  pct_top_n <- bulkqc_pct_counts_in_top_n_metrics(counts_mat, top_n)

  out <- tibble::tibble(
    sample_id = colnames(counts_mat),
    lib_size = lib_size,
    detected_genes = detected_genes,
    pct_zero = pct_zero,
    median_count = median_count,
    upper_quartile_count = upper_quartile_count
  )

  out <- cbind(out, detected_by_threshold, pct_top_n)
  tibble::as_tibble(out)
}

bulkqc_detected_threshold_metrics <- function(counts_mat, detected_thresholds) {
  detected_thresholds <- sort(unique(as.numeric(detected_thresholds)))
  detected_thresholds <- detected_thresholds[!is.na(detected_thresholds)]

  metrics <- lapply(
    detected_thresholds,
    function(threshold) {
      colSums(counts_mat >= threshold, na.rm = TRUE)
    }
  )
  names(metrics) <- paste0("detected_ge_", bulkqc_metric_suffix(detected_thresholds))

  as.data.frame(metrics, check.names = FALSE)
}

bulkqc_pct_counts_in_top_n_metrics <- function(counts_mat, top_n) {
  top_n <- sort(unique(as.integer(top_n)))
  top_n <- top_n[!is.na(top_n) & top_n > 0]

  metrics <- lapply(
    top_n,
    function(n) {
      apply(
        counts_mat,
        2,
        function(sample_counts) {
          bulkqc_pct_counts_in_top_n(sample_counts, n)
        }
      )
    }
  )
  names(metrics) <- paste0("pct_counts_top_", top_n)

  as.data.frame(metrics, check.names = FALSE)
}

bulkqc_pct_counts_in_top_n <- function(sample_counts, n) {
  sample_counts <- as.numeric(sample_counts)
  sample_counts <- sample_counts[!is.na(sample_counts)]
  lib_size <- sum(sample_counts)

  if (length(sample_counts) == 0 || lib_size <= 0) {
    return(0)
  }

  n <- min(n, length(sample_counts))
  top_counts <- sort(sample_counts, decreasing = TRUE)[seq_len(n)]
  sum(top_counts) / lib_size * 100
}

bulkqc_metric_suffix <- function(values) {
  values <- as.character(values)
  gsub("[^A-Za-z0-9]+", "_", values)
}

bulkqc_qc_metric_choices <- function() {
  c(
    "Library Size" = "lib_size",
    "Detected Genes (> 0)" = "detected_genes",
    "% Zero Genes" = "pct_zero",
    "Median Count" = "median_count",
    "Upper-Quartile Count" = "upper_quartile_count",
    "Detected Genes (>= 1)" = "detected_ge_1",
    "Detected Genes (>= 10)" = "detected_ge_10",
    "% Counts in Top 10 Genes" = "pct_counts_top_10",
    "% Counts in Top 50 Genes" = "pct_counts_top_50",
    "% Counts in Top 100 Genes" = "pct_counts_top_100"
  )
}
