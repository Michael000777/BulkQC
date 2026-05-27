bulkqc_validate_counts_df <- function(counts_df, counts_has_gene_id = TRUE) {
  if (!is.data.frame(counts_df)) {
    stop("Counts input must be a data frame.", call. = FALSE)
  }

  if (nrow(counts_df) < 1) {
    stop("Counts table must contain at least one gene row.", call. = FALSE)
  }

  if (isTRUE(counts_has_gene_id)) {
    if (ncol(counts_df) < 3) {
      stop("Counts table must include a gene_id column and at least two sample columns.", call. = FALSE)
    }

    gene_id <- as.character(counts_df[[1]])
    if (any(is.na(gene_id) | trimws(gene_id) == "")) {
      stop("Counts gene_id column contains missing or empty values.", call. = FALSE)
    }

    duplicated_gene_ids <- unique(gene_id[duplicated(gene_id)])
    if (length(duplicated_gene_ids) > 0) {
      stop(
        "Counts gene_id column contains duplicate IDs: ",
        bulkqc_collapse_ids(duplicated_gene_ids),
        call. = FALSE
      )
    }

    sample_ids <- names(counts_df)[-1]
  } else {
    if (ncol(counts_df) < 2) {
      stop("Counts table must contain at least two sample columns.", call. = FALSE)
    }

    sample_ids <- names(counts_df)
  }

  bulkqc_validate_sample_names(sample_ids)
  invisible(TRUE)
}

bulkqc_validate_sample_names <- function(sample_ids) {
  if (is.null(sample_ids) || length(sample_ids) == 0) {
    stop("Counts table must contain sample column names.", call. = FALSE)
  }

  if (any(is.na(sample_ids) | trimws(sample_ids) == "")) {
    stop("Counts table contains missing or empty sample column names.", call. = FALSE)
  }

  duplicate_sample_ids <- unique(sample_ids[duplicated(sample_ids)])
  if (length(duplicate_sample_ids) > 0) {
    stop(
      "Counts table contains duplicate sample column names: ",
      bulkqc_collapse_ids(duplicate_sample_ids),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

bulkqc_counts_df_to_matrix <- function(counts_df, counts_has_gene_id = TRUE) {
  bulkqc_validate_counts_df(counts_df, counts_has_gene_id)

  if (isTRUE(counts_has_gene_id)) {
    gene_id <- counts_df[[1]]
    counts_mat <- as.matrix(counts_df[, -1, drop = FALSE])
    rownames(counts_mat) <- as.character(gene_id)
  } else {
    counts_mat <- as.matrix(counts_df)
  }

  suppressWarnings(storage.mode(counts_mat) <- "numeric")
  if (anyNA(counts_mat)) {
    stop("Counts contains nonnumeric or missing values. Check file formatting.", call. = FALSE)
  }

  if (any(counts_mat < 0)) {
    stop("Counts contains negative values, which are not allowed.", call. = FALSE)
  }

  if (nrow(counts_mat) < 1) {
    stop("Counts matrix must contain at least one gene.", call. = FALSE)
  }

  if (ncol(counts_mat) < 2) {
    stop("Counts matrix must contain at least two samples.", call. = FALSE)
  }

  counts_mat
}

bulkqc_validate_metadata <- function(meta_df, sample_id_col, sample_ids) {
  if (!is.data.frame(meta_df)) {
    stop("Metadata input must be a data frame.", call. = FALSE)
  }

  if (nrow(meta_df) < 1) {
    stop("Metadata table must contain at least one sample row.", call. = FALSE)
  }

  if (is.null(sample_id_col) || identical(sample_id_col, "") || is.na(sample_id_col)) {
    stop("Metadata sample ID column must be specified.", call. = FALSE)
  }

  if (!sample_id_col %in% names(meta_df)) {
    stop("Metadata missing column: ", sample_id_col, call. = FALSE)
  }

  meta_ids <- as.character(meta_df[[sample_id_col]])
  if (any(is.na(meta_ids) | trimws(meta_ids) == "")) {
    stop("Metadata sample ID column contains missing or empty values.", call. = FALSE)
  }

  duplicate_meta_ids <- unique(meta_ids[duplicated(meta_ids)])
  if (length(duplicate_meta_ids) > 0) {
    stop(
      "Metadata sample ID column contains duplicate IDs: ",
      bulkqc_collapse_ids(duplicate_meta_ids),
      call. = FALSE
    )
  }

  missing <- setdiff(sample_ids, meta_ids)
  if (length(missing) > 0) {
    stop(
      "Metadata missing these samples: ",
      bulkqc_collapse_ids(missing),
      call. = FALSE
    )
  }

  extra <- setdiff(meta_ids, sample_ids)

  list(
    meta_ids = meta_ids,
    missing = missing,
    extra = extra
  )
}

bulkqc_align_metadata <- function(meta_df, sample_id_col, sample_ids) {
  validation <- bulkqc_validate_metadata(meta_df, sample_id_col, sample_ids)
  meta_df[[sample_id_col]] <- validation$meta_ids

  aligned <- meta_df[match(sample_ids, meta_df[[sample_id_col]]), , drop = FALSE]
  attr(aligned, "bulkqc_extra_metadata_samples") <- validation$extra
  aligned
}

bulkqc_collapse_ids <- function(ids, max_ids = 10) {
  ids <- as.character(ids)
  shown <- utils::head(ids, max_ids)
  suffix <- if (length(ids) > max_ids) {
    paste0(", and ", length(ids) - max_ids, " more")
  } else {
    ""
  }

  paste0(paste(shown, collapse = ", "), suffix)
}
