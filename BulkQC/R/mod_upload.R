#' upload UI Function for BulkQC
#'
#' @description An internal shiny Module.
#'
#' @param id Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList h3 fileInput
mod_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Upload"),
    fileInput(ns("counts_file"), "Counts (CSV/TSV). Genes x Samples",
              accept = c(".csv", ".tsv", ".txt")),
    fileInput(ns("meta_file"), "Metadata (CSV/TSV). Samples x Covariates",
              accept = c(".csv", ".tsv", ".txt")),
    shiny::checkboxInput(ns("counts_has_gene_id"), "Counts first column is gene_id", TRUE),
    shiny::textInput(ns("meta_sample_id_col"), "Metadata Sample ID Column", value = "Sample_id"),
    shiny::hr(),
    shiny::h4("Preview"),
    DT::DTOutput(ns("counts_preview")),
    DT::DTOutput(ns("meta_preview")),
    shiny::hr(),
    shiny::verbatimTextOutput(ns("status"))

  )
}

#' upload Server Functions
#'
#' @noRd
mod_upload_server <- function(id){
  shiny::moduleServer(id, function(input, output, session){

    read_table_any <- function(path){
      ext <- tolower(tools::file_ext(path))
      if (ext == "csv"){
        readr::read_csv(path, show_col_types = FALSE)
      } else {
        readr::read_tsv(path, show_col_types = FALSE)
      }

    }

  qc_data <- shiny::reactive({
    shiny::req(input$counts_file, input$meta_file)

    counts_df <- read_table_any(input$counts_file$datapath)
    meta_df <- read_table_any(input$meta_file$datapath)

    if (isTRUE(input$counts_has_gene_id)) {
      gene_id <- counts_df[[1]]
      counts_mat <- as.matrix(counts_df[, -1, drop = FALSE])
      rownames(counts_mat) <- as.character(gene_id)
    } else {
      counts_mat <- as.matrix(counts_df)
    }

    # --- Formatting checks for counts table ---
    suppressWarnings(storage.mode(counts_mat) <- "numeric")
    if (anyNA(counts_mat)){
      stop("Counts contains NA after numeric coercion. Check file formatting.")
    }

    if (any(counts_mat < 0)){
      stop("Counts contains negative values (not allowed).")
    }

    sample_ids <- colnames(counts_mat)

    # --- metadata: format checks ---
    sid_col <- input$meta_sample_id_col
    if (!sid_col %in% names(meta_df)) stop(paste0("Metadata missing column: ", sid_col))

    meta_df[[sid_col]] <- as.character(meta_df[[sid_col]])

    # --- Alignment checks: counts df to metadata df ---
    meta_aligned <- meta_df[match(sample_ids, meta_df[[sid_col]]), , drop = FALSE]
    if (anyNA(meta_aligned[[sid_col]])) {
      missing <- sample_ids[is.na(meta_aligned[[sid_col]])]
      stop(paste0("Metadata missing these samples: ", paste(missing, collapse = ", ")))
    }

    list(
      counts = counts_mat,
      meta = meta_aligned,
      sample_id_col = sid_col
    )

  })

  output$counts_preview <- DT::renderDataTable({
    shiny::req(input$counts_file)
    df <- read_table_any(input$counts_file$datapath)
    DT::datatable(utils::head(df, 10), options = list(scrollX=TRUE))
  })

  output$meta_preview <- DT::renderDT({
    shiny::req(input$meta_file)
    df <- read_table_any(input$meta_file$datapath)
    DT::datatable(utils::head(df, 10), options = list(scrollX = TRUE))
  })

  output$status <- shiny::renderPrint({
    if (is.null(input$counts_file) || is.null(input$meta_file)) {
      cat("Waiting for files...\n")
    } else {
      d <- qc_data()
      cat("OK\n")
      cat("Counts dim (genes x samples): ", paste(dim(d$counts), collapse = " x "), "\n", sep = "")
      cat("Metadata rows: ", nrow(d$meta), "\n", sep = "")
    }
  })

  qc_data

  })
}

