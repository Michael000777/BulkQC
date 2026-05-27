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
    shiny::actionButton(ns("load_example"), "Load example data"),
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

  shiny::observeEvent(input$load_example, {
    shiny::updateTextInput(session, "meta_sample_id_col", value = "sample_id")
  })

  raw_tables <- shiny::reactive({
    has_uploads <- bulkqc_file_is_selected(input$counts_file) &&
      bulkqc_file_is_selected(input$meta_file)

    if (has_uploads) {
      return(list(
        counts_df = bulkqc_read_table_any(input$counts_file$datapath),
        meta_df = bulkqc_read_table_any(input$meta_file$datapath),
        source = "Uploaded files"
      ))
    }

    if (isTRUE(input$load_example > 0)) {
      return(bulkqc_load_example_data())
    }

    NULL
  })

  qc_data <- shiny::reactive({
    tables <- shiny::req(raw_tables())
    sample_id_col <- bulkqc_resolve_sample_id_col(input$meta_sample_id_col, tables)

    prepared <- bulkqc_prepare_qc_data(
      counts_df = tables$counts_df,
      meta_df = tables$meta_df,
      counts_has_gene_id = isTRUE(input$counts_has_gene_id),
      meta_sample_id_col = sample_id_col
    )
    prepared$source <- tables$source
    prepared

  })

  output$counts_preview <- DT::renderDataTable({
    tables <- shiny::req(raw_tables())
    DT::datatable(utils::head(tables$counts_df, 10), options = list(scrollX=TRUE), rownames = FALSE)
  })

  output$meta_preview <- DT::renderDT({
    tables <- shiny::req(raw_tables())
    DT::datatable(utils::head(tables$meta_df, 10), options = list(scrollX = TRUE), rownames = FALSE)
  })

  output$status <- shiny::renderPrint({
    if (is.null(raw_tables())) {
      cat("Waiting for files or example data...\n")
    } else {
      tryCatch(
        {
          d <- qc_data()
          cat("OK\n")
          cat("Data source: ", d$source, "\n", sep = "")
          cat("Counts dim (genes x samples): ", paste(dim(d$counts), collapse = " x "), "\n", sep = "")
          cat("Metadata rows: ", nrow(d$meta), "\n", sep = "")
          if (length(d$extra_metadata_samples) > 0) {
            cat("Extra metadata rows ignored: ", bulkqc_collapse_ids(d$extra_metadata_samples), "\n", sep = "")
          }
        },
        error = function(e) {
          cat("Input validation failed\n")
          cat(conditionMessage(e), "\n", sep = "")
        }
      )
    }
  })

  qc_data

  })
}

bulkqc_read_table_any <- function(path){
  ext <- tolower(tools::file_ext(path))
  if (ext == "csv"){
    readr::read_csv(path, show_col_types = FALSE, name_repair = "minimal")
  } else {
    readr::read_tsv(path, show_col_types = FALSE, name_repair = "minimal")
  }
}

bulkqc_file_is_selected <- function(file_input) {
  !is.null(file_input) && isTRUE(nzchar(file_input$datapath))
}

bulkqc_example_paths <- function(ext = "csv") {
  ext <- match.arg(ext, c("csv", "tsv"))
  list(
    counts = app_sys("extdata", paste0("BulkQC_sample_counts.", ext)),
    meta = app_sys("extdata", paste0("BulkQC_sample_metadata.", ext))
  )
}

bulkqc_load_example_data <- function(ext = "csv") {
  paths <- bulkqc_example_paths(ext)

  list(
    counts_df = bulkqc_read_table_any(paths$counts),
    meta_df = bulkqc_read_table_any(paths$meta),
    sample_id_col = "sample_id",
    source = "Packaged example data"
  )
}

bulkqc_resolve_sample_id_col <- function(input_col, tables) {
  if (is.null(input_col) || identical(input_col, "") || isTRUE(is.na(input_col))) {
    return(input_col)
  }

  meta_names <- names(tables$meta_df)
  if (is.null(meta_names) || length(meta_names) == 0) {
    return(input_col)
  }

  if (input_col %in% meta_names) {
    return(input_col)
  }

  normalize_name <- function(x) {
    tolower(gsub("[^[:alnum:]]+", "", x))
  }

  matches <- meta_names[normalize_name(meta_names) == normalize_name(input_col)]
  if (length(matches) == 1) {
    return(matches)
  }

  input_col
}

bulkqc_prepare_qc_data <- function(counts_df,
                                  meta_df,
                                  counts_has_gene_id = TRUE,
                                  meta_sample_id_col = "Sample_id") {
  counts_mat <- bulkqc_counts_df_to_matrix(counts_df, counts_has_gene_id)
  sample_ids <- colnames(counts_mat)
  meta_aligned <- bulkqc_align_metadata(meta_df, meta_sample_id_col, sample_ids)

  list(
    counts = counts_mat,
    meta = meta_aligned,
    sample_id_col = meta_sample_id_col,
    extra_metadata_samples = attr(meta_aligned, "bulkqc_extra_metadata_samples", exact = TRUE)
  )
}
