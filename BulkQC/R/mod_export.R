#' export UI Function
#'
#' @description A shiny Module for the export logic of graphs and tables of interest.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_export_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::h3("Export Files"),
    shiny::p("Download QC results as a ZIP (QC table & plots)."),

    shiny::checkboxGroupInput(
      ns("include"),
      "Include",
      choices = c(
        "QC table (CSV)" = "qc_csv",
        "PCA plot (HTML)" = "pca_html",
        "QC histogram (HTML)" = "hist_html"
      ),
      selected = c("qc_csv", "pca_html")
    ),

    shiny::downloadButton(ns("download_bundle"), "Download ZIP")
  )
}

#' export Server Functions
#'
#' @noRd
mod_export_server <- function(id, pca_plot, qc_bundle){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$download_bundle <- shiny::downloadHandler(
      filename = function() {
        paste0("BulkQC_export_", format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".zip")
      },
      content = function(zip_path) {
        shiny::req(input$include)

        tmp_dir <- tempfile("bulkqc_export_")
        dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

        files <- character(0)

        # QC table
        if ("qc_csv" %in% input$include) {
          tbl <- shiny::req(qc_bundle$qc_tbl())
          qc_csv <- file.path(tmp_dir, "qc_table.csv")
          utils::write.csv(tbl, qc_csv, row.names = FALSE)
          files <- c(files, qc_csv)
        }

        # PCA plot (plotly) -> HTML
        if ("pca_html" %in% input$include) {
          shiny::validate(shiny::need(!is.null(pca_plot), "PCA export not wired in server."))
          p <- shiny::req(pca_plot())
          out_html <- file.path(tmp_dir, "pca_plot.html")
          htmlwidgets::saveWidget(p, out_html, selfcontained = TRUE)
          files <- c(files, out_html)
        }

        # QC histogram (plotly) -> HTML
        if ("hist_html" %in% input$include) {
          p <- tryCatch(qc_bundle$metric_hist(), error = function(e) NULL)
          shiny::validate(shiny::need(!is.null(p), "Histogram not available yet: choose a metric and bins first."))
          p <- shiny::req(qc_bundle$metric_hist())
          out_html <- file.path(tmp_dir, "qc_histogram.html")
          htmlwidgets::saveWidget(p, out_html, selfcontained = TRUE)
          files <- c(files, out_html)
        }

        shiny::validate(shiny::need(length(files) > 0, "Nothing selected to export."))


        zip::zipr(zipfile = zip_path, files = files, root = tmp_dir)
      }
    )


  })
}

