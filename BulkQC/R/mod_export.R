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
        "QC histogram (HTML)" = "hist_html",
        "Raw count distribution (HTML)" = "count_dist_html"
      ),
      selected = c("qc_csv", "pca_html")
    ),

    shiny::downloadButton(ns("download_bundle"), "Download ZIP")
  )
}

#' export Server Functions
#'
#' @noRd
mod_export_server <- function(id, pca_plot, qc_bundle, pca_settings = NULL){
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
          p <- bulkqc_add_export_metadata(
            p,
            title = "PCA plot",
            settings = bulkqc_eval_export_settings(pca_settings)
          )
          out_html <- file.path(tmp_dir, "pca_plot.html")
          htmlwidgets::saveWidget(p, out_html, selfcontained = TRUE)
          files <- c(files, out_html)
        }

        # QC histogram (plotly) -> HTML
        if ("hist_html" %in% input$include) {
          p <- tryCatch(qc_bundle$metric_hist(), error = function(e) NULL)
          shiny::validate(shiny::need(!is.null(p), "Histogram not available yet: choose a metric and bins first."))
          p <- shiny::req(qc_bundle$metric_hist())
          p <- bulkqc_add_export_metadata(
            p,
            title = "QC metric histogram",
            settings = bulkqc_eval_export_settings(qc_bundle$metric_hist_settings)
          )
          out_html <- file.path(tmp_dir, "qc_histogram.html")
          htmlwidgets::saveWidget(p, out_html, selfcontained = TRUE)
          files <- c(files, out_html)
        }

        # Raw count distribution (plotly) -> HTML
        if ("count_dist_html" %in% input$include) {
          p <- tryCatch(qc_bundle$count_dist(), error = function(e) NULL)
          shiny::validate(shiny::need(!is.null(p), "Count distribution not available yet: choose a scope and bins first."))
          p <- shiny::req(qc_bundle$count_dist())
          p <- bulkqc_add_export_metadata(
            p,
            title = "Raw count distribution",
            settings = bulkqc_eval_export_settings(qc_bundle$count_dist_settings)
          )
          out_html <- file.path(tmp_dir, "count_distribution.html")
          htmlwidgets::saveWidget(p, out_html, selfcontained = TRUE)
          files <- c(files, out_html)
        }

        shiny::validate(shiny::need(length(files) > 0, "Nothing selected to export."))


        zip::zipr(zipfile = zip_path, files = files, root = tmp_dir)
      }
    )


  })
}

bulkqc_eval_export_settings <- function(settings) {
  if (is.null(settings)) {
    return(list())
  }

  settings()
}

bulkqc_add_export_metadata <- function(widget, title, settings, exported_at = Sys.time()) {
  htmlwidgets::prependContent(
    widget,
    bulkqc_export_metadata_header(title, settings, exported_at)
  )
}

bulkqc_export_metadata_header <- function(title, settings, exported_at = Sys.time()) {
  if (is.null(settings)) {
    settings <- list()
  }

  rows <- Map(
    function(name, value) {
      htmltools::tags$li(
        htmltools::tags$strong(paste0(name, ": ")),
        paste(value, collapse = ", ")
      )
    },
    names(settings),
    settings
  )

  htmltools::tags$div(
    class = "bulkqc-export-metadata",
    style = paste(
      "font-family: system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;",
      "font-size: 14px;",
      "line-height: 1.4;",
      "margin: 16px 0 20px 0;",
      "padding: 12px 14px;",
      "border: 1px solid #d0d7de;",
      "border-radius: 6px;",
      "background: #f6f8fa;"
    ),
    htmltools::tags$h2(
      style = "font-size: 18px; margin: 0 0 8px 0;",
      title
    ),
    htmltools::tags$p(
      style = "margin: 0 0 8px 0;",
      paste("Exported from BulkQC on", format(exported_at, "%Y-%m-%d %H:%M:%S %Z"))
    ),
    do.call(htmltools::tags$ul, c(list(style = "margin: 0; padding-left: 20px;"), unname(rows)))
  )
}
