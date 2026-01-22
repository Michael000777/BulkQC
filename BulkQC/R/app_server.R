#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  qc_data <- mod_upload_server("upload")
  qc_bundle <- mod_qc_overview_server("qc", qc_data = qc_data)
  pca <- mod_pca_server("pca", qc_data = qc_data)
  mod_export_server("export", pca_plot = pca$pca_plot, qc_bundle = qc_bundle)
}
