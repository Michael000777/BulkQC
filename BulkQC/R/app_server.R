#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  qc_data <- mod_upload_server("upload")
  mod_qc_overview_server("qc", qc_data = qc_data)
  mod_pca_server("pca", qc_data = qc_data)
}
