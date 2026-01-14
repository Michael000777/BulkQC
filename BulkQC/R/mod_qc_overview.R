#' QC Overview UI
#' @noRd
mod_qc_overview_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("QC Overview"),
    DT::DTOutput(ns("qc_table"))
  )
}

#' QC Overview server
#' @noRd
mod_qc_overview_server <- function(id, qc_data) {
  shiny::moduleServer(id, function(input, output, session) {

    qc_tbl <- shiny::reactive({
      d <- shiny::req(qc_data())
      compute_qc_metrics(d$counts)
    })

    output$qc_table <- DT::renderDT({
      d <- qc_data()
      shiny::validate(shiny::need(!is.null(d), "Upload counts + metadata to view QC."))
      DT::datatable(compute_qc_metrics(d$counts), options = list(pageLength = 15, scrollX = TRUE))
    })
  })
}
