#' pca UI Function for BulkQC
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_pca_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::h3("Principal Component Analysis"),
    shiny::fluidRow(
      shiny::column(
        4,
        shiny::uiOutput(ns("pca_factor_picker"))
      ),

      shiny::column(8, plotly::plotlyOutput(ns("pca_plot"), height = "540px"))
    )
  )
}

#' pca Server Functions
#'
#' @noRd
mod_pca_server <- function(id, qc_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    #--- PCA Factor for coloring plot
    output$pca_factor_picker <- shiny::renderUI({
      d <- shiny::req(qc_data())
      if (is.null(d) || is.null(d$counts)) return(NULL)

      choices <- colnames(d$meta)
      if (is.null(choices) || length(choices) == 0) return(NULL)

      shiny::selectInput(
        session$ns("pca_factor"),
        "Coloring Factor",
        choices = choices,
        selected = choices[[1]]
      )
    })

    #---PCA Plot ---

    output$pca_plot <- plotly::renderPlotly({
      d <- shiny::req(qc_data())
      counts <- shiny::req(d$counts)
      meta <- shiny::req(d$meta)
      factor_color <- shiny::req(input$pca_factor)

      log_counts <- log2(counts + 1)
      variance <- apply(log_counts, 1, var)
      log_counts <- log_counts[variance > 1e-8, ]

      pca_result <- stats::prcomp(t(log_counts), scale. = TRUE)
      variance_explained <- round(100 * (pca_result$sdev^2 / sum(pca_result$sdev^2)), 2)

      pca_data <- data.frame(PC1 = pca_result$x[, 1], PC2 = pca_result$x[, 2],
                             PC3 = pca_result$x[, 3], PC4 = pca_result$x[, 4],
                             meta)

      pca_p <- ggplot2::ggplot(pca_data, ggplot2::aes(x = PC1, y = PC2, color = .data[[factor_color]], text=sample_id)) +
        ggplot2::geom_point() +
        ggplot2::labs(title = paste("PCA Plot Colored by:", factor_color),
             x = paste0("PC1 (", variance_explained[1], "% variance)"),
             y = paste0("PC2 (", variance_explained[2], "% variance)"))

      plotly::ggplotly(pca_p, tooltip = c("text", "x", "y"))
    })

  })
}

