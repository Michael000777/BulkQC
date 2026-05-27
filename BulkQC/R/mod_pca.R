#' pca UI Function for BulkQC
#'
#' @noRd
#' @importFrom rlang .data
#' @importFrom shiny NS tagList
mod_pca_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::h3("Principal Component Analysis"),
    shiny::fluidRow(
      shiny::column(
        4,
        shiny::uiOutput(ns("pca_factor_picker")),
        shiny::uiOutput(ns("pca_axis_picker"))
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

    pca_result_obj <- shiny::reactive({
      d <- shiny::req(qc_data())
      counts <- shiny::req(d$counts)

      tryCatch(
        bulkqc_compute_pca(counts),
        error = function(e) {
          shiny::validate(shiny::need(FALSE, conditionMessage(e)))
        }
      )
    })

    output$pca_axis_picker <- shiny::renderUI({
      pca_result <- shiny::req(pca_result_obj())
      pcs <- bulkqc_available_pcs(pca_result)
      if (length(pcs) < 2) return(NULL)

      shiny::tagList(
        shiny::selectInput(
          ns("x_pc"),
          "X axis",
          choices = pcs,
          selected = pcs[[1]]
        ),
        shiny::selectInput(
          ns("y_pc"),
          "Y axis",
          choices = pcs,
          selected = pcs[[2]]
        )
      )
    })

    pca_plot_obj <- shiny::reactive({
      d <- shiny::req(qc_data())
      meta <- shiny::req(d$meta)
      factor_color <- shiny::req(input$pca_factor)
      pca_result <- shiny::req(pca_result_obj())
      pcs <- bulkqc_available_pcs(pca_result)
      x_pc <- if (is.null(input$x_pc)) pcs[[1]] else input$x_pc
      y_pc <- if (is.null(input$y_pc)) pcs[[2]] else input$y_pc

      tryCatch(
        bulkqc_validate_pca_axes(pca_result, x_pc, y_pc),
        error = function(e) {
          shiny::validate(shiny::need(FALSE, conditionMessage(e)))
        }
      )

      pca_data <- bulkqc_pca_plot_data(pca_result, meta)
      variance_explained <- pca_result$variance_explained

      pca_p <- ggplot2::ggplot(pca_data, ggplot2::aes(x = .data[[x_pc]], y = .data[[y_pc]], color = .data[[factor_color]], text = .data$sample_id)) +
        ggplot2::geom_point() +
        ggplot2::labs(title = paste("PCA Plot Colored by:", factor_color),
                      x = paste0(x_pc, " (", variance_explained[[x_pc]], "% variance)"),
                      y = paste0(y_pc, " (", variance_explained[[y_pc]], "% variance)")) +
        ggplot2::theme_minimal()

      plotly::ggplotly(pca_p, tooltip = c("text", "x", "y"))

    })

    output$pca_plot <- plotly::renderPlotly({
      pca_plot_obj()
    })

    pca_settings <- shiny::reactive({
      pca_result <- shiny::req(pca_result_obj())
      pcs <- bulkqc_available_pcs(pca_result)

      list(
        "Coloring factor" = shiny::req(input$pca_factor),
        "X axis" = if (is.null(input$x_pc)) pcs[[1]] else input$x_pc,
        "Y axis" = if (is.null(input$y_pc)) pcs[[2]] else input$y_pc
      )
    })

    return(list(
      pca_plot = pca_plot_obj,
      pca_settings = pca_settings
    ))
  })
}
