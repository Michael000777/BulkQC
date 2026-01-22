#' QC Overview UI
#' @noRd
mod_qc_overview_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("QC Overview"),
    DT::DTOutput(ns("qc_table")),
    shiny::hr(),
    shiny::h4("QC Metric Distributions"),

    shiny::fluidRow(
      shiny::column(
        4,
        shiny::selectInput(
          ns("metric"),
          "Metric",
          choices = c("Library Size" = "lib_size",
                      "Detected Genes" = "detected_genes",
                      "% Zero Genes" = "pct_zero"
                      ),
          selected = "lib_size"
        )
      ),

      shiny::column(
        4,
        shiny::sliderInput(ns("bins_metric"), "Bins", min = 10, max = 200, value = 40)
      )
    ),

    plotly::plotlyOutput(ns("qc_metric_hist"), height = "540px"),

    shiny::hr(),
    shiny::h4("Distribution of Raw Counts"),
    shiny::fluidRow(
      shiny::column(
        4,
        shiny::radioButtons(
          ns("counts_scope"),
          "Histogram Scope",
          choices = c("All counts(subsampled)" = "all", "One Sample" = "one"),
          selected = "all",
          inline = TRUE
        )
      ),

      shiny::column(
        4,
        shiny::uiOutput(ns("sample_picker_ui"))
      ),

      shiny::column(
        4,
        shiny::sliderInput(ns("bins_counts"), "Bins", min = 10, max = 200, value = 60)
      )
    ),
    shiny::fluidRow(
      shiny::column(
        4,
        shiny::checkboxInput(ns("log1p"), "Use log1p(counts)", value = TRUE)
      ),
      shiny::column(
        4,
        shiny::numericInput(
          ns("max_points"),
          "Max points to plot (subsample)",
          value = 200000,
          min = 1000,
          step = 10000
        )
      )
    ),
    plotly::plotlyOutput(ns("count_hist"), height = "540px")

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

    #--- QC Histogram Logic---

    metric_hist_obj <- shiny::reactive({
      qc <- shiny::req(qc_tbl())

      metric <- shiny::req(input$metric)
      bins <- shiny::req(input$bins_metric)


      plot_df <- data.frame(
        value = qc[[metric]],
        sample_id = qc[["sample_id"]] #from the qc table
      )

      p_hist <- ggplot2::ggplot(plot_df, ggplot2::aes(x= value, text = sample_id)) +
        ggplot2::geom_histogram(bins = bins) +
        ggplot2::labs(
          title = paste("Histogram:", metric),
          x = metric,
          y = "Count"
        ) + ggplot2::theme_minimal()

      plotly::ggplotly(p_hist, tooltip = c("x", "y", "text"))

    })



    output$qc_metric_hist <- plotly::renderPlotly({
      metric_hist_obj()
    })


    # --- UI picker --
    output$sample_picker_ui <- shiny::renderUI({
      d <- qc_data()
      if (is.null(d) || is.null(d$counts)) return(NULL)

      choices <- colnames(d$counts)
      if (is.null(choices) || length(choices) == 0) return(NULL)

      shiny::selectInput(
        session$ns("sample_id"),
        "Sample",
        choices = choices,
        selected = choices[[1]]
      )
    })

    #--- Counts Distribution
    count_dist_obj <- shiny::reactive({
      d <- shiny::req(qc_data())
      scope <- shiny::req(input$counts_scope)
      bins <- shiny::req(input$bins_counts)

      counts <- d$counts

      max_points <- as.integer(shiny::req(input$max_points))

      vals <- if (scope == "one") {
        sid <- shiny::req(input$sample_id)
        shiny::req(sid %in% colnames(counts))
        counts[, sid]
      } else {
        as.vector(counts)
      }

      vals <- vals[!is.na(vals)]

      if (length(vals) > max_points) {
        set.seed(1)
        vals <- sample(vals, max_points)
      }

      if (isTRUE(input$log1p)) {
        vals <- log1p(vals)
        xlab <- "log1p(counts)"
      } else {
        xlab <- "counts"
      }

      plot_df <- data.frame(value = vals)

      title <- if (scope == "one") {
        paste("Counts histogram:", input$sample_id)
      } else {
        paste0("Counts histogram: all counts (subsampled to ", format(max_points, big.mark = ","), ")")
      }

      p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$value)) +
        ggplot2::geom_histogram(bins = bins) +
        ggplot2::labs(
          title = title,
          x = xlab,
          y = "Count"
        ) +
        ggplot2::theme_minimal()

      plotly::ggplotly(p, tooltip = c("x", "y"))
    })

    output$count_hist <- plotly::renderPlotly({
      count_dist_obj()
    })

    return(list(qc_tbl = qc_tbl,
                metric_hist = metric_hist_obj,
                count_dist = count_dist_obj
                )
           )
  })


}
