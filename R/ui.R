library(shiny)


DimReductionOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        8,
        mainPanel(plotOutput(ns("dim_red")), width = "100%"),
      ),
      column(
        4,
        selectizeInput(
          inputId = ns("dim_red_method"),
          label = "Select dimensionality reduction:",
          choices = NULL, # Choices will be set in the server function
          selected = NULL,
          options = list(placeholder = "Dimensionality reduction")
        ),
        selectizeInput(
          inputId = ns("expr_input"),
          label = "Select count table:",
          choices = NULL, # Choices will be set in the server function
          selected = NULL,
          options = list(placeholder = "Count table")
        ),
        selectizeInput(
          inputId = ns("genes"),
          label = "Color by gene:",
          choices = NULL,
          options = list(maxItems = 1, maxOptions = 5, placeholder = "Gene name")
        ),
        checkboxInput(
          inputId = ns("log_expr"),
          label = "Log1p transform expression",
          value = FALSE
        )
      )
    )
  )
}


DimReductionServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    db_file <- "seurat.duckdb"

    ns <- session$ns

    # Read schema names
    count_names <- get_tables_in_schema(db_file, "layer")
    embedding_names <- get_tables_in_schema(db_file, "embedding")

    # Update selectizeInput choices
    updateSelectizeInput(session, "dim_red_method", choices = embedding_names, selected = embedding_names[1])
    updateSelectizeInput(session, "expr_input", choices = count_names, selected = count_names[1])

    # Observe changes to expr_input and update `genes` input
    observeEvent(input$expr_input, {
      gene_names <- read_gene_names(db_file, input$expr_input)
      updateSelectizeInput(session, "genes", choices = gene_names, server = TRUE)
    })

    updateCheckboxInput(session, "log_expr", value = FALSE)

    # Placeholder for plot output
    output$dim_red <- renderPlot(
      {
        DimPlot(db_file,
          reduction = input$dim_red_method,
          group.by = input$genes,
          group.by.table = input$expr_input,
          log = input$log_expr,
          shuffle = TRUE
        )
      },
      height = 700,
    )
  })
}
