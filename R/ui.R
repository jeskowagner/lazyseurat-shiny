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
          inputId = ns("color_by"),
          label = "Color by:",
          choices = c("Nothing", "Metadata", "Gene expression"),
          options = list(placeholder = "Color by"),
          selected = "Nothing"
        ),
        conditionalPanel(
          condition = "input.color_by == 'Metadata'",
          {
            selectizeInput(
              inputId = ns("color_by_metadata"),
              label = "Color by metadata:",
              choices = NULL,
              options = list(placeholder = "Metadata column")
            )
          },
          ns = ns # Required for correct namespacing
        ),
        conditionalPanel(
          condition = "input.color_by == 'Gene expression'",
          {
            selectizeInput(
              inputId = ns("color_by_gene"),
              label = "Color by gene:",
              choices = NULL,
              options = list(maxItems = 1, maxOptions = 5, placeholder = "Gene name")
            )
          },
          ns = ns # Required for correct namespacing
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

    # Observe changes to expr_input and update `color_by_gene` input
    observeEvent(input$expr_input, {
      gene_names <- read_gene_names(db_file, input$expr_input)
      updateSelectizeInput(session, "color_by_gene", choices = gene_names, server = TRUE)
    })

    # Observe changes to color_by input and update `color_by_metadata` input
    metadata_names <- read_metadata_names(db_file)
    updateSelectizeInput(session, "color_by_metadata", choices = metadata_names, server = TRUE)

    # Placeholder for plot output
    output$dim_red <- renderPlot(
      {
        if(input$color_by == "Metadata") {
          color_by_column <- input$color_by_metadata
        } else if (input$color_by == "Gene expression") {
          color_by_column <- input$color_by_gene
        } else {
            color_by_column <- NULL
        }

        DimPlot(db_file,
          reduction = input$dim_red_method,
          group.by = color_by_column,
          group.by.table = input$expr_input,
          shuffle = FALSE
        )
      },
      height = 700,
    )
  })
}
