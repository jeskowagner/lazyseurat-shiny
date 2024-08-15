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
            list(
              selectizeInput(
                inputId = ns("color_by_gene"),
                label = "Color by gene:",
                choices = NULL,
                options = list(maxItems = 1, maxOptions = 5, placeholder = "Gene name")
              ),
              selectizeInput(
                inputId = ns("expr_input"),
                label = "Select count table:",
                choices = NULL, # Choices will be set in the server function
                selected = NULL,
                options = list(placeholder = "Count table")
              )
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

    ns <- NS(id)

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
    metadata_names <- read_metadata_names(db_file, max_unique_entries = 50, include_numeric = TRUE)
    updateSelectizeInput(session, "color_by_metadata", choices = metadata_names, server = TRUE)

    # Placeholder for plot output
    output$dim_red <- renderPlot(
      {
        if (input$color_by == "Metadata") {
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



ViolinPlotOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        8,
        fluidRow(
          mainPanel(plotOutput(ns("violin_plot")), width = "100%"),
        )
      ),
      column(
        4,
        selectizeInput(
          inputId = ns("expr_input"),
          label = "Select count table:",
          choices = NULL, # Choices will be set in the server function
          selected = NULL,
          options = list(placeholder = "Count table")
        ),
        selectizeInput(
          inputId = ns("violin_gene"),
          label = "Select gene:",
          choices = NULL, # Choices will be set in the server function
          options = list(maxItems = 1, maxOptions = 10, placeholder = "Gene name")
        ),
        selectizeInput(
          inputId = ns("color_by"),
          label = "Color by metadata:",
          choices = NULL,
          options = list(placeholder = "Metadata column")
        ),
        selectizeInput(
          inputId = ns("split_x"),
          label = "Separate on x-axis:",
          choices = NULL,
          options = list(placeholder = "Metadata column")
        ),
      )
    )
  )
}

ViolinPlotServer <- function(id) {
  ns <- NS(id)
  moduleServer(id, function(input, output, session) {
    db_file <- "seurat.duckdb"

    # Read schema names
    count_names <- get_tables_in_schema(db_file, "layer")

    # Update selectizeInput choices
    updateSelectizeInput(session, "expr_input", choices = count_names, selected = count_names[1], server = TRUE)
    updateSelectizeInput(session, "violin_gene", choices = read_gene_names(db_file), selected = read_gene_names(db_file)[1], server = TRUE)

    metadata_columns <- read_metadata_names(db_file, include_numeric = TRUE, max_unique_entries = 10)
    metadata_columns <- metadata_columns[metadata_columns != "barcode"]
    updateSelectizeInput(session, "color_by", choices = metadata_columns, server = TRUE)
    updateSelectizeInput(session, "split_x", choices = metadata_columns, server = TRUE)

    # Placeholder for plot output
    output$violin_plot <- renderPlot(
      {
        VlnPlot(db_file, gene = input$violin_gene, split.by = input$color_by, x = input$split_x, table = input$expr_input)
      },
      height = 700,
    )
  })
}

DotPlotOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        8,
        fluidRow(
          mainPanel(plotOutput(ns("dot_plot")), width = "100%"),
        )
      ),
      column(
        4,
        selectizeInput(
          inputId = ns("expr_input"),
          label = "Select count table:",
          choices = NULL, # Choices will be set in the server function
          selected = NULL,
          options = list(placeholder = "Count table")
        ),
        shinyWidgets::multiInput(
          inputId = ns("dot_gene"),
          label = "Select gene:",
          choices = c("Gene"), # Choices will be set in the server function
          options=list(limit = 10)
        ),
        selectizeInput(
          inputId = ns("split_y"),
          label = "Separate on y-axis:",
          choices = NULL,
          options = list(placeholder = "Metadata column")
        ),
      )
    )
  )
}

DotPlotServer <- function(id) {
  ns <- NS(id)
  moduleServer(id, function(input, output, session) {
    db_file <- "seurat.duckdb"

    # Read schema names
    count_names <- get_tables_in_schema(db_file, "layer")

    # Update selectizeInput choices
    updateSelectizeInput(session, "expr_input", choices = count_names, selected = count_names[1], server = TRUE)
    gene_name <- gtools::mixedsort(read_gene_names(db_file, ordered_by_average = FALSE))
    shinyWidgets::updateMultiInput(session, "dot_gene", choices = gene_name, selected = gene_name[1:3])

    metadata_columns <- read_metadata_names(db_file, include_numeric = FALSE, max_unique_entries = 20)
    metadata_columns <- metadata_columns[metadata_columns != "barcode"]
    updateSelectizeInput(session, "color_by", choices = metadata_columns, server = TRUE)
    updateSelectizeInput(session, "split_y", choices = metadata_columns, server = TRUE, selected = metadata_columns[1])

    # Placeholder for plot output
    output$dot_plot <- renderPlot(
      {
        DotPlot(db_file, gene = input$dot_gene, split.by = input$split_y, table = input$expr_input)
      },
      height = 700,
    )
  })
}
