#' DimReductionOutput
#'
#' Creates a UI component for displaying dimensionality reduction plots with options to color by metadata or gene expression.
#'
#' @param id A string representing the namespace ID for the UI component.
#'
#' @return A UI component (tagList) containing the layout for the dimensionality reduction plot and associated input controls.
#'
#' @details This function generates a UI layout for displaying a dimensionality reduction plot. It includes:
#' - A plot output for the dimensionality reduction plot.
#' - A selectize input for choosing the dimensionality reduction method.
#' - A selectize input for choosing the coloring method (Nothing, Metadata, or Gene expression).
#' - Conditional panels for additional inputs based on the selected coloring method:
#'   - If "Metadata" is selected, a selectize input for choosing the metadata column.
#'   - If "Gene expression" is selected, selectize inputs for choosing the gene and the count table.
#'
#' @examples
#' # Example usage in a Shiny app:
#' # ui <- fluidPage(
#' #   DimReductionOutput("dim_red_ui")
#' # )
#' #
#' # server <- function(input, output, session) {
#' #   # Server logic to populate choices and render plot
#' # }
#' #
#' # shinyApp(ui, server)
#'
#' @importFrom shiny NS tagList fluidRow column mainPanel plotOutput selectizeInput conditionalPanel
#' @export
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

#' DimReductionServer
#'
#' Server logic for handling dimensionality reduction plots in a Shiny app.
#'
#' @param id A string representing the namespace ID for the server module.
#' @param db_file A string representing the path to the database file containing the data. Default is "seurat.duckdb".
#'
#' @return A Shiny module server function.
#'
#' @details This function sets up the server-side logic for a dimensionality reduction plot module in a Shiny app. It performs the following tasks:
#' - Reads schema names for count and embedding tables from the database.
#' - Updates the choices for the dimensionality reduction method and expression input selectize inputs.
#' - Observes changes to the expression input and updates the gene names for coloring by gene expression.
#' - Observes changes to the coloring method and updates the metadata names for coloring by metadata.
#' - Renders the dimensionality reduction plot based on the selected inputs.
#'
#' @examples
#' # Example usage in a Shiny app:
#' # ui <- fluidPage(
#' #   DimReductionOutput("dim_red_ui")
#' # )
#' #
#' # server <- function(input, output, session) {
#' #   DimReductionServer("dim_red_ui", db_file = "path/to/database/file")
#' # }
#' #
#' # shinyApp(ui, server)
#'
#' @importFrom shiny moduleServer updateSelectizeInput observeEvent renderPlot
#' @importFrom lazyseurat get_tables_in_schema
#'
#' @export
DimReductionServer <- function(id, db_file = "seurat.duckdb") {
  moduleServer(id, function(input, output, session) {

    # Read schema names
    count_names <- lazyseurat::get_tables_in_schema(db_file, "layer")
    embedding_names <- lazyseurat::get_tables_in_schema(db_file, "embedding")

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


#' ViolinPlotOutput
#'
#' Creates a UI component for displaying violin plots with options to select count table, gene, and metadata for coloring and splitting.
#'
#' @param id A string representing the namespace ID for the UI component.
#'
#' @return A UI component (tagList) containing the layout for the violin plot and associated input controls.
#'
#' @details This function generates a UI layout for displaying a violin plot. It includes:
#' - A plot output for the violin plot.
#' - A selectize input for choosing the count table.
#' - A selectize input for choosing the gene.
#' - A selectize input for choosing the metadata column for coloring.
#' - A selectize input for choosing the metadata column for separating on the x-axis.
#'
#' @examples
#' # Example usage in a Shiny app:
#' # ui <- fluidPage(
#' #   ViolinPlotOutput("violin_plot_ui")
#' # )
#' #
#' # server <- function(input, output, session) {
#' #   # Server logic to populate choices and render plot
#' # }
#' #
#' # shinyApp(ui, server)
#'
#' @importFrom shiny NS tagList fluidRow column mainPanel plotOutput selectizeInput
#' @export
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

#' ViolinPlotServer
#'
#' Server logic for handling violin plots in a Shiny app.
#'
#' @param id A string representing the namespace ID for the server module.
#' @param db_file A string representing the path to the database file containing the data. Default is "seurat.duckdb".
#'
#' @return A Shiny module server function.
#'
#' @details This function sets up the server-side logic for a violin plot module in a Shiny app. It performs the following tasks:
#' - Reads schema names for count tables from the database.
#' - Updates the choices for the count table and gene selectize inputs.
#' - Reads metadata names and updates the choices for coloring and splitting the violin plot.
#' - Renders the violin plot based on the selected inputs.
#'
#' @examples
#' # Example usage in a Shiny app:
#' # ui <- fluidPage(
#' #   ViolinPlotOutput("violin_plot_ui")
#' # )
#' #
#' # server <- function(input, output, session) {
#' #   ViolinPlotServer("violin_plot_ui", db_file = "path/to/database/file")
#' # }
#' #
#' # shinyApp(ui, server)
#'
#' @importFrom shiny moduleServer updateSelectizeInput observeEvent renderPlot
#' @importFrom lazyseurat get_tables_in_schema
#' @export
ViolinPlotServer <- function(id, db_file = "seurat.duckdb") {
  moduleServer(id, function(input, output, session) {
    # Read schema names
    count_names <- lazyseurat::get_tables_in_schema(db_file, "layer")

    # Update selectizeInput choices
    updateSelectizeInput(session, "expr_input", choices = count_names, selected = count_names[1], server = TRUE)
    updateSelectizeInput(session, "violin_gene", choices = read_gene_names(db_file), selected = read_gene_names(db_file)[1], server = TRUE)

    metadata_columns <- read_metadata_names(db_file, include_numeric = TRUE, max_unique_entries = 20)
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

#' DotPlotOutput
#'
#' Creates a UI component for displaying dot plots with options to select count table, gene, and metadata for separating on the y-axis.
#'
#' @param id A string representing the namespace ID for the UI component.
#'
#' @return A UI component (tagList) containing the layout for the dot plot and associated input controls.
#'
#' @details This function generates a UI layout for displaying a dot plot. It includes:
#' - A plot output for the dot plot.
#' - A selectize input for choosing the count table.
#' - A multi-input for choosing the gene.
#' - A selectize input for choosing the metadata column for separating on the y-axis.
#'
#' @examples
#' # Example usage in a Shiny app:
#' # ui <- fluidPage(
#' #   DotPlotOutput("dot_plot_ui")
#' # )
#' #
#' # server <- function(input, output, session) {
#' #   # Server logic to populate choices and render plot
#' # }
#' #
#' # shinyApp(ui, server)
#'
#' @importFrom shiny NS tagList fluidRow column mainPanel plotOutput selectizeInput
#' @importFrom shinyWidgets multiInput
#' @export
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
          options = list(limit = 10)
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

#' DotPlotServer
#'
#' Server logic for handling dot plots in a Shiny app.
#'
#' @param id A string representing the namespace ID for the server module.
#' @param db_file A string representing the path to the database file containing the data. Default is "seurat.duckdb".
#'
#' @return A Shiny module server function.
#'
#' @details This function sets up the server-side logic for a dot plot module in a Shiny app. It performs the following tasks:
#' - Reads schema names for count tables from the database.
#' - Updates the choices for the count table and gene multi-input.
#' - Reads metadata names and updates the choices for separating the dot plot on the y-axis.
#' - Renders the dot plot based on the selected inputs.
#'
#' @examples
#' # Example usage in a Shiny app:
#' # ui <- fluidPage(
#' #   DotPlotOutput("dot_plot_ui")
#' # )
#' #
#' # server <- function(input, output, session) {
#' #   DotPlotServer("dot_plot_ui", db_file = "path/to/database/file")
#' # }
#' #
#' # shinyApp(ui, server)
#'
#' @importFrom shiny moduleServer updateSelectizeInput observeEvent renderPlot
#' @importFrom shinyWidgets updateMultiInput
#' @importFrom lazyseurat get_tables_in_schema
#' @importFrom gtools mixedsort
#' @export
DotPlotServer <- function(id, db_file = "seurat.duckdb") {
  moduleServer(id, function(input, output, session) {
    # Read schema names
    count_names <- lazyseurat::get_tables_in_schema(db_file, "layer")

    # Update selectizeInput choices
    selected_table <- ifelse("data" %in% count_names, "data", count_names[1])
    updateSelectizeInput(session, "expr_input", choices = count_names, selected = selected_table, server = TRUE)
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
