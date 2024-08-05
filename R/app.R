## Setup
library(shiny)
library(ggplot2)
library(lazyseurat)
source("R/ui.R")

# Best practice options for shiny
options(shiny.sanitize.errors = TRUE)
options(shiny.reactlog = TRUE)

# Path to database
db_file <- "seurat.duckdb"

get_default_tabs <- function(db_file) {
  count_names <- get_tables_in_schema(db_file, "layer")
  embedding_names <- get_tables_in_schema(db_file, "embedding")

  tabs <- list()

  tabs[["Dimensionality Reduction"]] <- list(
    strong("Dimensionality Reduction"),
    fluidRow(
      # Show plot
      column(
        8,
        mainPanel(plotOutput("dim_red"), width = "100%")
      ),

      # Show options
      column(
        4,

        # Selecting dim reduction method
        fluidRow(
          selectizeInput(
            inputId = "dim_red_method",
            label = "Select dimensionality reduction:",
            choices = embedding_names,
            selected = embedding_names[1],
            options = list(placeholder = "Dimensionality reduction")
          )
        ),

        # Selecting count table
        fluidRow(
          selectizeInput(
            inputId = "expr_input",
            label = "Select count table:",
            choices = count_names,
            selected = count_names[1],
            options = list(placeholder = "Count table")
          )
        ),



        # Select whether to log transform expression
        fluidRow(
          checkboxInput(
            inputId = "log_expr",
            label = "Log1p transform expression",
            value = FALSE
          )
        )
      )
    )
  )

  tabs
}

# Get default tab UI
tabs <- get_default_tabs(db_file)

ui <- fluidPage(
  titlePanel("lazyseurat-app template"),

  # Add tabs into page
  tabsetPanel(
    do.call(tabPanel, tabs[["Dimensionality Reduction"]])
  )
)

## Define server logic
server <- function(input, output, session) {
  # Observe changes to expr_input and update `genes` input
  observeEvent(input$expr_input, {
    gene_names <- read_gene_names(db_file, input$expr_input)
    updateSelectizeInput(session, "genes", choices = gene_names, server = TRUE)
  })

  output$dim_red <- renderPlot(
    {
      DimPlot(db_file,
        reduction = input$dim_red_method,
        group.by = input$genes,
        group.by.table = input$expr_input,
        log = input$log_expr
      )
    },
    height = 700,
  )
}

shinyApp(ui = ui, server = server)
