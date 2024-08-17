## Setup
library(shiny)
library(ggplot2)
library(lazyseurat)
source("R/ui.R")

# Best practice options for shiny
options(shiny.sanitize.errors = TRUE)

LazyseuratApp <- function(db="seurat.duckdb") {
  ui <- fluidPage(
    titlePanel("lazyseurat-app template"),
    tabsetPanel(
      tabPanel(
        title = "Dimensionality Reduction",
        DimReductionOutput("dim_red")
      ),
      tabPanel(
        title = "Violin Plot",
        ViolinPlotOutput("vln_plot")
      ),
      tabPanel(
        title = "Dot Plot",
        DotPlotOutput("dot_plot")
      )
    )
  )

  server <- function(input, output, session) {
    DimReductionServer("dim_red", db_file=db)
    ViolinPlotServer("vln_plot", db_file=db)
    DotPlotServer("dot_plot", db_file=db)
  }

  shinyApp(ui = ui, server = server)
}

example_db <- system.file("extdata", "pbmc_small.duckdb", package = "lazyseurat")

LazyseuratApp(example_db)
