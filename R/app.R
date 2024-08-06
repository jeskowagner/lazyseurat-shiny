## Setup
library(shiny)
library(ggplot2)
library(lazyseurat)

# Best practice options for shiny
options(shiny.sanitize.errors = FALSE)

LazyseuratApp <- function() {
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
      )
    )
  )

  server <- function(input, output, session) {
    DimReductionServer("dim_red")
    ViolinPlotServer("vln_plot")
  }

  shinyApp(ui = ui, server = server)
}

LazyseuratApp()
