## Setup
library(shiny)
library(ggplot2)
library(lazyseurat)

# Best practice options for shiny
options(shiny.sanitize.errors = FALSE)

DimReductionApp <- function() {
  ui <- fluidPage(
    titlePanel("lazyseurat-app template"),
    tabsetPanel(
      tabPanel(
        title = "Dimensionality Reduction",
        DimReductionOutput("dim_red")
      )
    )
  )

  server <- function(input, output, session) {
    DimReductionServer("dim_red")
  }

  shinyApp(ui = ui, server = server)
}

DimReductionApp()
