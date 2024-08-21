#' runLazySeuratShiny
#'
#' Creates a Shiny app template for visualizing Seurat data with dimensionality reduction, violin plots, and dot plots.
#'
#' @param db A string representing the path to the database file containing the Seurat data. Default is "seurat.duckdb".
#' @param title A string representing the title of the Shiny app. Default is "lazyseurat-app template".
#' @return A Shiny app object.
#'
#' @details This function sets up a Shiny app with the following features:
#' - A title panel with the app title.
#' - A tabset panel with three tabs:
#'   - "Dimensionality Reduction" tab containing the DimReductionOutput UI component.
#'   - "Violin Plot" tab containing the ViolinPlotOutput UI component.
#'   - "Dot Plot" tab containing the DotPlotOutput UI component.
#' - Server logic to handle the corresponding server modules for each tab:
#'   - DimReductionServer for the "Dimensionality Reduction" tab.
#'   - ViolinPlotServer for the "Violin Plot" tab.
#'   - DotPlotServer for the "Dot Plot" tab.
#'
#' @examples
#' # Example usage:
#' # runLazySeuratShiny()
#'
#' @importFrom shiny fluidPage titlePanel tabsetPanel tabPanel shinyApp
#' @export
runLazySeuratShiny <- function(db = system.file("extdata", "pbmc_small.duckdb", package = "lazyseurat"), title = "lazyseurat-app template") {
  ui <- fluidPage(
    titlePanel(title),
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
    DimReductionServer("dim_red", db_file = db)
    ViolinPlotServer("vln_plot", db_file = db)
    DotPlotServer("dot_plot", db_file = db)
  }

  shinyApp(ui = ui, server = server)
}