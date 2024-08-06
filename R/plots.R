library(dplyr)
library(ggplot2)
library(shiny)

base.plot.theme <- function() {
  theme_classic() +
    theme(
      plot.title = element_text(size = 25, hjust = 0.5),
      legend.position = "right",
      legend.title = element_text(size = 20),
      legend.key.size = unit(1.5, "line"),
      legend.text = element_text(size = 20),
      axis.line = element_line(
        colour = "black",
        linewidth = 1.25,
        linetype = "solid"
      ),
      axis.text.x = element_text(size = 20, colour = "black"),
      axis.text.y = element_text(size = 20, colour = "black"),
      axis.title.x = element_text(size = 20),
      axis.title.y = element_text(size = 20),
      axis.ticks.length = unit(0.2, "cm")
    )
}

plot.theme_vln <- function() {
  base.plot.theme() +
    theme(
      axis.text.x = element_text(
        size = 20,
        colour = "black",
        angle = 45,
        hjust = 1,
        vjust = 1
      ),
      legend.title = element_blank()
    )
}

#' VlnPlot
#'
#' Generates a violin plot for gene expression data.
#'
#' @param db_file A string representing the path to the database file containing gene expression data.
#' @param gene A string representing the gene for which the expression data is to be plotted.
#' @param x (Optional) A string representing the variable to be used for the x-axis. Default is NULL.
#' @param split.by (Optional) A string representing the variable to be used for splitting the fill color. Default is NULL.
#' @param table A string representing the table to use for gene expression data. Default is "counts".
#' @return A ggplot2 object representing the violin plot of gene expression.
#'
#' @examples
#' # Example usage:
#' # db_file <- "path/to/database/file"
#' # gene <- "GeneName"
#' # VlnPlot(db_file, gene)
#' # VlnPlot(db_file, gene, x = "CellType")
#' # VlnPlot(db_file, gene, x = "CellType", split.by = "Condition")
#'
#' @import shiny
#' @import ggplot2
#' @export
VlnPlot <- function(db_file, gene, x = NULL, split.by = NULL, table="counts") {
  req(db_file, gene)
  # if(!is.null(x) && !is.null(split.by) && x == split.by) {
  #   x <- NULL
  # }
  df <- read_gene_expression(db_file, gene = gene, table = table)

  if (!is.numeric(df[[gene]])) {
    df[[gene]] <- as.numeric(df[[gene]])
  }

  aesthetics <- aes(y = .data[[gene]], x = factor(1))
  if (!is.null(x)) {
    aesthetics <- modifyList(aesthetics, aes(x = .data[[x]]))
  }
  if (!is.null(split.by)) {
    aesthetics <- modifyList(aesthetics, aes(fill = .data[[split.by]]))
  }

  p <- ggplot(df, aesthetics) +
    geom_violin(scale = "width") +
    plot.theme_vln() +
    labs(title = gene, y = "Expression Level", x = x)
  return(p)
}

#' DimPlot
#'
#' Generates a dimensionality reduction plot (e.g., UMAP, PCA) from a database db_filenection.
#'
#' @param db_file A database db_filenection object.
#' @param reduction A string specifying the type of dimensionality reduction to use (default is "umap").
#' @param group.by An optional string specifying the column to group points by.
#' @param shape.by An optional string specifying the column to shape points by.
#' @param shuffle A logical value indicating whether to shuffle the points (default is TRUE).
#' @param raster A logical value indicating whether to use rasterization for large datasets (default is NULL).
#' @param raster.dpi A numeric vector specifying the resolution for rasterization (default is c(512, 512)).
#' @param group.by.table A string specifying the table to use for grouping (default is "counts").
#' @param log A logical value indicating whether to log-transform the data (default is TRUE).
#' @param ... Additional arguments passed to the plotting functions.
#'
#' @return A ggplot object representing the dimensionality reduction plot.
#'
#' @details This function reads the dimensionality reduction data from the specified table in the database,
#'          db_filestructs a ggplot object with the specified aesthetics, and optionally applies rasterization
#'          for large datasets to improve performance.
#'
#'
#' @examples
#' # Example usage:
#  # db_file <- "path/to/database/file"
#' # Generate a UMAP plot grouped by "cell_type":
#' # DimPlot(db_file = db_file, reduction = "umap", group.by = "cell_type")
#'
#' # Generate a PCA plot with points shaped by "condition" and grouped by "cell_type":
#' # DimPlot(db_file = db_file, reduction = "pca", group.by = "cell_type", shape.by = "condition")
#'
#' # Generate a UMAP plot with rasterization for large datasets:
#' # DimPlot(db_file = db_file, reduction = "umap", raster = TRUE, raster.dpi = c(300, 300))
#'
DimPlot <- function(db_file,
                    reduction = "umap",
                    group.by = NULL,
                    shape.by = NULL,
                    shuffle = FALSE,
                    raster = NULL,
                    raster.dpi = c(512, 512),
                    group.by.table = "counts",
                    log = FALSE,
                    ...) {
  req(db_file, reduction, group.by.table)
  df <- read_dim_reduction(db_file, table = reduction)
  if (isTRUE(raster) || nrow(df) > 100000) {
    require(scattermore)
    raster <- TRUE
  }

  # Colnames for PCA are capitalized, not for UMAP
  reduction_title <- reduction
  if (reduction == "pca") {
    reduction <- "PC"
    reduction_title <- "PCA"
  }

  x <- paste0(reduction, "_1")
  y <- paste0(reduction, "_2")

  xlab <- paste0(toupper(reduction), "1")
  ylab <- paste0(toupper(reduction), "1")

  aesthetics <- aes(x = .data[[x]], y = .data[[y]])

  if (!is.null(group.by) && group.by != "") {
    aesthetics <- modifyList(aesthetics, aes(color = .data[[group.by]]))
    if (!group.by %in% colnames(df)) {
      df2 <- read_gene_expression(db_file, gene = group.by, table = group.by.table)
      if (log) {
        df2[[group.by]] <- log1p(df2[[group.by]])
      }
      df <- merge(df, df2)
    }
  }

  if (!is.null(shape.by)) aesthetics <- modifyList(aesthetics, aes(shape = .data[[shape.by]]))

  if (isTRUE(shuffle)) {
    set.seed(2024)
    df <- df[sample(nrow(df)), ]
  } else if (!is.null(group.by) && group.by != "") {
    df <- df[order(df[[group.by]]), ]
  }

  geom <- if (isTRUE(raster)) geom_scattermore(raster.dpi = raster.dpi, size = 3, ...) else geom_point(size = 3, ...)

  p <- ggplot(df, aesthetics) +
    geom +
    base.plot.theme() +
    labs(title = toupper(reduction_title), x = xlab, y = ylab) +
    theme(aspect.ratio = 1)

  if (!is.null(group.by) && group.by != "" && is.numeric(df[[group.by]])) {
    p <- p + scale_color_viridis_c()
  }

  return(p)
}
