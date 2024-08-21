# Shiny template for Seurat object visualization
This Shiny template package uses `lazyseurat` to visualize Seurat objects that have been converted to a `lazyseurat`-compatible DuckDB. It offers three main app panels:
1. Dimensionality reduction: PCA/UMAP plots
2. Violin plot: Gene expression violin plots
3. Dot plot: Gene expression dot plots

## Installation
```r
devtools::install_github("jeskowagner/lazyseurat")
devtools::install_github("jeskowagner/lazyseurat-shiny")
lazyseuratShiny::runLazySeuratShiny()
```