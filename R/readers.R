library(lazyseurat)

META = NULL
FAC_COLS = NULL

initializeOptions <- function(con){
    meta = read_metadata(con)
    assign("META", meta, envir = .GlobalEnv)

    fac_cols = names(meta)[sapply(META, is.factor)]
    assign("FAC_COLS", fac_cols, envir = .GlobalEnv)
}

read_gene_expression <- function(con, gene=NULL, table="counts") {
  read_data_with_meta(con=con, what="layer", name=table, col_select=gene)
}
