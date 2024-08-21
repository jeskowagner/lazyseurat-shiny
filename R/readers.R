#' read_gene_expression
#'
#' Reads gene expression data from a specified database file.
#'
#' @param db_file A string representing the path to the database file containing gene expression data.
#' @param gene An optional string representing the gene for which the expression data is to be read. Default is NULL.
#' @param table A string representing the table name in the database from which to read the gene expression data. Default is "counts".
#'
#' @return A data frame containing the gene expression data.
#'
#' @details This function reads gene expression data from the specified table in the database file. If a gene is specified, only the expression data for that gene is read.
#'
#' @examples
#' # Example usage:
#' # db_file <- system.file("extdata", "pbmc_small.duckdb", package = "lazyseurat")
#' # read_gene_expression(db_file)
#' # read_gene_expression(db_file, gene = "MYC")
#' # read_gene_expression(db_file, gene = "MYC", table = "data")
#'
#' @importFrom lazyseurat read_data_with_meta
#' @importFrom shiny req
#' @export
read_gene_expression <- function(db_file, gene = NULL, table = "counts") {
  req(db_file)
  read_data_with_meta(
    db_file = db_file,
    what = "layer",
    name = table,
    col_select = gene
  )
}

#' read_dim_reduction
#'
#' Reads dimensionality reduction data from a specified database file.
#'
#' @param db_file A string representing the path to the database file containing dimensionality reduction data.
#' @param table A string representing the table name in the database from which to read the dimensionality reduction data. Default is "umap".
#'
#' @return A data frame containing the dimensionality reduction data.
#'
#' @details This function reads dimensionality reduction data from the specified table in the database file. The data is read using the `read_data_with_meta` function.
#'
#' @examples
#' # Example usage:
#' # db_file <- system.file("extdata", "pbmc_small.duckdb", package = "lazyseurat")
#' # read_dim_reduction(db_file)
#' # read_dim_reduction(db_file, table = "pca")
#'
#' @importFrom shiny req
#' @importFrom lazyseurat read_data_with_meta
#' @export
read_dim_reduction <- function(db_file, table = "umap") {
  req(db_file, table)
  read_data_with_meta(
    db_file = db_file,
    what = "embedding",
    name = table
  )
}


#' read_schema_names
#'
#' Reads the names of tables within a specified schema from a database file.
#'
#' @param db_file A string representing the path to the database file.
#' @param schema A string representing the schema name to filter tables by. Default is "layer".
#'
#' @return A character vector containing the names of tables within the specified schema.
#'
#' @details This function retrieves the names of tables within a specified schema from the database file. It uses the `get_tables_per_schema_in_db` function to get all tables and then filters them by the specified schema.
#'
#' @examples
#' # Example usage:
#' # db_file <- system.file("extdata", "pbmc_small.duckdb", package = "lazyseurat")
#' # read_schema_names(db_file)
#' # read_schema_names(db_file, schema = "embedding")
#'
#' @importFrom dplyr filter select pull
#' @importFrom lazyseurat get_tables_per_schema_in_db
#' @importFrom shiny req
read_schema_names <- function(db_file, schema = "layer") {
  req(db_file, schema)
  get_tables_per_schema_in_db(db_file) %>%
    filter(table_schema == schema) %>%
    select(table_name) %>%
    pull(table_name)
}

#' get_default_count_layer
#'
#' Retrieves the default count layer table name from a specified schema in a database file.
#'
#' @param db_file A string representing the path to the database file.
#' @param schema A string representing the schema name to filter tables by. Default is "layer".
#'
#' @return A string representing the name of the first table in the specified schema.
#'
#' @details This function retrieves the names of tables within a specified schema from the database file using the `get_tables_per_schema_in_db` function. It then filters the tables by the specified schema and returns the name of the first table.
#'
#' @examples
#' # Example usage:
#' # db_file <- system.file("extdata", "pbmc_small.duckdb", package = "lazyseurat")
#' # get_default_count_layer(db_file)
#' # get_default_count_layer(db_file, schema = "public")
#'
#' @importFrom dplyr filter pull first
#' @importFrom shiny req
#' @importFrom lazyseurat get_tables_per_schema_in_db
#' @export
get_default_count_layer <- function(db_file, schema = "layer") {
  req(db_file)
  get_tables_per_schema_in_db(db_file) %>%
    filter(table_schema == schema) %>%
    pull(table_name) %>%
    first()
}

#' read_gene_names
#'
#' Reads gene names from a specified database file and table, optionally ordered by average expression.
#'
#' @param db_file A string representing the path to the database file.
#' @param table A string representing the table name in the database from which to read the gene names. Default is the result of `get_default_count_layer(db_file)`.
#' @param ordered_by_average A logical value indicating whether to order the gene names by average expression. Default is TRUE.
#'
#' @return A character vector containing the gene names.
#'
#' @details This function reads gene names from the specified table in the database file. If `ordered_by_average` is TRUE, it retrieves the gene names ordered by their average expression from the 'averages' schema. Otherwise, it retrieves the gene names directly from the 'information_schema'.
#'
#' @examples
#' # Example usage:
#' # db_file <- system.file("extdata", "pbmc_small.duckdb", package = "lazyseurat")
#' # read_gene_names(db_file)
#' # read_gene_names(db_file, table = "custom_table")
#' # read_gene_names(db_file, ordered_by_average = FALSE)
#'
#' @importFrom dplyr filter select pull arrange desc tbl collect
#' @importFrom withr local_db_connection
#' @importFrom DBI Id
#' @importFrom shiny req
#' @importFrom lazyseurat get_connection get_tables_per_schema_in_db
#' @export
read_gene_names <- function(db_file, table = get_default_count_layer(db_file), ordered_by_average = TRUE) {
  req(db_file, table)
  con <- withr::local_db_connection(get_connection(db_file))

  if (!ordered_by_average) {
    gene_names <- tbl(con, Id(schema = "information_schema", table = "columns")) %>%
      filter(table_schema == "layer", table_name == table) %>%
      select(column_name) %>%
      pull(column_name)
  } else {
    # lazyseurat stores gene-averages in schema 'averages' with the corresponding table name
    existing_average_tables <- get_tables_per_schema_in_db(db_file) %>%
      filter(table_schema == "averages") %>%
      pull(table_name)
    if (table %in% existing_average_tables) {
      gene_averages <- tbl(con, Id(schema = "averages", table = table)) %>% collect()
      gene_averages <- gene_averages %>% arrange(desc(average))
      gene_names <- gene_averages$gene
    }
  }
  return(gene_names)
}

#' read_metadata_names
#'
#' Reads metadata names from a specified database file, filtering columns based on unique entries and optionally excluding numeric columns.
#'
#' @param db_file A string representing the path to the database file.
#' @param max_unique_entries An integer specifying the maximum number of unique entries a column can have to be included. Default is 10.
#' @param include_numeric A logical value indicating whether to include numeric columns. Default is TRUE.
#'
#' @return A data frame containing the filtered metadata.
#'
#' @details This function reads metadata from the 'metadata' schema in the database file. It filters columns based on the number of unique entries, and optionally excludes numeric columns if `include_numeric` is FALSE.
#'
#' @examples
#' # Example usage:
#' # db_file <- system.file("extdata", "pbmc_small.duckdb", package = "lazyseurat")
#' # read_metadata_names(db_file)
#' # read_metadata_names(db_file, max_unique_entries = 5)
#' # read_metadata_names(db_file, include_numeric = FALSE)
#'
#' @importFrom dplyr select_if n_distinct tbl collect
#' @importFrom withr local_db_connection
#' @importFrom DBI Id
#' @importFrom lazyseurat get_connection
#' @importFrom shiny req
#' @export
read_metadata_names <- function(db_file, max_unique_entries = 10, include_numeric = TRUE) {
  req(db_file)
  con <- withr::local_db_connection(get_connection(db_file))

  metadata <- tbl(con, Id(schema = "metadata", table = "metadata")) %>%
    collect() %>%
    select_if(~ n_distinct(.) <= max_unique_entries)

  if (!include_numeric) {
    metadata <- metadata %>% select_if(~ !is.numeric(.))
  }

  return(names(metadata))
}
