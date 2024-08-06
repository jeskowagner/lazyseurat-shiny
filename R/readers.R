library(lazyseurat)
library(tibble)
library(dplyr)

read_gene_expression <- function(db_file, gene = NULL, table = "counts") {
  read_data_with_meta(
    db_file = db_file,
    what = "layer",
    name = table,
    col_select = gene
  )
}

read_dim_reduction <- function(db_file, table = "umap") {
  req(db_file, table)
  read_data_with_meta(
    db_file = db_file,
    what = "embedding",
    name = table
  )
}

read_schema_names <- function(db_file, schema = "layer") {
  req(db_file, schema)
  get_tables_per_schema_in_db(db_file) %>%
    filter(table_schema == schema) %>%
    select(table_name) %>%
    pull(table_name)
}

get_default_count_layer <- function(db_file, schema = "layer") {
  req(db_file)
  get_tables_per_schema_in_db(db_file) %>%
    filter(table_schema == schema) %>%
    pull(table_name) %>%
    first()
}

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

read_metadata_names <- function(db_file, max_unique_entries=50, include_numeric=TRUE) {
  req(db_file)
  con <- withr::local_db_connection(get_connection(db_file))

  metadata <- tbl(con, Id(schema = "metadata", table = "metadata")) %>%
    collect() %>%
    select_if(~n_distinct(.) <= max_unique_entries)

  if(!include_numeric) {
    metadata <- metadata %>% select_if(~!is.numeric(.))
  }

  return(names(metadata))
}
