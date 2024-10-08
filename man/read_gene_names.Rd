% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readers.R
\name{read_gene_names}
\alias{read_gene_names}
\title{read_gene_names}
\usage{
read_gene_names(
  db_file,
  table = get_default_count_layer(db_file),
  ordered_by_average = TRUE
)
}
\arguments{
\item{db_file}{A string representing the path to the database file.}

\item{table}{A string representing the table name in the database from which to read the gene names. Default is the result of \code{get_default_count_layer(db_file)}.}

\item{ordered_by_average}{A logical value indicating whether to order the gene names by average expression. Default is TRUE.}
}
\value{
A character vector containing the gene names.
}
\description{
Reads gene names from a specified database file and table, optionally ordered by average expression.
}
\details{
This function reads gene names from the specified table in the database file. If \code{ordered_by_average} is TRUE, it retrieves the gene names ordered by their average expression from the 'averages' schema. Otherwise, it retrieves the gene names directly from the 'information_schema'.
}
\examples{
# Example usage:
# db_file <- system.file("extdata", "pbmc_small.duckdb", package = "lazyseurat")
# read_gene_names(db_file)
# read_gene_names(db_file, table = "custom_table")
# read_gene_names(db_file, ordered_by_average = FALSE)

}
