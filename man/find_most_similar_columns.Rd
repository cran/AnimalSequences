% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/similarity_find_similar.R
\name{find_most_similar_columns}
\alias{find_most_similar_columns}
\title{Find Most Similar Columns in a Distance Matrix}
\usage{
find_most_similar_columns(distance_matrix, n_similar = 3)
}
\arguments{
\item{distance_matrix}{A numeric matrix where the distance between columns is represented. The rows and columns should correspond to the same set of entities.}

\item{n_similar}{An integer specifying the number of most similar columns to find for each column. Default is 3.}
}
\value{
A list of character vectors. Each element of the list corresponds to a column in the distance matrix and contains the column names of the most similar columns.
}
\description{
This function identifies the most similar columns for each column in a distance matrix. For each column, it finds the columns with the smallest distances (i.e., most similar) based on the given number of similar columns to retrieve.
}
\examples{
# Create a sample distance matrix
distance_matrix <- matrix(c(0, 1, 2, 1, 0, 3, 2, 3, 0),
                          nrow = 3,
                          dimnames = list(NULL, c("A", "B", "C")))

# Find the 2 most similar columns for each column
find_most_similar_columns(distance_matrix, n_similar = 2)
}
