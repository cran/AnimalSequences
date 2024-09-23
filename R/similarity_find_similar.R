#' Find Most Similar Columns in a Distance Matrix
#'
#' This function identifies the most similar columns for each column in a distance matrix. For each column, it finds the columns with the smallest distances (i.e., most similar) based on the given number of similar columns to retrieve.
#'
#' @param distance_matrix A numeric matrix where the distance between columns is represented. The rows and columns should correspond to the same set of entities.
#' @param n_similar An integer specifying the number of most similar columns to find for each column. Default is 3.
#'
#' @return A list of character vectors. Each element of the list corresponds to a column in the distance matrix and contains the column names of the most similar columns.
#'
#' @examples
#' # Create a sample distance matrix
#' distance_matrix <- matrix(c(0, 1, 2, 1, 0, 3, 2, 3, 0),
#'                           nrow = 3,
#'                           dimnames = list(NULL, c("A", "B", "C")))
#'
#' # Find the 2 most similar columns for each column
#' find_most_similar_columns(distance_matrix, n_similar = 2)
#' @export
find_most_similar_columns <- function(distance_matrix, n_similar = 3) {
  most_similar <- list()

  for (i in 1:nrow(distance_matrix)) {
    # Get the distances for the current row
    distances <- distance_matrix[i,]

    # Find the indices of the n_similar smallest distances
    similar_indices <- order(distances, decreasing = FALSE)[1:n_similar]

    # Get the column names corresponding to the similar_indices
    similar_columns <- colnames(distance_matrix)[similar_indices]

    # Store the result in the list
    most_similar[[i]] <- similar_columns
  }

  return(most_similar)
}
