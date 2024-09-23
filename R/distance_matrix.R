#' Calculate Distance Matrix from Co-occurrence Matrix
#'
#' This function calculates a distance matrix from a given co-occurrence matrix.
#'
#' @param cooccurrence_matrix A matrix representing the co-occurrence counts of elements.
#' @return A distance matrix computed from the normalized co-occurrence matrix.
#' @examples
#' # Example usage:
#' cooccurrence_matrix <- matrix(c(3, 2, 1, 2, 5, 0, 1, 0, 4), nrow = 3, byrow = TRUE)
#' result <- calculate_distance_matrix(cooccurrence_matrix)
#' print(result)
#' @importFrom stats as.dist
#' @export

calculate_distance_matrix <- function(cooccurrence_matrix) {
  # Normalize the co-occurrence matrix
  normalized_cooccurrence_matrix <- cooccurrence_matrix / sum(cooccurrence_matrix)

  # Calculate the distance matrix
  distance_matrix <- as.dist(1 - normalized_cooccurrence_matrix)

  return(distance_matrix)
}
