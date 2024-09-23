#' Cluster Elements Using Hierarchical Clustering
#'
#' This function performs hierarchical clustering on a distance matrix and optionally plots the dendrogram. It uses the specified method for clustering and can visualize the results.
#'
#' @param distance_matrix A matrix of distances between elements. Should be a symmetric matrix with row and column names representing elements.
#' @param method A character string specifying the method for hierarchical clustering. Options include "complete", "average", "single", etc. Default is "complete".
#' @param plot A logical value indicating whether to plot the dendrogram. Default is TRUE.
#'
#' @return An object of class \code{"hclust"} representing the hierarchical clustering result.
#'
#' @details
#' Hierarchical clustering is performed using the specified method. If \code{plot} is TRUE, the function will generate a dendrogram to visualize the clustering.
#'
#' @importFrom stats hclust
#'
#' @examples
#' # Create a distance matrix
#' distance_matrix <- dist(matrix(rnorm(20), nrow = 5))
#'
#' # Perform hierarchical clustering and plot the dendrogram
#' cluster_elements(distance_matrix, method = "complete", plot = TRUE)
#'
#' @export
cluster_elements <- function(distance_matrix, method = "complete", plot = TRUE) {
  # Perform hierarchical clustering
  clustering <- hclust(distance_matrix, method = method)

  if (plot) {
    # Plot the dendrogram
    plot(clustering, main = "Hierarchical Clustering of Elements", xlab = "Elements", ylab = "Distance")
  }

  return(clustering)
}
