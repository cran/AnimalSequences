#' Perform Various Clustering Methods
#'
#' This function performs multiple clustering methods on the input data and returns the results. The methods include K-means, hierarchical clustering, DBSCAN, Gaussian Mixture Model (GMM), spectral clustering, and affinity propagation.
#'
#' @param data A numeric matrix or data frame where rows represent observations and columns represent features.
#' @param n_clusters An integer specifying the number of clusters for methods that require it (e.g., K-means, hierarchical clustering). Default is 3.
#'
#' @return A list with clustering results for each method:
#' \item{kmeans}{A list containing the results of K-means clustering, including cluster assignments.}
#' \item{hierarchical}{A vector of cluster assignments from hierarchical clustering.}
#' \item{dbscan}{A vector of cluster assignments from DBSCAN.}
#' \item{gmm}{A vector of cluster assignments from Gaussian Mixture Model (GMM).}
#' \item{spectral}{A vector of cluster assignments from spectral clustering.}
#' \item{affinity_propagation}{A list of clusters from affinity propagation.}
#'
#' @details
#' - **K-means**: Performs K-means clustering with the specified number of clusters.
#' - **Hierarchical clustering**: Performs hierarchical clustering and cuts the dendrogram to create the specified number of clusters.
#' - **DBSCAN**: Applies DBSCAN clustering with predefined parameters.
#' - **Gaussian Mixture Model (GMM)**: Uses the Mclust package to perform GMM clustering.
#' - **Spectral clustering**: Uses the kernlab package to perform spectral clustering with a kernel matrix.
#' - **Affinity propagation**: Uses the apcluster package to perform affinity propagation clustering.
#'
#' @examples
#' # Generate sample data
#' data <- matrix(rnorm(100), nrow = 10)
#'
#' # Perform clustering
#' clustering_results <- perform_clustering(data, n_clusters = 3)
#'
#' # Access the results
#' clustering_results$kmeans
#' clustering_results$hierarchical
#' clustering_results$dbscan
#' clustering_results$gmm
#' clustering_results$spectral
#' clustering_results$affinity_propagation
#'
#' @importFrom stats kmeans dist hclust cutree
#' @importFrom fpc dbscan
#' @importFrom mclust Mclust mclustBIC
#' @importFrom kernlab specc
#' @importFrom apcluster apcluster negDistMat
#' @export
perform_clustering <- function(data, n_clusters = 3) {
  results <- list()

  # K-means clustering
  results$kmeans <- kmeans(data, centers = n_clusters, nstart = 25)

  # Hierarchical clustering (agglomerative)
  distance_matrix <- dist(data)
  hclust_result <- hclust(distance_matrix, method = "ward.D2")
  results$hierarchical <- cutree(hclust_result, k = n_clusters)

  # DBSCAN
  results$dbscan <- dbscan::dbscan(data, eps = 0.3, minPts = 3)$cluster

  # Gaussian Mixture Model (GMM)
  gmm_result <- Mclust(data, G = n_clusters)
  results$gmm <- gmm_result$classification

  # Spectral clustering
  similarity_matrix <- exp(-dist(data, upper = T)^2 / (2 * 0.1^2))
  spectral_result <- kernlab::specc(data, centers = n_clusters, kernel = "matrix")
  results$spectral <- spectral_result@.Data

  # Affinity propagation
  ap_result <- apcluster(negDistMat(r = 2), data)
  results$affinity_propagation <- ap_result@clusters

  return(results)
}
