#' Calculate Co-occurrence Matrix for Sequences
#'
#' This function calculates a co-occurrence matrix for elements in sequences.
#'
#' @param sequences A character vector of sequences to analyze.
#' @return A matrix representing the co-occurrence counts of elements.
#' @examples
#' # Example usage:
#' sequences <- c("e1 e2 e3", "e2 e3 e4", "e1 e4", "e1 e2 e4")
#' result <- cooccurrence_matrix(sequences)
#' print(result)
#' @importFrom stringr str_split
#' @export
cooccurrence_matrix <- function(sequences) {
  # Extract unique elements to determine the size of the matrix
  elements <- unique(unlist(strsplit(sequences, " ")))
  elements <- as.integer(sub("e", "", elements))
  num_elements <- max(elements)

  # Initialize the co-occurrence matrix
  cooccurrence_matrix <- matrix(0, nrow = num_elements, ncol = num_elements)

  # Calculate the co-occurrences
  for (seq in sequences) {
    elements <- strsplit(seq, " ")[[1]]
    elements <- as.integer(sub("e", "", elements))

    for (i in 1:length(elements)) {
      for (j in (i + 1):length(elements)) {
        cooccurrence_matrix[elements[i], elements[j]] <- cooccurrence_matrix[elements[i], elements[j]] + 1
        cooccurrence_matrix[elements[j], elements[i]] <- cooccurrence_matrix[elements[j], elements[i]] + 1
      }
    }
  }

  return(cooccurrence_matrix)
}
