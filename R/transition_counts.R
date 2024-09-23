#' Calculate Transition Counts from Sequences
#'
#' This function calculates the transition counts between elements in a set of sequences. It creates a matrix where each element represents the number of times a transition occurs from one element to another.
#'
#' @param sequences A vector of character strings, where each string represents a sequence of elements separated by spaces. Elements should be labeled with prefixes (e.g., "e1", "e2").
#'
#' @return A matrix where the entry at \code{[i, j]} represents the number of times an element labeled \code{i} is followed by an element labeled \code{j} across all sequences.
#'
#' @details
#' The function assumes that elements in the sequences are labeled with prefixes (e.g., "e1", "e2"), which are stripped to extract the integer labels for counting. The matrix is initialized to be of size \code{num_elements x num_elements}, where \code{num_elements} should be defined in your script or session. Ensure that \code{num_elements} is set to the correct number of unique elements before running this function.
#'
#' @export
#'
#' @examples
#' sequences <- c("e1 e2 e3", "e2 e3 e1", "e1 e3")
#' num_elements <- 3
#' calculate_transition_counts(sequences)

calculate_transition_counts <- function(sequences) {
  # Ensure num_elements is defined
  num_elements <- length(unique(unlist(strsplit(sequences, " "))))

  # Initialize a transition count matrix
  transition_count_matrix <- matrix(0, nrow = num_elements, ncol = num_elements)

  # Calculate transition counts
  for (seq in sequences) {
    elements <- strsplit(seq, " ")[[1]]
    elements <- as.integer(sub("e", "", elements))

    for (i in 1:(length(elements) - 1)) {
      transition_count_matrix[elements[i], elements[i + 1]] <- transition_count_matrix[elements[i], elements[i + 1]] + 1
    }
  }

  return(transition_count_matrix)
}
