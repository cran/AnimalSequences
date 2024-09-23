
#' Shuffle Elements Across All Sequences
#'
#' This function shuffles elements across all sequences, preserving the lengths of the original sequences.
#'
#' @param sequences A character vector of sequences to shuffle.
#' @return A character vector of sequences with elements shuffled across all sequences.
#' @examples
#' # Example usage:
#' sequences <- c("A B C", "D E F", "G H I")
#' result <- shuffle_sequences_across(sequences)
#' print(result)
#' @export

shuffle_sequences_across <- function(sequences) {
  # Flatten all sequences into one vector
  all_elements <- unlist(strsplit(sequences, " "))
  # Shuffle the elements
  shuffled_elements <- sample(all_elements, size = length(all_elements), replace = FALSE)
  # Reassemble the sequences with the same lengths as before
  shuffled_sequences <- rep(NA, length(sequences))
  idx <- 1
  # Loop through each sequence
  for (i in 1:length(sequences)) {
    # Calculate the length of the original sequence
    len <- length(strsplit(sequences[i], " ")[[1]])
    # Reconstruct the shuffled sequence by pasting the shuffled elements
    shuffled_sequences[i] <- paste(shuffled_elements[idx:(idx + len - 1)], collapse = " ")
    # Update the index for the next sequence
    idx <- idx + len
  }
  return(shuffled_sequences)
}
