#' Shuffle Elements Within Each Sequence
#'
#' This function shuffles the elements within each sequence independently.
#'
#' @param sequences A character vector of sequences to shuffle.
#' @return A character vector of sequences with elements shuffled within each sequence.
#' @examples
#' # Example usage:
#' sequences <- c("A B C", "D E F", "G H I")
#' result <- shuffle_sequences_within(sequences)
#' print(result)
#' @export
shuffle_sequences_within <- function(sequences) {
  shuffled_sequences <- sapply(sequences, function(seq) {
    elements <- strsplit(seq, " ")[[1]]
    shuffled_elements <- sample(elements)
    paste(shuffled_elements, collapse = " ")
  })
  return(shuffled_sequences)
}
