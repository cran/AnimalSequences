#' Calculate Observed and Expected Redundancy of Sequences
#'
#' This function calculates the observed redundancy of sequences and compares it to expected redundancy values obtained from shuffled sequences. The redundancy is defined as the proportion of consecutive identical elements in the sequences.
#'
#' @param sequences A vector of character strings, where each string represents a sequence of elements separated by spaces.
#'
#' @return A data frame with the following columns:
#' \item{redundancy}{The observed redundancy in the original sequences. This is the mean proportion of consecutive identical elements across all sequences.}
#' \item{redundancy_expected_across}{The expected redundancy obtained from sequences where elements have been shuffled across the sequences.}
#' \item{redundancy_expected_within}{The expected redundancy obtained from sequences where elements have been shuffled within each sequence.}
#'
#' @details
#' The function calculates redundancy as the proportion of consecutive identical elements within each sequence. It then compares this observed redundancy to expected values derived from sequences where elements are shuffled either across sequences or within each sequence. The function relies on auxiliary functions `shuffle_sequences_across` and `shuffle_sequences_within` for generating the shuffled sequences.
#'
#' @import dplyr
#' @import stringr
#' @importFrom stats na.omit
#' @export
#'
#' @examples
#' # Example sequences
#' sequences <- c("A A B C C", "B A A C C", "A B C C C")
#' # Compute redundancy
#' redundancy(sequences)
redundancy <- function(sequences) {

  # Calculate redundancy for the original sequences
  redundancy <- sapply(sequences, function(x) {
    # Split each sequence by spaces and unlist the result
    yy <- str_split(x, ' ') %>% unlist()

    # If the sequence has only one element, return NA
    if(length(yy) == 1) {
      return(NA)
    }

    # Check for redundancy by comparing each element with the previous one
    redundancy <- sapply(2:length(yy), function(y) {
      yy[y] == yy[y-1]
    })

    return(redundancy)
  }, USE.NAMES = FALSE) %>%
    # Unlist the results, remove NA values, and calculate the mean
    unlist() %>% na.omit() %>% mean()

  # Calculate expected redundancy for the shuffled sequences
  redundancy_expected_across <- sapply(shuffle_sequences_across(sequences), function(x) {
    # Split each shuffled sequence by spaces and unlist the result
    yy <- str_split(x, ' ') %>% unlist()

    # If the sequence has only one element, return NA
    if(length(yy) == 1) {
      return(NA)
    }

    # Check for redundancy by comparing each element with the previous one
    redundancy <- sapply(2:length(yy), function(y) {
      yy[y] == yy[y-1]
    })

    return(redundancy)
  }, USE.NAMES = FALSE) %>%
    # Unlist the results, remove NA values, and calculate the mean
    unlist() %>% na.omit() %>% mean()

  # Calculate expected redundancy for the shuffled sequences within each sequence
  redundancy_expected_within <- sapply(shuffle_sequences_within(sequences), function(x) {
    # Split each shuffled sequence by spaces and unlist the result
    yy <- str_split(x, ' ') %>% unlist()

    # If the sequence has only one element, return NA
    if(length(yy) == 1) {
      return(NA)
    }

    # Check for redundancy by comparing each element with the previous one
    redundancy <- sapply(2:length(yy), function(y) {
      yy[y] == yy[y-1]
    })

    return(redundancy)
  }, USE.NAMES = FALSE) %>%
    # Unlist the results, remove NA values, and calculate the mean
    unlist() %>% na.omit() %>% mean()

  # Return a data frame with the calculated redundancy and expected redundancy
  return(data.frame(
    redundancy = redundancy,
    redundancy_expected_across = redundancy_expected_across,
    redundancy_expected_within = redundancy_expected_within
  ))
}
