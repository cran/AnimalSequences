#' Perform a Statistical Test for Transition Probabilities
#'
#' This function performs a permutation test to evaluate the significance of observed transition probabilities in sequences. It compares the observed transition probabilities to those obtained from permuted sequences to determine if the observed probabilities are significantly different from what would be expected by chance.
#'
#' @param sequences A character vector of sequences where each sequence is represented as a string of elements separated by spaces.
#' @param observed_probs A data frame containing observed transition probabilities with columns \code{previous_element}, \code{element}, and \code{probability}.
#' @param n_permutations An integer specifying the number of permutations to perform. Default is 1000.
#'
#' @return A data frame with the observed transition probabilities, expected probabilities from permutations, and p-values for each transition. The data frame contains the following columns:
#' \item{previous_element}{The element preceding the transition.}
#' \item{element}{The element following the transition.}
#' \item{probability}{The observed probability of the transition.}
#' \item{expected_probability}{The mean probability of the transition obtained from permuted sequences.}
#' \item{p_value}{The p-value indicating the significance of the observed probability compared to the permuted probabilities.}
#'
#' @details
#' - **Observed Transition Probabilities**: Calculated from the input sequences.
#' - **Permutations**: The sequences are permuted \code{n_permutations} times, and transition probabilities are computed for each permutation.
#' - **P-Values**: Calculated as the proportion of permuted transition probabilities that are greater than or equal to the observed transition probabilities.
#'
#' @examples
#' # Example sequences
#' sequences <- c('e1 e2 e3', 'e2 e3 e4', 'e3 e4 e1')
#'
#' # Calculate observed transition probabilities
#' observed_probs <- calculate_transition_probs(sequences)
#'
#' # Perform the transition test
#' test_results <- transition_test(sequences, observed_probs, n_permutations = 50)
#'
#' # View results
#' head(test_results)
#'
#' @importFrom dplyr left_join mutate pull
#' @importFrom tidyr replace_na
#' @export

transition_test <- function(sequences, observed_probs, n_permutations = 1000) {

  # Initialize a matrix to store the transition probabilities of the permuted sequences
  permuted_transition_probs <- matrix(0, nrow = n_permutations, ncol = nrow(observed_probs))

  # Perform the permutations
  for (i in 1:n_permutations) {
    permuted_sequences <- shuffle_sequences_across(sequences)
    permuted_probs <- calculate_transition_probs(permuted_sequences)
    permuted_transition_probs[i,] <-
      left_join(observed_probs, permuted_probs, by = c("previous_element", "element")) %>%
      pull(.data$probability.y) %>% replace_na(0)
  }

  # Compute the p-values for each dyad
  p_values <- colSums(permuted_transition_probs >= observed_probs$probability) / n_permutations

  return(observed_probs %>% ungroup() %>%
           mutate(expected_probability = colMeans(permuted_transition_probs),
                  p_value = p_values))
}
