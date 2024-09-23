#' Calculate Transition Probabilities from Sequences
#'
#' This function calculates the transition probabilities between elements in a set of sequences. It computes the probability of transitioning from one element to another based on the frequency of transitions observed in the input sequences.
#'
#' @param sequences A vector of character strings, where each string represents a sequence of elements separated by spaces.
#'
#' @return A data frame with the following columns:
#' \item{previous_element}{The element that transitions to the next element.}
#' \item{element}{The element that follows the previous element.}
#' \item{count}{The number of times the transition from the previous element to the current element occurs.}
#' \item{probability}{The probability of transitioning from the previous element to the current element.}
#'
#' @details
#' The function uses the `unnest_tokens` function from the `tidytext` package to split sequences into individual elements. It then calculates transition counts and probabilities for each pair of consecutive elements in the sequences. The resulting data frame shows the transition probabilities for each possible element pair.
#'
#' @importFrom tidytext unnest_tokens
#' @importFrom tibble enframe
#' @importFrom dplyr mutate group_by summarise ungroup select
#' @export
#'
#' @examples
#' library(tidytext)
#' sequences <- c("A B C", "A B", "B C A")
#' calculate_transition_probs(sequences)
calculate_transition_probs <- function(sequences) {
  sequences %>%
    # Create a data frame
    enframe(name = NULL, value = "sequence") %>%
    mutate(id = row_number()) %>%
    # Unnest the elements
    unnest_tokens(output = .data$element, input = .data$sequence, token = "words", to_lower = FALSE, drop = FALSE) %>%
    # Group by sequence and calculate the lag
    group_by(.data$id) %>%
    mutate(previous_element = lag(.data$element)) %>%
    # Drop the NA values
    drop_na() %>%
    # Count the transitions
    count(.data$previous_element, .data$element) %>%
    # Summarise across sequences
    group_by(.data$previous_element, .data$element) %>%
    summarise(count = sum(.data$n), .groups = 'keep') %>%
    ungroup() %>%
    # Group by the previous element
    group_by(.data$previous_element) %>%
    # Calculate the sum of the counts
    mutate(total = sum(.data$count)) %>%
    # Calculate the transition probabilities
    mutate(probability = .data$count / .data$total) %>%
    # Select the required columns
    select(.data$previous_element, .data$element, .data$count, .data$probability)
}
