#' Calculate Transition Entropy for Sequences
#'
#' This function calculates the transition entropy for sequences using n-grams.
#' It performs bootstrapping to compute entropy and expected entropy over multiple iterations.
#'
#' @param sequences A list of sequences (character vectors) to analyze.
#' @param ngram The size of the n-gram (default is 2).
#' @param iterations The number of bootstrap iterations (default is 20).
#' @return A data frame with calculated entropies, expected entropies, and entropy ratios for each iteration.
#' @importFrom tidytext unnest_tokens
#' @importFrom dplyr bind_rows mutate select filter
#' @importFrom tidyr separate unite
#' @importFrom tibble enframe
#' @examples
#' sequences <- unlist(list("A B C", "B C A", "C A B"))
#' transition_entropy(sequences, ngram = 2, iterations = 20)
#'
#' @export
#'
transition_entropy <- function(sequences, ngram = 2, iterations = 20) {

  entros <- lapply(1:iterations, function(y) {
    boot_sequences <- sample(sequences, replace = TRUE)

    # Create ngrams from bootstrapped sequences
    dataset <- boot_sequences %>%
      enframe(name = NULL, value = "sequence") %>%
      unnest_tokens(output = .data$element, input = .data$sequence, token = "ngrams", n = ngram, to_lower = FALSE, drop = FALSE) %>%
      select(.data$element) %>%
      filter(!is.na(.data$element)) %>%
      separate(.data$element, into = str_c('c', 1:ngram), sep = " ", remove = FALSE) %>%
      select(-.data$element) %>%
      unite(col = 'consequent', str_c('c', ngram), sep = ' ', remove = TRUE) %>%
      unite(col = 'antecedent', str_c('c', 1:(ngram-1)), sep = ' ', remove = TRUE)

    # Create ngrams from shuffled sequences
    dataset_random <- shuffle_sequences_across(boot_sequences) %>%
      enframe(name = NULL, value = "sequence") %>%
      unnest_tokens(output = .data$element, input = .data$sequence, token = "ngrams", n = ngram, to_lower = FALSE, drop = FALSE) %>%
      select(.data$element) %>%
      filter(!is.na(.data$element)) %>%
      separate(.data$element, into = str_c('c', 1:ngram), sep = " ", remove = FALSE) %>%
      select(-.data$element) %>%
      unite(col = 'consequent', str_c('c', ngram), sep = ' ', remove = TRUE) %>%
      unite(col = 'antecedent', str_c('c', 1:(ngram-1)), sep = ' ', remove = TRUE)

    # Calculate entropies for real and shuffled sequences
    entropy_values <- data.frame(
      entropy = calculate_conditional_entropy(dataset$consequent, dataset$antecedent),
      expected_entropy = calculate_conditional_entropy(dataset_random$consequent, dataset_random$antecedent),
      iteration = y
    )

    return(entropy_values)
  }) %>%
    bind_rows() %>%
    mutate(entropy_ratio = .data$entropy / .data$expected_entropy) %>%
    mutate(ngram = ngram) %>%
    return()
}
