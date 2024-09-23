#' Calculate Element-Covariate Conditional Probabilities
#'
#' This function calculates the conditional probability of each element given each covariate and
#' performs permutation tests to compute the expected conditional probabilities and p-values.
#'
#' @param sequences_long A data frame containing the sequences, with columns for elements and covariates.
#' @param element A string specifying the column name for elements in the sequences data frame.
#' @param covariate A string specifying the column name for covariates in the sequences data frame.
#' @param n_permutations An integer specifying the number of permutations for the bootstrapping process.
#' @return A data frame with the calculated probabilities, expected probabilities, and p-values for each element-covariate pair.
#'
#' @importFrom tidyr unite
#' @examples
#' # Example usage:
#' sequences_long <- data.frame(
#'   element = rep(letters[1:3], each = 4),
#'   covariate = rep(letters[4:7], times = 3)
#' )
#' result <- element_covariate(sequences_long,
#'                             element = 'element',
#'                             covariate = 'covariate',
#'                             n_permutations = 50)
#' print(result)
#' @export

element_covariate <- function(sequences_long, element = 'element', covariate = 'covariate', n_permutations = 1000) {
  # Calculate the conditional probability of each element given each covariate

  sequences_long$element <- sequences_long[,paste(element)] %>% unlist()
  sequences_long$covariate <- sequences_long[,paste(covariate)] %>% unlist()

  conditional_probability <-
    sequences_long %>%
    unite('element_covariate', .data$element, .data$covariate, sep = '_', remove = FALSE) %>%
    add_count(name = 'count', .data$element_covariate) %>%
    add_count(name = 'element_count', .data$element) %>%
    mutate(probability = .data$count / .data$element_count) %>%
    group_by(.data$element, .data$covariate) %>%
    summarise(probability = mean(.data$probability), count = mean(.data$count), .groups = 'keep') %>%
    ungroup()

  # Calculate the expected conditional probability of each element given each covariate
  boot_probability <- lapply(1:n_permutations, function(i) {
    # Shuffle the elements across covariates
    shuffled_sequences <- sequences_long %>%
      mutate(covariate = sample(.data$covariate)) %>%
      unite('element_covariate', .data$element, .data$covariate, sep = '_', remove = FALSE) %>%
      add_count(name = 'count', .data$element_covariate) %>%
      add_count(name = 'element_count', .data$element) %>%
      mutate(probability = .data$count / .data$element_count) %>%
      group_by(.data$element, .data$covariate) %>%
      summarise(probability = mean(.data$probability), count = mean(.data$count), .groups = 'keep') %>%
      ungroup()

    conditional_probability %>%
      left_join(shuffled_sequences, by = c('element', 'covariate'), suffix = c('', '_shuffled')) %>%
      pull(.data$probability_shuffled) %>% replace_na(0) %>% return()
  }) %>% bind_cols() %>%
    # prevent warnings
    suppressMessages()

  # Compute the p-values for each element-covariate pair
  conditional_probability %>%
    mutate(expected_probability = boot_probability %>% rowMeans()) %>%
    mutate(p_value = ifelse(
      .data$probability > .data$expected_probability,
      rowSums(boot_probability >= conditional_probability$probability) / n_permutations,
      rowSums(boot_probability <= conditional_probability$probability) / n_permutations
    )) %>%
    select(.data$element, .data$covariate, .data$count, .data$probability, .data$expected_probability, .data$p_value) %>%
    arrange(.data$covariate, .data$element) %>%
    # round all numeric columns
    mutate_if(is.numeric, round, 3) %>%
    return()
}
