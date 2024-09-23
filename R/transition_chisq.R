#' Perform a Chi-Squared Test for Transition Counts
#'
#' This function performs a chi-squared test to determine if there are significant differences between observed and expected transition counts in sequences. It calculates the chi-squared statistic and tests the null hypothesis that transitions occur according to the expected frequencies.
#'
#' @param sequences A vector of sequences, where each sequence is a character string with elements separated by spaces.
#' @param alpha A numeric value representing the significance level for the chi-squared test. Default is 0.05.
#'
#' @return A list with two elements:
#' \item{significant}{A logical value indicating whether the chi-squared test result is significant at the given significance level.}
#' \item{p_value}{A numeric value representing the p-value of the chi-squared test.}
#'
#' @details
#' The function calculates observed transition counts from the input sequences, computes expected transition counts based on row and column sums, and performs a chi-squared test to compare observed and expected counts. The test determines if the transitions in the sequences differ significantly from what would be expected by chance.
#'
#' @examples
#' # Define sequences
#' sequences <- c('e1 e2 e3', 'e2 e1 e3', 'e3 e2 e1')
#'
#' # Perform chi-squared test
#' transition_chisq(sequences, alpha = 0.05)
#' @importFrom stats pchisq
#' @export
transition_chisq <- function(sequences, alpha = 0.05) {

  # Calculate transition counts
  observed_counts <- calculate_transition_counts(sequences)
  num_elements <- nrow(observed_counts)

  # Calculate the row and column sums
  row_sums <- rowSums(observed_counts)
  col_sums <- colSums(observed_counts)
  total_count <- sum(observed_counts)

  # Calculate the expected counts under the null hypothesis
  expected_counts <- outer(row_sums, col_sums) / total_count

  # Perform the chi-squared test
  chisq_stat <- sum((observed_counts - expected_counts)^2 / expected_counts, na.rm = TRUE)
  df <- (num_elements - 1) * (num_elements - 1)
  p_value <- 1 - pchisq(chisq_stat, df)

  # Test if the p-value is below the significance level
  significant <- p_value < alpha

  return(list(significant = significant, p_value = p_value))
}
