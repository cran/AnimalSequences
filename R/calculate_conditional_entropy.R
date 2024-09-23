#' Calculate Conditional Entropy of B given A in Bits
#'
#' This function calculates the conditional entropy of B given A in bits between two categorical vectors
#'
#' @param vectorA A categorical vector representing the conditioning variable A
#' @param vectorB A categorical vector representing the conditioned variable B
#' @return The conditional entropy of B given A in bits
#' @examples
#' vectorA <- c("A", "B", "A", "A", "B", "C", "C", "C", "A", "B")
#' vectorB <- c("X", "Y", "X", "Y", "X", "Y", "Y", "X", "X", "Y")
#' calculate_conditional_entropy(vectorA, vectorB)
#' @export

calculate_conditional_entropy <- function(vectorA, vectorB) {
  # Check if the lengths of the vectors are the same
  if (length(vectorA) != length(vectorB)) {
    stop("The lengths of the two vectors must be the same")
  }

  # Create a table of joint frequencies
  joint_freq <- table(vectorA, vectorB)

  # Calculate the joint probability distribution P(A, B)
  joint_prob <- joint_freq / sum(joint_freq)

  # Calculate the marginal probability distribution P(A)
  marginal_prob_A <- rowSums(joint_prob)

  # Calculate the conditional probability distribution P(B|A)
  conditional_prob_B_given_A <- joint_prob / marginal_prob_A

  # Calculate the conditional entropy H(B|A)
  conditional_entropy <- -sum(joint_prob * log2(conditional_prob_B_given_A + 1e-10))

  return(conditional_entropy)
}
