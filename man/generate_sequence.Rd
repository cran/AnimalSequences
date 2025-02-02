% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_sequences.R
\name{generate_sequence}
\alias{generate_sequence}
\title{Generate a Sequence of Elements}
\usage{
generate_sequence(length_func, transition_matrix, first_element_probs)
}
\arguments{
\item{length_func}{A function that generates a numeric value representing the length of the sequence. It is typically a random function that defines the length distribution of the sequence.}

\item{transition_matrix}{A matrix representing the transition probabilities between elements. Each entry in the matrix indicates the probability of transitioning from one element to another.}

\item{first_element_probs}{A numeric vector of probabilities for selecting the first element in the sequence. The length of the vector should match the number of possible elements.}
}
\value{
A character string representing the generated sequence. The sequence elements are prefixed with "e" and separated by spaces.
}
\description{
This function generates a sequence of elements based on a given length function, a transition matrix, and probabilities for the first element. The sequence is generated by sampling from the transition matrix and then combining the sampled elements into a single sequence string.
}
\examples{
# Define parameters
num_elements <- 3
average_sequence_length <- 5
sequence_length_sd <- 1
length_func <- function() {
  rnorm(1, mean = average_sequence_length, sd = sequence_length_sd)
}
transition_matrix <- matrix(c(0.1, 0.6, 0.3,
                               0.2, 0.5, 0.3,
                               0.3, 0.3, 0.4), nrow = 3, byrow = TRUE)
first_element_probs <- c(0.3, 0.4, 0.3)

# Generate a sequence
generate_sequence(length_func, transition_matrix, first_element_probs)

}
