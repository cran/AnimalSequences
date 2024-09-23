#' Plot the Distribution of Sequence Lengths
#'
#' This function plots the distribution of the lengths of sequences of elements, where each sequence is split by spaces. The plot includes a histogram and a vertical line indicating the mean length.
#'
#' @param sequences A character vector where each element is a sequence of elements separated by spaces.
#'
#' @return A `ggplot` object showing the distribution of sequence lengths.
#'
#' @examples
#' sequences <- c('hello world', 'hello world hello', 'hello world hello world')
#' plot_seq_length_distribution(sequences)
#' @export
plot_seq_length_distribution <- function(sequences){
  return(ggplot(data = data.frame(sequence_length = str_count(sequences, ' ') + 1), aes(x = .data$sequence_length)) +
           geom_histogram(binwidth = 1, fill = 'blue', color = 'black', alpha = 0.4) +
           geom_vline(aes(xintercept = mean(.data$sequence_length)), color = 'red', linetype = 'dashed', linewidth = 1) +
           theme_minimal() +
           labs(title = 'Distribution of Sequence Lengths',
                x = 'Sequence Length',
                y = 'Frequency'))
}
