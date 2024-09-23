#' Create a Zipf's Law Plot
#'
#' This function creates a log-log plot to visualize Zipf's law, which states that the frequency of a word is inversely proportional to its rank in the frequency table. The plot compares the observed frequency distribution of elements with the expected distribution if Zipf's law were true.
#'
#' @param sequences_long A data frame containing at least one column named `element` which represents the elements of sequences. Each element's frequency is used to create the plot.
#'
#' @return A `ggplot` object that visualizes the observed and expected frequencies of elements according to Zipf's law. The plot includes:
#' \item{Rank}{The rank of each element based on its frequency, plotted on a log scale.}
#' \item{Count}{The observed frequency of each element, plotted on a log scale.}
#' \item{Expected}{The expected frequency of each element if Zipf's law were true, shown as a grey dashed line.}
#'
#' @details
#' - **Observed Frequencies**: Calculated from the provided `sequences_long` data frame.
#' - **Expected Frequencies**: Calculated using Zipf's law formula, where the frequency of the element is inversely proportional to its rank.
#' - **Plotting**: Both observed and expected frequencies are plotted on a log-log scale to compare against Zipf's law.
#'
#' @examples
#' # Example data frame
#' sequences_long <- data.frame(element = c('a', 'b', 'a', 'c', 'b', 'a', 'd', 'c', 'b', 'a'))
#'
#' # Generate the Zipf's law plot
#' zipf_plot(sequences_long)
#'
#' @import ggplot2
#' @export
zipf_plot <- function(sequences_long){
  # Create a data frame with the frequencies of the words
  freqs <- table(sequences_long$element)
  freqs <- as.data.frame(freqs)
  colnames(freqs) <- c("Element", "Count")

  # Sort the data frame by frequency
  freqs <- freqs[order(-freqs$Count),]

  # Create a rank column
  freqs$Rank <- 1:nrow(freqs)

  # Calculate the expected frequency for each element if Zipf's law was true
  freqs$Expected <- max(freqs$Count) / freqs$Rank

  # Create a log-log plot
  ggplot(freqs, aes(x=.data$Rank, y=.data$Count)) +
    geom_point() +
    geom_line(aes(x=.data$Rank, y=.data$Expected), color='grey', linetype = 2, linewidth = 1.5) +
    scale_x_log10() +
    scale_y_log10(limits = c(1, max(freqs$Count))) +
    theme_minimal() +
    ggtitle('Zipf\'s Law Plot') +
    xlab('Rank') +
    ylab('Frequency')
}
