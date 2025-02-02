% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zipfs_plot.R
\name{zipf_plot}
\alias{zipf_plot}
\title{Create a Zipf's Law Plot}
\usage{
zipf_plot(sequences_long)
}
\arguments{
\item{sequences_long}{A data frame containing at least one column named `element` which represents the elements of sequences. Each element's frequency is used to create the plot.}
}
\value{
A `ggplot` object that visualizes the observed and expected frequencies of elements according to Zipf's law. The plot includes:
\item{Rank}{The rank of each element based on its frequency, plotted on a log scale.}
\item{Count}{The observed frequency of each element, plotted on a log scale.}
\item{Expected}{The expected frequency of each element if Zipf's law were true, shown as a grey dashed line.}
}
\description{
This function creates a log-log plot to visualize Zipf's law, which states that the frequency of a word is inversely proportional to its rank in the frequency table. The plot compares the observed frequency distribution of elements with the expected distribution if Zipf's law were true.
}
\details{
- **Observed Frequencies**: Calculated from the provided `sequences_long` data frame.
- **Expected Frequencies**: Calculated using Zipf's law formula, where the frequency of the element is inversely proportional to its rank.
- **Plotting**: Both observed and expected frequencies are plotted on a log-log scale to compare against Zipf's law.
}
\examples{
# Example data frame
sequences_long <- data.frame(element = c('a', 'b', 'a', 'c', 'b', 'a', 'd', 'c', 'b', 'a'))

# Generate the Zipf's law plot
zipf_plot(sequences_long)

}
