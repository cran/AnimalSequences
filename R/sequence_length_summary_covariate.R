#' Summarize Sequence Lengths by Covariate
#'
#' This function calculates summary statistics for the lengths of sequences of elements, grouped by a specified covariate. It includes mean, standard deviation, median, minimum, and maximum lengths, along with the number of distinct elements and the p-value comparing to shuffled sequences.
#'
#' @param sequences A character vector where each element is a sequence of elements separated by spaces.
#' @param covariate A vector of covariates with the same length as `sequences`, used to group the sequences.
#'
#' @return A data frame with the following columns:
#' \item{covariate}{The value of the covariate.}
#' \item{mean_seq_elements}{The mean length of sequences for this covariate value.}
#' \item{sd_seq_elements}{The standard deviation of the sequence lengths for this covariate value.}
#' \item{median_seq_elements}{The median length of sequences for this covariate value.}
#' \item{min_seq_elements}{The minimum length of sequences for this covariate value.}
#' \item{max_seq_elements}{The maximum length of sequences for this covariate value.}
#' \item{distinct_elements}{The number of distinct elements for this covariate value.}
#' \item{pvalue_distinct_elements}{The p-value comparing the number of distinct elements to shuffled sequences for this covariate value.}
#'
#' @examples
#' sequences <- c('hello world', 'hello world hello', 'hello world hello world')
#' covariate <- c('A', 'B', 'A')
#' sequence_length_summary_covariate(sequences, covariate)
#' @export
sequence_length_summary_covariate <- function(sequences, covariate){

  covariate_summary <- lapply(unique(covariate), function(y){
    covariate_sequences <- sequences[covariate == y]

    return(data.frame(
      covariate = y,
      mean_seq_elements = average_seq_length(covariate_sequences),
      sd_seq_elements = sd_seq_length(covariate_sequences),
      median_seq_elements = median_seq_length(covariate_sequences),
      min_seq_elements = min_max_seq_length(covariate_sequences)[1],
      max_seq_elements = min_max_seq_length(covariate_sequences)[2],
      distinct_elements = count_distinct_elements(covariate_sequences),
      pvalue_distinct_elements = compare_distinct_elements_per_list_item(covariate_sequences)$p_value
    ))
  })

  return(bind_rows(covariate_summary))
}
