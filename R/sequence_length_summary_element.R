#' Summarize Sequence Lengths by Element
#'
#' This function calculates summary statistics for the lengths of sequences containing specific distinct elements. It performs the summary for each distinct element found across the sequences.
#'
#' @param sequences A character vector where each element is a sequence of elements separated by spaces.
#'
#' @return A data frame with the following columns:
#' \item{element}{The distinct element.}
#' \item{mean_seq_elements}{The mean length of sequences containing the element.}
#' \item{sd_seq_elements}{The standard deviation of the lengths of sequences containing the element.}
#' \item{median_seq_elements}{The median length of sequences containing the element.}
#' \item{min_seq_elements}{The minimum length of sequences containing the element.}
#' \item{max_seq_elements}{The maximum length of sequences containing the element.}
#' \item{distinct_elements}{The number of distinct elements in sequences containing the element.}
#' \item{pvalue_distinct_elements}{The p-value comparing the true number of distinct elements to shuffled sequences.}
#'
#' @examples
#' sequences <- c('hello world', 'hello world hello', 'hello world hello world')
#' sequence_length_summary_element(sequences)
#' @export
sequence_length_summary_element <- function(sequences){
  elements <- unique(str_split(sequences, ' ') %>% unlist()) %>% sort()

  sequences_split <- str_split(sequences, ' ')

  element_summary <- lapply(elements, function(y){
    element_sequences <- sequences[sapply(sequences_split, function(k) y %in% k)]

    return(data.frame(
      element = y,
      mean_seq_elements = average_seq_length(element_sequences),
      sd_seq_elements = sd_seq_length(element_sequences),
      median_seq_elements = median_seq_length(element_sequences),
      min_seq_elements = min_max_seq_length(element_sequences)[1],
      max_seq_elements = min_max_seq_length(element_sequences)[2],
      distinct_elements = count_distinct_elements(element_sequences),
      pvalue_distinct_elements = compare_distinct_elements_per_list_item(element_sequences)$p_value
    ))
  })

  return(bind_rows(element_summary))
}
