#' Calculate Median Position of Each Element in Sequences
#'
#' This function calculates the median position of each element across sequences,
#' summarizes the distribution, and compares it to a shuffled distribution.
#'
#' @param sequences A character vector of sequences to analyze.
#' @param n_permutations The number of permutations to use for the null distribution.
#' @return A data frame with the median position, standard deviation, expected position, effect size, and p-value for each element.
#' @examples
#' # Example usage:
#' sequences <- c("A B C", "A B", "A C", "B C", "A B C D")
#' result <- element_position(sequences)
#' print(result)
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @importFrom stats median sd
#' @export
element_position <- function(sequences, n_permutations = 1000) {
  # Split the elements
  elements <- unique(str_split(sequences, ' ') %>% unlist()) %>% sort()

  # Split the sequences
  sequences_split <- str_split(sequences, ' ')

  element_summary <- lapply(elements, function(y) {
    element_positions <- sapply(sequences_split, function(x) {
      which(x == y)
    }) %>% unlist()

    median_position <- median(element_positions)
    sd_position <- sd(element_positions)

    boots <- lapply(1:n_permutations, function(k) {
      sapply(str_split(shuffle_sequences_within(sequences), ' '), function(x) {
        which(x == y)
      }) %>% unlist() %>% median()
    }) %>% unlist()

    return(
      data.frame(
        element = y,
        count = element_positions %>% unlist() %>% length(),
        median_position = median_position,
        sd_position = sd_position,
        expected_position = boots %>% mean(),
        effect_size = median_position - (boots %>% mean()),
        p_value = ifelse(
          median_position > (boots %>% mean()),
          sum(boots >= median_position) / length(boots),
          sum(boots <= median_position) / length(boots)
        )
      )
    )
  }) %>%
    bind_rows()

  return(element_summary)
}
