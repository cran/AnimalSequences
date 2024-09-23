
#' Calculate Individual Element Durations
#'
#' This function calculates the individual element durations and compares them to a shuffled distribution.
#'
#' @param sequences_long A data frame containing sequences with start and end times for each element.
#' @param n_permutations An integer specifying the number of permutations to perform. Default is 1000.
#' @return A data frame with the median duration, standard deviation, expected duration, effect size, and p-value for each element.
#' @examples
#' # Example usage:
#' sequences_long <- data.frame(
#'   element = c("A", "B", "C", "A", "B", "C"),
#'   start_time = c(0, 5, 10, 15, 20, 25),
#'   end_time = c(5, 10, 15, 20, 25, 30)
#' )
#' result <- element_duration(sequences_long, n_permutations = 100)
#' print(result)
#' @import dplyr
#' @importFrom stats median sd
#' @export
element_duration <- function(sequences_long, n_permutations = 1000) {
  # Split the elements
  elements <- unique(sequences_long$element) %>% sort()

  element_summary <- lapply(elements, function(y) {
    element_durations <- sequences_long %>%
      filter(.data$element == y) %>%
      mutate(duration = .data$end_time - .data$start_time) %>%
      pull(.data$duration)

    median_duration <- median(element_durations)
    sd_duration <- sd(element_durations)

    boots <- lapply(1:n_permutations, function(k) {
      sequences_long %>%
        mutate(element = sample(sequences_long$element)) %>%
        filter(.data$element == y) %>%
        mutate(duration = .data$end_time - .data$start_time) %>%
        pull(.data$duration) %>% median()
    }) %>% unlist()

    return(
      data.frame(
        element = y,
        count = element_durations %>% length(),
        median_duration = round(median_duration, 3),
        sd_duration = round(sd_duration, 3),
        expected_duration = round(boots %>% mean(), 3),
        effect_size = round(median_duration - (boots %>% mean()), 3),
        p_value = ifelse(
          median_duration > (boots %>% mean()),
          sum(boots >= median_duration) / length(boots),
          sum(boots <= median_duration) / length(boots)
        )
      )
    )
  }) %>%
    bind_rows()

  return(element_summary)
}
