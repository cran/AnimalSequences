#' Temporal Overlap
#'
#' This function calculates the temporal overlap of elements in sequences.
#' It determines how much each element overlaps with other elements in the same sequence.
#'
#' @param sequences_long A data frame containing sequences with columns: sequence_nr, element, start_time, and end_time.
#'
#' @return A data frame summarizing the mean overlap elements and mean overlap proportion for each element.
#' @importFrom dplyr filter summarise group_by ungroup arrange bind_rows
#' @export
#'
#' @examples
#' sequences_long <- data.frame(
#'   sequence_nr = c(1, 1, 1, 2, 2),
#'   element = c("A", "B", "C", "A", "B"),
#'   start_time = c(0, 5, 10, 0, 5),
#'   end_time = c(5, 10, 15, 5, 10)
#' )
#' result <- temporal_overlap(sequences_long)
#' print(result)
#'
temporal_overlap <- function(sequences_long) {

  overlaps <- lapply(1:nrow(sequences_long), function(i) {
    subset_data <- sequences_long %>%
      filter(.data$sequence_nr == sequences_long$sequence_nr[i] &
               .data$start_time >= sequences_long$start_time[i] &
               .data$start_time <= sequences_long$end_time[i])

    if (nrow(subset_data) == 0) {
      overlap_elements <- 0
      overlap_proportion <- 0
    } else {
      element_duration <- sequences_long$end_time[i] - sequences_long$start_time[i]
      overlap_elements <- nrow(subset_data)
      overlap_start <- min(subset_data$start_time)
      overlap_end <- min(max(subset_data$end_time), sequences_long$end_time[i])
      overlap_proportion <- (overlap_end - overlap_start) / element_duration
    }

    return(
      data.frame(element = sequences_long$element[i],
                 overlap_elements = overlap_elements,
                 overlap_proportion = overlap_proportion)
    )
  })

  overlaps %>%
    bind_rows() %>%
    group_by(.data$element) %>%
    summarise(overlap_elements = mean(.data$overlap_elements),
              overlap_proportion = mean(.data$overlap_proportion)) %>%
    ungroup() %>%
    arrange(.data$element) %>%
    return()
}
