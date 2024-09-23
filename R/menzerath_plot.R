#' Create a Menzerath-Altmann Plot
#'
#' This function generates a Menzerath-Altmann plot from a data frame in long format. The plot visualizes the relationship between the number of elements in sequences and the mean duration of these sequences.
#'
#' @param sequences_long A data frame in long format. It should include columns for `sequence_nr`, `start_time`, and `end_time`. Each row represents an element in the sequence with its start and end times.
#'
#' @return A `ggplot` object. The plot shows the number of elements (x-axis) against the mean duration of sequences (y-axis) with a linear regression line.
#'
#' @details
#' The function calculates the duration of each element as the difference between `end_time` and `start_time`. It then groups the data by `sequence_nr` to compute the number of elements and the mean duration of each sequence. The resulting plot helps in understanding the relationship described by the Menzerath-Altmann law, which postulates that larger linguistic units tend to have shorter mean durations.
#'
#' @import dplyr
#' @import ggplot2
#' @export
#'
#' @examples
#' # Sample data frame
#' sequences_long <- data.frame(
#'   sequence_nr = rep(1:5, each = 3),
#'   start_time = rep(1:3, times = 5),
#'   end_time = rep(2:4, times = 5)
#' )
#' menzerath_plot(sequences_long)
menzerath_plot <- function(sequences_long) {
  sequences_long %>%
    mutate(duration = .data$end_time - .data$start_time) %>%
    group_by(.data$sequence_nr) %>%
    summarise(n = n(), mean_duration = mean(.data$duration)) %>%
    ungroup() %>%
    ggplot(aes(x = .data$n, y = .data$mean_duration)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", se = TRUE) +
    labs(x = "Number of elements", y = "Mean duration") +
    theme_minimal() +
    # make x-axis only show integers
    scale_x_continuous(breaks = seq(0,
                                    max(table(sequences_long$sequence_nr)),
                                    by = round(max(table(sequences_long$sequence_nr))/4)))
}
