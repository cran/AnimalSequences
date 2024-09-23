#' Summarize Sequence Durations
#'
#' This function calculates summary statistics for the durations of sequences, where the duration is defined as the difference between `end_time` and `start_time`. If `duration` is provided, it will be used directly.
#'
#' @param sequences A character vector where each element is a sequence of elements separated by spaces.
#' @param start_time A numeric vector representing the start times of the sequences.
#' @param end_time A numeric vector representing the end times of the sequences.
#' @param duration (Optional) A numeric vector representing the durations of the sequences. If `NULL`, it will be calculated as `end_time - start_time`.
#'
#' @return A data frame with the following columns:
#' \item{mean_seq_duration}{The mean duration of the sequences.}
#' \item{sd_seq_duration}{The standard deviation of the sequence durations.}
#' \item{median_seq_duration}{The median duration of the sequences.}
#' \item{min_seq_duration}{The minimum duration of the sequences.}
#' \item{max_seq_duration}{The maximum duration of the sequences.}
#'
#' @examples
#' sequences <- c('hello world', 'hello world hello', 'hello world hello world')
#' start_time <- c(1, 2, 3)
#' end_time <- c(2, 4, 7)
#' sequence_duration_summary(sequences, start_time, end_time)
#' @importFrom stats sd median
#' @export
sequence_duration_summary <- function(sequences, start_time, end_time, duration = NULL){
  if(is.null(duration)) {
    duration <- end_time - start_time
  }

  return(data.frame(
    mean_seq_duration = mean(duration),
    sd_seq_duration = sd(duration),
    median_seq_duration = median(duration),
    min_seq_duration = min(duration),
    max_seq_duration = max(duration)
  ))
}
