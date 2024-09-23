#' Convert Sequences to Long Format
#'
#' This function converts a data frame with sequences into long format. It expands each sequence into individual rows, optionally including start and end times and covariates.
#'
#' @param sequences A data frame containing sequences.
#' @param sequence Column name with the sequences.
#' @param start_time Column name with the start time. Defaults to NULL.
#' @param end_time Column name with the end time. Defaults to NULL.
#' @param covariates A vector with column names of the covariates. Defaults to NULL.
#'
#' @return A data frame in long format with sequences, start time, end time, duration, and covariates.
#' @importFrom dplyr select filter mutate rename bind_rows
#' @importFrom rlang :=
#' @importFrom utils head tail
#' @export
#'
#' @examples
#' sequences <- data.frame(sequence = c('A B C', 'A B', 'A C', 'B C'),
#'                         covariate1 = c('X', 'Y', 'X', 'Y'),
#'                         covariate2 = c('M', 'N', 'M', 'N'))
#' sequences_to_long(sequences,
#'                   sequence = 'sequence',
#'                   covariates = c('covariate1', 'covariate2'))

sequences_to_long <- function(sequences, sequence = 'sequence', start_time = NULL, end_time = NULL, covariates = NULL) {

    # Create a data frame with the sequences
    sequences_long <-
      lapply(1:nrow(sequences), function(x) {

        sequence_long <-
          sequences %>%
          select(!!as.name(sequence)) %>%
          filter(row_number() == x) %>%
          # Split the sequence into elements
          mutate(sequence = str_split(!!as.name(sequence), ' ')) %>%
          unnest(.data$sequence) %>%
          mutate(sequence_identifier = str_c('sequence', x)) %>%
          rename(element = sequence)

        if(!is.null(start_time)) {
          s_time <- sequences %>%
            filter(row_number() == x) %>%
            pull(!!as.name(start_time))
          e_time <- sequences %>%
            filter(row_number() == x) %>%
            pull(!!as.name(end_time))

          start_series <-
            seq(s_time, e_time, length.out = length(sequence_long$element)+1) %>% head(nrow(sequence_long))

          end_series <-
            seq(s_time, e_time, length.out = length(sequence_long$element)+1) %>% tail(nrow(sequence_long))

          sequence_long <- sequence_long %>%
            mutate(start_time = start_series) %>%
            mutate(end_time = end_series)
        }

        if(is.null(start_time)) {
          sequence_long <- sequence_long %>%
            mutate(start_time = row_number(), end_time = row_number() + 1, duration = 1)
        }

        if(length(covariates) > 0) {
          for(i in 1:length(covariates)) {
            sequence_long <- sequence_long %>%
              mutate(!!as.name(covariates[i]) := sequences %>%
                       filter(row_number() == x) %>%
                       pull(!!as.name(covariates[i])))
          }
        }

        return(sequence_long)
      }) %>% bind_rows()

    return(sequences_long)
}

