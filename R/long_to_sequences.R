#' Convert Long Format to Sequences
#'
#' This function converts a data frame in long format into sequences by combining all rows with the same sequence identifier. It also aggregates covariates if provided.
#'
#' @param sequences_long A data frame in long format containing the sequences.
#' @param elements Column name for elements that should be combined into sequences.
#' @param sequence_identifier Column name with the sequence identifier.
#' @param start_time Column name with the start time.
#' @param end_time Column name with the end time.
#' @param covariates A vector with column names of the covariates. Defaults to NULL.
#'
#' @return A data frame with sequences, start time, end time, and aggregated covariates.
#' @importFrom dplyr select filter arrange summarise mutate pull
#' @importFrom rlang :=
#' @importFrom utils head tail
#' @export
#'
#' @examples
#' sequences_long <- data.frame(sequence_identifier = c(1, 1, 2, 2, 2),
#'                              element = c('A', 'B', 'A', 'B', 'C'),
#'                              start_time = c(1, 2, 1, 2, 3),
#'                              end_time = c(2, 3, 2, 3, 4),
#'                              covariate1 = c('X', 'Y', 'X', 'Y', 'Z'),
#'                              covariate2 = c('M', 'N', 'M', 'N', 'O'))
#' long_to_sequences(sequences_long,
#'                   elements = 'element',
#'                   sequence_identifier = 'sequence_identifier',
#'                   start_time = 'start_time',
#'                   covariates = c('covariate1', 'covariate2'))

long_to_sequences <- function(sequences_long, elements = 'element', sequence_identifier = 'sequence_identifier', start_time = 'start_time', end_time = 'end_time', covariates = NULL) {
  # Create a data frame with the sequences
  sequence_frame <-
    lapply(unique(sequences_long[,sequence_identifier] %>% unlist()), function(x) {

      sequence_frame <-
        sequences_long %>%
        select(!!as.name(sequence_identifier),
               !!as.name(elements),
               !!as.name(start_time),
               !!as.name(end_time),
               !!as.name(covariates)) %>%
        filter(!!as.name(sequence_identifier) == x) %>%
        arrange(!!as.name(start_time)) %>%
        # Combine elements into a sequence
        summarise(sequence = paste(!!as.name(elements), collapse = ' '),
                  start_time = first(!!as.name(start_time)),
                  end_time = last(!!as.name(end_time))) %>%
        mutate(sequence_identifier = x) %>%
        mutate(duration = .data$end_time - .data$start_time)

      if(length(covariates) > 0) {
        for(i in 1:length(covariates)) {
          sequence_frame <- sequence_frame %>%
            mutate(!!as.name(covariates[i]) := paste(
              unique(
                sequences_long %>%
                  filter(!!as.name(sequence_identifier) == x) %>%
                  pull(!!as.name(covariates[i])) %>%
                  unlist()), collapse = '_'))
        }
      }

      return(sequence_frame)
    }) %>% bind_rows()

  return(sequence_frame)
}
