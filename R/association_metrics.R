#' Calculate Association Metrics for Sequences
#'
#' This function calculates various association metrics for elements in a sequence,
#' such as Pointwise Mutual Information (PMI), normalized PMI, attraction, reliance,
#' Delta P, z-score, t-score, Chi-squared, Jaccard coefficient, Dice coefficient,
#' log odds ratio, and geometric mean.
#'
#' @param sequences A character vector of sequences to analyze.
#' @return A data frame with the calculated association metrics for each dyad (pair of elements).
#' @examples
#' # Example usage:
#' sequences <- c("A B C", "A B", "A C", "B C", "A B C D")
#' result <- association_metrics(sequences)
#' print(result)
#' @import dplyr
#' @import tidyr
#' @import tidytext
#' @importFrom tibble enframe
#' @export

association_metrics <- function(sequences) {

  # Convert sequences to a data frame
  transition_df <- sequences %>%
    # Create a data frame
    enframe(name = NULL, value = "sequence") %>%
    mutate(id = row_number()) %>%
    # Unnest the elements
    unnest_tokens(output = .data$element, input = .data$sequence, token = "words", to_lower = FALSE, drop = FALSE) %>%
    # Group by sequence and calculate the lag
    group_by(.data$id) %>%
    mutate(previous_element = lag(.data$element)) %>%
    # Drop the NA values
    drop_na() %>%
    # Count the transitions
    count(.data$previous_element, .data$element)

  # Count the occurrences of each element
  element_counts <-
    sequences %>%
    str_split(' ') %>%
    unlist() %>%
    table() %>%
    as.data.frame()

  colnames(element_counts) <- c("element", "element_count")

  # Count the occurrences of each dyad
  dyad_counts <- transition_df %>%
    group_by(.data$element, .data$previous_element) %>%
    summarise(count = sum(.data$n), .groups = 'keep') %>%
    ungroup() %>%
    left_join(element_counts, by = c("element" = "element")) %>%
    left_join(element_counts, by = c("previous_element" = "element"))

  # Calculate the total number of dyads
  total_dyads <- nrow(transition_df)

  # A and B
  val_a <- dyad_counts$count
  # A not B
  val_b <- dyad_counts$element_count.y - dyad_counts$count
  # B not A
  val_c <- dyad_counts$element_count.x - dyad_counts$count
  # not A not B
  val_d <- (transition_df$n %>% sum()) - dyad_counts$element_count.y - dyad_counts$element_count.x

  # A expected
  val_a_expected <- ((val_a + val_b)*(val_a + val_c))/(val_a + val_b + val_c + val_d)
  # D expected
  val_d_expected <- ((val_d + val_b)*(val_d + val_c))/(val_a + val_b + val_c + val_d)

  # Calculate the Pointwise Mutual Information (PMI)
  associations <- dyad_counts %>%
    # conditional probability
    mutate(
      conditional_probability = val_a/dyad_counts$element_count.y,
      # pmi
      pmi = log(val_a / val_a_expected),
      # normalised pmi
      pmi_normalised = .data$pmi/(-1 * log(val_a/(val_a + val_b + val_c + val_d))),
      # attraction
      attraction = (100 * val_a)  / (val_a + val_c),
      # Reliance
      reliance = (100 * val_a)  / (val_a + val_b),
      # Delta P
      delta_P = (val_a/ (val_a + val_c)) - (val_b/ (val_b + val_d)),
      # z-score
      z = (val_a - val_a_expected)/sqrt(val_a_expected),
      # t-score
      t = (val_a - val_a_expected)/sqrt(val_a),
      # Chisq
      Chisq = (val_a + val_b + val_c + val_d)*
        ((val_a - val_a_expected)^2)/
        (val_a_expected * val_d_expected),
      # Jaccard coefficient
      jaccard_coefficient = val_a/(val_a + val_b + val_c),
      # Dice coefficient
      dice_coefficient = (2 * val_a)/(2 * val_a + val_b + val_c),
      # Log odds ratio
      logOR = log((val_a * val_d) / (val_b*val_c)),
      # Geometric mean
      gmean = val_a/(sqrt((val_a + val_b) * (val_a + val_c)))
    )

  return(associations)
}
