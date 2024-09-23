#' Calculate the Average Length of Sequences
#'
#' This function calculates the average length of a sequence of elements, where each sequence is split by spaces.
#'
#' @param sequences A character vector where each element is a sequence of elements separated by spaces.
#'
#' @return A numeric value representing the average length of the sequences.
#'
#' @examples
#' sequences <- c('hello world', 'hello world hello', 'hello world hello world')
#' average_seq_length(sequences)
#' @importFrom stats sd median
#' @export
average_seq_length <- function(sequences){
  return(mean(str_count(sequences, ' ') + 1))
}


#' Calculate the Standard Deviation of Sequence Lengths
#'
#' This function calculates the standard deviation of the lengths of sequences of elements, where each sequence is split by spaces.
#'
#' @param sequences A character vector where each element is a sequence of elements separated by spaces.
#'
#' @return A numeric value representing the standard deviation of the lengths of the sequences.
#'
#' @examples
#' sequences <- c('hello world', 'hello world hello', 'hello world hello world')
#' sd_seq_length(sequences)
#' @export
sd_seq_length <- function(sequences){
  return(sd(str_count(sequences, ' ') + 1))
}


#' Calculate the Median Length of Sequences
#'
#' This function calculates the median length of a sequence of elements, where each sequence is split by spaces.
#'
#' @param sequences A character vector where each element is a sequence of elements separated by spaces.
#'
#' @return A numeric value representing the median length of the sequences.
#'
#' @examples
#' sequences <- c('hello world', 'hello world hello', 'hello world hello world')
#' median_seq_length(sequences)
#' @export
median_seq_length <- function(sequences){
  return(median(str_count(sequences, ' ') + 1))
}


#' Calculate the Minimum and Maximum Length of Sequences
#'
#' This function calculates the minimum and maximum length of sequences of elements, where each sequence is split by spaces.
#'
#' @param sequences A character vector where each element is a sequence of elements separated by spaces.
#'
#' @return A numeric vector of length 2, with the minimum and maximum lengths of the sequences.
#'
#' @examples
#' sequences <- c('hello world', 'hello world hello', 'hello world hello world')
#' min_max_seq_length(sequences)
#' @export
min_max_seq_length <- function(sequences){
  return(c(min(str_count(sequences, ' ') + 1), max(str_count(sequences, ' ') + 1)))
}

#' Count Distinct Elements in Sequences
#'
#' This function counts the number of distinct elements across all sequences, where each sequence is split by spaces.
#'
#' @param sequences A character vector where each element is a sequence of elements separated by spaces.
#'
#' @return An integer representing the number of distinct elements across all sequences.
#'
#' @examples
#' sequences <- c('hello world', 'hello world hello', 'hello world hello world')
#' count_distinct_elements(sequences)
#' @export
count_distinct_elements <- function(sequences){
  return(length(unique(unlist(str_split(sequences, ' ')))))
}

#' Count Distinct Elements per List Item
#'
#' This function calculates the average number of distinct elements per item in a list of sequences, where each sequence is split by spaces.
#'
#' @param sequences A list of character vectors, where each vector contains sequences of elements separated by spaces.
#'
#' @return A numeric value representing the average number of distinct elements per list item.
#'
#' @examples
#' sequences <- c('hello world', 'hello world hello', 'hello world hello world')
#' count_distinct_elements_per_list_item(sequences)
#' @export
count_distinct_elements_per_list_item <- function(sequences){
  return(mean(sapply(sequences, function(x) length(unique(unlist(str_split(x, ' ')))))))
}


#' Count Distinct Elements per List Item in Shuffled Sequences
#'
#' This function calculates the number of distinct elements per list item in a list of sequences shuffled using the 'shuffle_sequences_across' function. The shuffling is performed a specified number of times.
#'
#' @param sequences A list of character vectors, where each vector contains sequences of elements separated by spaces.
#' @param iterations An integer specifying the number of shuffling iterations.
#'
#' @return A numeric vector of length `iterations`, each element representing the number of distinct elements per list item in a shuffled sequence.
#'
#' @examples
#' sequences <- c('hello world', 'hello world hello', 'hello world hello world')
#' count_distinct_elements_per_list_item_shuffled(sequences, iterations = 100)
#' @export
count_distinct_elements_per_list_item_shuffled <- function(sequences, iterations = 100){
  return(sapply(1:iterations, function(x) count_distinct_elements_per_list_item(shuffle_sequences_across(sequences))))
}


#' Compare True and Shuffled Distinct Elements per List Item
#'
#' This function compares the true number of distinct elements per list item in a list of sequences to the number of distinct elements per list item in shuffled sequences. The comparison is done by calculating p-values from shuffled sequences.
#'
#' @param sequences A list of character vectors, where each vector contains sequences of elements separated by spaces.
#' @param iterations An integer specifying the number of shuffling iterations.
#'
#' @return A data frame with columns:
#' \item{true_distinct_elements}{The number of distinct elements per list item in the original sequences.}
#' \item{shuffled_distinct_elements}{The average number of distinct elements per list item in shuffled sequences.}
#' \item{p_value}{The p-value representing the proportion of shuffled sequences where the number of distinct elements is less than or equal to the true number.}
#'
#' @examples
#' sequences <- c('hello world', 'hello world hello', 'hello world hello world', 'hello world')
#' compare_distinct_elements_per_list_item(sequences, iterations = 100)
#' @export
compare_distinct_elements_per_list_item <- function(sequences, iterations = 100){
  return(data.frame(
    true_distinct_elements = count_distinct_elements_per_list_item(sequences),
    shuffled_distinct_elements = mean(count_distinct_elements_per_list_item_shuffled(sequences, iterations)),
    p_value = mean(count_distinct_elements_per_list_item_shuffled(sequences, iterations) <= count_distinct_elements_per_list_item(sequences))
  ))
}

#' Summarize Sequence Lengths
#'
#' This function calculates summary statistics for the lengths of sequences of elements, including mean, standard deviation, median, minimum, and maximum lengths. It also counts the number of distinct elements and compares this to shuffled sequences.
#'
#' @param sequences A character vector where each element is a sequence of elements separated by spaces.
#'
#' @return A data frame with the following columns:
#' \item{mean_seq_elements}{The mean length of the sequences.}
#' \item{sd_seq_elements}{The standard deviation of the sequence lengths.}
#' \item{median_seq_elements}{The median length of the sequences.}
#' \item{min_seq_elements}{The minimum length of the sequences.}
#' \item{max_seq_elements}{The maximum length of the sequences.}
#' \item{distinct_elements}{The number of distinct elements across all sequences.}
#' \item{pvalue_distinct_elements}{The p-value comparing the true number of distinct elements to shuffled sequences.}
#'
#' @examples
#' sequences <- c('hello world', 'hello world hello', 'hello world hello world')
#' sequence_length_summary(sequences)
#' @export
sequence_length_summary <- function(sequences){
  return(data.frame(
    mean_seq_elements = average_seq_length(sequences),
    sd_seq_elements = sd_seq_length(sequences),
    median_seq_elements = median_seq_length(sequences),
    min_seq_elements = min_max_seq_length(sequences)[1],
    max_seq_elements = min_max_seq_length(sequences)[2],
    distinct_elements = count_distinct_elements(sequences),
    pvalue_distinct_elements = compare_distinct_elements_per_list_item(sequences)$p_value
  ))
}
