#' Transition Predictions
#'
#' This function takes sequences of elements and uses a machine learning classifier to predict the next elements in the sequence.
#' It supports n-gram tokenization and k-fold cross-validation. Optionally, it can upsample the training data.
#'
#' @param sequences A list of character strings representing sequences of elements.
#' @param classifier A character string specifying the classifier to use. Options are 'nb' for Naive Bayes and 'forest' for random forest.
#' @param ngram An integer specifying the number of elements to consider in the n-gram tokenization. Default is 2.
#' @param upsample A logical value indicating whether to upsample the training data to balance class distribution. Default is TRUE.
#' @param k An integer specifying the number of folds for k-fold cross-validation. Default is 10.
#'
#' @return A list containing the mean accuracy, mean null accuracy, and a data frame of prediction errors.
#' @importFrom dplyr select mutate filter row_number sample_frac bind_rows across everything
#' @importFrom tidyr separate unite
#' @importFrom tibble enframe
#' @importFrom tidytext unnest_tokens
#' @importFrom stringr str_c
#' @importFrom ranger ranger
#' @importFrom naivebayes naive_bayes
#' @importFrom stats predict
#' @export
#'
#' @examples
#' sequences <- list("a b c", "b c d", "c d e")
#' result <- transition_predictions(sequences, classifier = 'nb', ngram = 2, upsample = TRUE, k = 5)
#' print(result)

transition_predictions <- function(sequences, classifier = 'nb', ngram = 2, upsample = TRUE, k = 10) {

  dataset <- sequences %>%
    # Create a data frame
    enframe(name = NULL, value = "sequence") %>%
    # Unnest the elements
    unnest_tokens(output = .data$element, input = .data$sequence, token = "ngrams", n = ngram, to_lower = FALSE, drop = FALSE) %>%
    select(.data$element) %>%
    filter(!is.na(.data$element)) %>%
    separate(.data$element, into = str_c('c', 1:ngram), sep = " ", remove = FALSE) %>%
    select(-.data$element) %>%
    unite(col = 'consequent', str_c('c', ngram), sep = ' ', remove = TRUE) %>%
    mutate(consequent = as.factor(.data$consequent))

  # Split dataset into training and testing sets
  dataset <- dataset %>%
    mutate(across(everything(), as.factor)) %>%
    mutate(id = row_number()) %>%
    sample_frac(1)

  # Create k folds
  folds <- cut(seq(1, nrow(dataset)), breaks = k, labels = FALSE)

  # Initialize list to store train and test datasets
  kfold_datasets <- list()

  # Loop over each fold
  for (i in 1:k) {
    test_indices <- which(folds == i, arr.ind = TRUE)
    test_data <- dataset[test_indices, ]
    train_data <- dataset[-test_indices, ]

    kfold_datasets[[i]] <- list(train = train_data %>% select(-.data$id), test = test_data %>% select(-.data$id))
  }

  if (upsample) {
    for (i in seq_along(kfold_datasets)) {
      max_count <- max(table(kfold_datasets[[i]]$train$consequent))
      train <- lapply(unique(kfold_datasets[[i]]$train$consequent), function(x) {
        kfold_datasets[[i]]$train %>%
          filter(.data$consequent == x) %>%
          sample_n(max_count, replace = TRUE)
      }) %>% bind_rows()
      kfold_datasets[[i]]$train <- train
    }
  }

  if(classifier == 'nb'){
    class_predict <- lapply(kfold_datasets, function(x) {
    # Train the Naive Bayes classifier
    classifier_fit <- naive_bayes(consequent ~ ., data = x$train, laplace = 0.01)

    # Use the trained model to predict outcomes for the test data
    model_results <-
      bind_cols(target = as.factor(as.character(x$test$consequent))) %>%
      mutate(.pred_class = as.factor(as.character(predict(classifier_fit, x$test)))) %>%
      suppressWarnings()

    # Calculate the accuracy of the model
    accuracy <- mean(as.character(model_results$target) == as.character(model_results$.pred_class), na.rm = TRUE)

    return(
      list(
        accuracy = accuracy,
        null_accuracy = max(table(x$train$consequent) / nrow(x$train)),
        predictions = model_results
      )
    )})
  }

  if(classifier == 'forest'){
    class_predict <- lapply(kfold_datasets, function(x) {
      # Train the Naive Bayes classifier
      classifier_fit <- ranger(consequent ~ ., data = x$train, num.trees = 1000,verbose = FALSE)

      # Use the trained model to predict outcomes for the test data
      model_results <-
        bind_cols(target = as.factor(as.character(x$test$consequent))) %>%
        mutate(.pred_class = as.factor(as.character(predict(classifier_fit, x$test)$predictions))) %>%
        suppressWarnings()

      # Calculate the accuracy of the model
      accuracy <- mean(as.character(model_results$target) == as.character(model_results$.pred_class), na.rm = TRUE)

      return(
        list(
          accuracy = accuracy,
          null_accuracy = max(table(x$train$consequent) / nrow(x$train)),
          predictions = model_results
        )
      )})
  }

  prediction_error <- lapply(class_predict, function(x) {
    x$predictions
  }) %>% bind_rows()

  return(
    list(
      mean_accuracy = mean(unlist(lapply(class_predict, function(x) x$accuracy))),
      mean_null_accuracy = mean(unlist(lapply(class_predict, function(x) x$null_accuracy))),
      prediction_error = prediction_error
    )
  )
}
