#' @title Training and Testing (Validation) Split
#' @description Randomly splits the dataset into training and test sets using
#'   the proportion specified
#' @details The input dataset contains 3 columns for User ID's, Item ID's and
#'   Ratings respectively. The implicit (zero) ratings should be removed before
#'   splitting into training and testing but this is catered for in this
#'   function in case this is forgotten. The output is 2 datasets, train and
#'   test, both in wide format
#'
#' @param ratings_long The input dataset contains 3 columns for User ID, Item ID
#'   and Rating respectively
#' @param split This is the percentage of the dataset assigned to the training
#'   set eg. 0.8 denotes 80 percent assigned to training and 20 percent assigned
#'   to testing
#' @export train_test_split
#' @name train_test_split
#' @return The output is 2 datasets, train and test, in wide format
#'
#'

train_test_split <- function(ratings_long, split){

  # split into train and test sets
  index <- sample(seq_len(nrow(ratings_long)), size = round(nrow(ratings_long)*split,0))
  train <- ratings_long[index,]    # train the model on 80% of the observed values
  test <- ratings_long[-index,]    # hold out 20% to assess prediction accuracy on the test set
  blanks <- test                   # create a copy of the test ratings
  blanks[,3] <- NA                 # remove the test ratings and replace with NA (treated like blanks)
  train <- rbind(train,blanks)     # combine NAs with the train set to preserve matrix size

  train_wide <- cf_long_to_wide(train, NA)  # convert training set from long to wide format
  test_wide <- cf_long_to_wide(test, NA)    # convert testing set from long to wide format

  return(list(train_wide, test_wide))

}
