#' @title Generate Predictions for 1 User
#' @description Generate Predictions for 1 User
#' @import dplyr
#' @export mf_recommend
#' @name mf_recommend
#' @return The output dataset is a wide matrix that contains predictions of book
#'   ratings for users using the matrix factorisation method..
#'
#' @param user An existing or new user denoted by their unique User.ID
#' @param predictions A wide matrix that contains predictions using the matrix
#'   factorisation method. This is the output to the `mf_predict` function.
#' @param ratings_wide A wide matrix which contains unique User ID's as row
#'   names and unique Item ID's as column names with ratings as the values in
#'   the matrix. This is used to see whether a user has already 'used' an item
#'   and exclude those from predictions.
#' @param predict_n The number of predictions to generate for the specified
#'   user.
#'
mf_recommend <- function (user, predictions, ratings_wide, predict_n) {

  # turn into character if not already
  user <- ifelse(is.character(user), user, as.character(user))

  # get predictions
  user_predictions <- data.frame(title = colnames(ratings_wide),
                                 score = as.vector(predictions[user,]),
                                 used = ratings_wide[user,])

  # sort 'unused' items by score and remove the 'used' column
  user_predictions <- user_predictions %>%
    filter(is.na(used) == TRUE) %>%
    arrange(desc(score)) %>%
    select(-used)

  return(user_predictions[1:predict_n,])

}
