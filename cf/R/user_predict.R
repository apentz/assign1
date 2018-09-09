#' @title Generate Predictions for 1 User
#' @description Generate Predictions for 1 User
#' @import coop
#' @export user_predict
#' @name user_predict
#' @return The output dataset is a wide matrix that contains cosine similarity
#'   scores between each User and Item combination. A cosine similarity score of
#'   1 indicates that Users are similar to each other and a cosine similarity
#'   score of 0 indicates that Users are dissimilar (different from each other)
#'   with numbers between 0 and 1 depicting relative similarity of Users.
#'
#' @param user An existing or new user denoted by their unique User.ID
#' @param user_similarities A wide matrix that contains cosine similarity scores
#'   between each User and Item combination. A cosine similarity score of 1
#'   indicates that Users are similar to each other and a cosine similarity
#'   score of 0 indicates that Users are dissimilar (different from each other)
#'   with numbers between 0 and 1 depicting relative similarity of Users.
#' @param ratings_wide A wide matrix which contains unique User ID's as row
#'   names and unique Item ID's as column names with ratings as the values in
#'   the matrix.
#' @param predict_n The number of predictions to generate for the specified
#'   user.
#' @example user_predict_test <- user_predict(user = "23872", user_similarities
#'   = user_similarities_books, ratings_wide = book_ratings_wide, predict_n =
#'   10)
#'
#'
user_predict <- function (user, user_similarities, ratings_wide, predict_n) {

  # turn into character if not already
  user <- ifelse(is.character(user), user, as.character(user))

  # get scores
  user_scores <- data.frame(title = colnames(ratings_wide),
                            score = as.vector(user_similarities[user,] %*% ratings_wide),
                            used = ratings_wide[user,])

  # sort 'unused' items by score and remove the 'used' column
  user_scores <- user_scores %>%
    filter(used == 0) %>%
    arrange(desc(score)) %>%
    select(-used)

  return(user_scores[1:predict_n,])

}

