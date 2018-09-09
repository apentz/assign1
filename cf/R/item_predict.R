#' @title Generate Predictions for 1 User
#' @description Generate Predictions for 1 User
#' @import coop
#' @export item_predict
#' @name item_predict
#' @return The output dataset is a wide matrix that contains cosine similarity
#'   scores between each User and Item combination. A cosine similarity score of
#'   1 indicates that Items are similar to each other and a cosine similarity
#'   score of 0 indicates that Items are dissimilar (different from each other)
#'   with numbers between 0 and 1 depicting relative similarity of Items. Items
#'   are said to be similar if many Users have 'used' the Item.
#'
#' @param user An existing or new user denoted by their unique User.ID
#' @param item_similarities A wide matrix that contains cosine similarity scores
#'   between each User and Item combination. A cosine similarity score of 1
#'   indicates that Items are similar to each other and a cosine similarity
#'   score of 0 indicates that Items are dissimilar (different from each other)
#'   with numbers between 0 and 1 depicting relative similarity of Items. Items
#'   are said to be similar if many Users have 'used' the Item.
#' @param ratings_wide_read A wide matrix which contains unique User ID's as row
#'   names and unique Item ID's as column names with ratings as the values in
#'   the matrix where 0 means not read and 1 means read.
#' @param predict_n The number of predictions to generate for the specified
#'   user.
#'
#'
item_predict <- function(user, item_similarities, ratings_wide_read, predict_n){

  # turn into character if not already
  user <- ifelse(is.character(user), user, as.character(user))

  # get scores
  user_read <- row.names(item_similarities)[ratings_wide_read[user,] == TRUE]
  user_scores <- tibble(title = row.names(item_similarities),
                        score = apply(item_similarities[,user_read], 1, sum),
                        read = ratings_wide_read[user,])

  # sort unseen movies by score and remove the 'seen' column
  user_scores <- user_scores %>%
    filter(read == 0) %>%
    arrange(desc(score)) %>%
    select(-read)

  return(user_scores[1:predict_n,])

}
