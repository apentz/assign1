#' @title Matrix Factorisation Model Selection with Cross Validation
#' @description Train multiple models by varying parameters and compare RMSE on the training and out of sample (testing) set
#' @details The input dataset is a long matrix which contains User ID's, Item ID's and Ratings in the first 3 columns respectively.
#'   The output is a grid of parameters that were varied and the RMSE on the training and test set respectively.
#' @import dplyr
#' @importFrom NNLM nnmf
#' @export mf_model_selection
#' @name mf_model_selection
#' @return The output dataset is a wide matrix that contains cosine similarity scores between each unique User and Item combination. A cosine similarity score of 1 indicated that users or items are similar to each other and a cosine similarity score of 0 indicates that users or items are differnt from each other.
#' @examples
#' cosine_similarities_user <- cosine_similarities(book_ratings, method = "user")
#'  cosine_similarities_item <- cosine_similarities(users_test, method = "item")
#'
#' @param ratings_long The input dataset is a long matrix which contains User ID's, Item ID's and Ratings in the first 3 columns respectively.
#' @param k_latent Set method = "user" to return cosine similarities between rows (users) eg. readers who gave similar books similar ratings would have cosine similarities closer to 1 and readers who give different ratings to similar books or who rated a different set of books have a cosine similarity closer to zero. Set method = "item" to return cosine similarities between columns (items) eg. users that gave movies similar ratings are considered similar and cosine similarity is closer to 1 and users that gave movies different ratings are considered different from each other and cosine similarity is closer to 0
#'
#'

mf_model_selection <- function(ratings_long, k_latent){

  # split into training and test sets
  train_test_split(ratings_long, 0.8)

  # generate rmse for all values of k specified on traiiing and test sets
    for (k_latent in k_latent){

          # build model
          model[k_latent] <- NNLM::nnmf(train_wide, method = 'scd', loss = 'mse', check.k = FALSE, k_latent=k_latent)

          # generate predictions
          predict[k_latent] <- model$W %*% model$H

          # model accuracy is sum of squared errors
          errors_train <- (train_wide[index,] - predict[index,]) ^ 2     # training set errors
          errors_test <- (test_wide - predict[-index,]) ^ 2              # testing (validation) set errors

          # rmse
          rmse_train[k_latent] <- sqrt(mean(errors_train[!is.na(train_wide)]))
          rmse_test[k_latent] <- sqrt(mean(errors_test[!is.na(test_wide)]))

    }
    return(rmse_train)
    return(rmse_test)

k_rmse <- data.frame(k_latent = k_latent,
                     rmse_train = mf_model_selection(ratings_long),
                     rmse_test = mf_model_selection(ratings_long)[2])

}
