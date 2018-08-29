#' @title Matrix Factorisation Model Build
#' @description Build the matrix factorisation model
#' @details The input dataset is a long matrix which contains User ID's, Item ID's and Ratings in the first 3 columns respectively.
#'   The output is the model specified which can then be used to predict ratings..
#' @import dplyr
#' @importFrom NNLM nnmf
#' @export mf_model_build
#' @name mf_model_build
#' @return The output is the predictive model which can then be used to make predictions and calculate the accuracy as measured by the RMSE..
#'
#' @param train_wide The input dataset is a long matrix which contains User ID's, Item ID's and Ratings in the first 3 columns respectively.
#' @param k_latent Set method = "user" to return cosine similarities between rows (users) eg. readers who gave similar books similar ratings would have cosine similarities closer to 1 and readers who give different ratings to similar books or who rated a different set of books have a cosine similarity closer to zero. Set method = "item" to return cosine similarities between columns (items) eg. users that gave movies similar ratings are considered similar and cosine similarity is closer to 1 and users that gave movies different ratings are considered different from each other and cosine similarity is closer to 0
#'
#'

mf_model_build <- function(train_wide, k_latent, alpha, beta, bias){

  if(bias == TRUE){

    # build model with bias
    model <- NNLM::nnmf(train_wide, method = 'scd', loss = 'mse', check.k = FALSE,
                        k_latent = k_latent, alpha = alpha, beta = beta,
                        init = list( W0 = matrix(1, nrow = 1, ncol = nrow(train_wide)),
                                     W1 = matrix(1, nrow = ncol(train_wide), ncol = 1) ))
  } else if (bias == FALSE) {

    # build model with bias
    model <- NNLM::nnmf(train_wide, method = 'scd', loss = 'mse', check.k = FALSE,
                        k_latent = k_latent, alpha = alpha, beta = beta)

  } else

    print("Error: set bias = TRUE to add bias or set bias = FALSE if bias is not required")

}
