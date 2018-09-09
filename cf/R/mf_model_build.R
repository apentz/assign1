#' @title Matrix Factorisation Model Build
#' @description Build the matrix factorisation model
#' @details The input dataset is a wide matrix, let's call it `A`. The Matrix
#'   Factorisation Technique takes in a matrix `A` and decomposes it into the
#'   product of 2 matrices, `W` and `H` such that `W` %*% `H` = `A`. The output
#'   is the model specified which can then be used to either assess model
#'   accuracy (see the `mf_model_rmse` function) or to predict ratings (see the
#'   `mf_model_predict` function).
#' @import dplyr
#' @import NNLM
#' @export mf_model_build
#' @name mf_model_build
#' @return The output is the predictive model which can then be used to make
#'   predictions and calculate the accuracy as measured by the RMSE..
#'
#' @param train_wide The training dataset including rows with NAs for testing.
#' @param k_latent The number of latent factors to add. This corresponds to the
#'   number of columns in the W matrix and the number of rows in the H matrix
#'
#' @param alpha L2 regularisation for Users (rows) of A
#' @param beta L2 regularisation for Items (columns) of A
#' @param bias Set bias = "TRUE" to add or "FALSE" to omit
#'
mf_model_build <- function(train_wide, k_latent, alpha, beta, bias){

  if(bias == "TRUE"){

    # build model with bias
    model <- NNLM::nnmf(train_wide, method = 'scd', loss = 'mse', check.k = FALSE,
                        k = k_latent, alpha = alpha, beta = beta,
                        init = list( H0 = matrix(1, nrow = 1, ncol = ncol(train_wide)),
                                     W0 = matrix(1, nrow = nrow(train_wide), ncol = 1) ))
    return(model)
  } else if (bias == "FALSE") {

    # build model without bias
    model <- NNLM::nnmf(train_wide, method = 'scd', loss = 'mse', check.k = FALSE,
                        k = k_latent, alpha = alpha, beta = beta)
    return(model)
  } else

    print("Error: set bias to true to add bias or set bias to false if bias is not required")

}
