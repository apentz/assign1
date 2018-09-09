#' @title Matrix Factorisation Model RMSE for Cross Validation
#' @description Compare RMSE on the training and out of sample (testing) set
#' @details The input datasets are the wide training and the wide testing set.
#'   The output is a dataframe of RMSE scores on the training, testing and
#'   complete set respectively.
#' @import dplyr
#' @import NNLM
#' @export mf_model_rmse
#' @name mf_model_rmse
#' @return The output is a dataframe of RMSE scores on the training, testing and
#'   complete set respectively.
#' @examples
#'
#' @param model The output to the `mf_model_build` function
#' @param train_wide The training dataset including rows with NAs respresenting the testing set.
#' @param test_wide The number of latent factors to add. This corresponds to the
#'   number of columns in the W matrix and the number of rows in the H matrix
#'
#'
mf_model_rmse <- function(model, train_wide, test_wide){

  # generate predictions on the full set
  predict <- model$W %*% model$H

  # create index to apply to wide data, it's now indexing the 20% test set
  set.seed(123)
  index_wide <- which(rownames(train_wide) %in% rownames(test_wide))
  # store train and test errors separately
  errors_train <- (train_wide[-index_wide] - predict[-index_wide]) ^ 2
  errors_test <- (test_wide - predict[index_wide]) ^ 2

  # calculate rmse
  rmse_all <- sqrt(model$mse)[length(model$mse)]      # get the last value from the vector
  rmse_train <- sqrt(mean(errors_train[!is.na(train_wide[-index_wide])],na.rm = TRUE))
  rmse_test <- sqrt(mean(errors_test[!is.na(test_wide)],na.rm = TRUE))

  return(data.frame(rmse_train, rmse_test, rmse_all))
}
