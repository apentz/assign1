#' @title Matrix Factorisation Predictions
#' @description Use the decomposed matrix to predict ratings
#' @details The predictions can be used to calculate the error and accuracy of the model's predictions on the training and or test (validation sets)..
#' @importFrom NNLM nnmf
#' @export mf_predict
#' @name mf_predict
#' @return The output is the predictions of the chosen model
#' @import dplyr
#' @param model
#'
#'

mf_predict <- function(model){

  # generate predictions
  predict <- model$W %*% model$H

}
