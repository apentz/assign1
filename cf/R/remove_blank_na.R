#' @title Remove Missing Values or NAs
#' @description Takes a long dataset and removes implicit ratings (not rated ie.
#'   rating value = zero) and NA values if applicable
#' @details Implicit ratings are those that occur in a dataset with a value of
#'   zero eg. the movie has been seen but not rated or the book has been read
#'   but not rated. In user based and item based collaborative filtering it is
#'   necessary to remove the zero ratings because missing values are replaced
#'   with zeros in order for the cosine similarity calculation to work. In
#'   contrast, the matrix factorisation technique replaces missing values with
#'   NAs so that the model doesn't learn to predict a zero but learns a more
#'   appropriate and useful value
#' @import dplyr
#' @export remove_blank_na
#' @name remove_blank_na
#' @param ratings_long The input dataset contains 3 columns for User ID, Item ID
#'   and Rating respectively
#' @return A long dataset without zero ratings and without NA ratings
#' @name remove_blank_na
#'
#'

remove_blank_na <- function(ratings_long){

  ratings_long <- ratings_long %>%
    filter(ratings_long[,3] %in% c(1:10)) %>%     # remove implicit ratings ie. not rated (value=0) and NA values

    return(ratings_long)

}

