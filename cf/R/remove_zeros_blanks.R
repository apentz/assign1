#' @title Remove Zero and NA Values (blanks)
#' @description Takes a long dataset and removes implicit ratings (not rated ie.
#'   rating value is zero) ratings and NAs (blanks) if applicable
#' @details Implicit ratings are those that occur in a dataset with a value of
#'   zero eg. the movie has been seen but not rated or the book has been read
#'   but not rated.
#'
#'   In user based and item based collaborative filtering it is necessary to
#'   remove the zero ratings because missing values are replaced with zeros in
#'   order for the cosine similarity calculation to work and there should be a
#'   distinction between item not rated and item not read (books) or item not seen (movies)
#'   for example.
#'
#'   In contrast, the matrix factorisation technique replaces missing values
#'   with NAs so that the model doesn't learn to predict a zero but learns a
#'   more appropriate value which is more useful for prediction.
#'
#' @param ratings_long The input dataset contains 3 columns for User ID, Item ID
#'   and Rating respectively
#' @import dplyr
#' @return A long dataset without zero and missing ratings
#' @export remove_zeros_blanks
#'
#' @examples
#'
remove_zeros_blanks <- function(ratings_long){

  ratings_long <- ratings_long %>%
    filter(ratings_long[,3] %in% c(1:10))     # remove implicit ratings ie. not rated (value=0) and NA values

}
