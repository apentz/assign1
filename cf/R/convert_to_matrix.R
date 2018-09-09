#' @title Convert to Matrix of Ratings
#' @description Takes a wide dataset and transforms it into a matrix of ratings
#' @details The input dataset is a wide matrix that contains User ID's in the
#'   first column and 1 Item ID in each column from column 2 onwards. The output
#'   dataset is a wide matrix. The User ID column has been removed from the matrix to prepare for
#'   the format required by the collaborative filtering functions namely, user
#'   based, item based and matrix factorisation.
#' @export convert_to_matrix
#' @name convert_to_matrix
#' @return The output dataset is a wide matrix that contains each unique user as
#'   rownames and each unique item as column names with rating values populating
#'   the matrix values
#'
#' @param ratings_wide The input dataset is a wide matrix that contains User ID's in the
#'   first column and 1 Item ID in each column from column 2 onwards.
#'
convert_to_matrix <- function(ratings_wide){
  # convert to matrix; convert first column to rownames
  users <- as.character(unlist(ratings_wide[,1]))  # save the Users
  ratings_wide <- as.matrix(ratings_wide[,-1])     # convert to matrix and remove the UserID column
  row.names(ratings_wide) <- users                 # replace the rownames with User IDs
  return(ratings_wide)
}
