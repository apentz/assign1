#' @title Long to Wide
#' @description Takes a long dataset and transforms it into a wide dataset
#' @details The input dataset contains 3 columns for User ID's, Item ID's and
#'   Ratings respectively. The output dataset is a wide matrix that contains each unique user as rownames and each unique item as column
#'   names with rating values populating the matrix values. The User ID column has been removed from the matrix to prepare for the format required by the
#'   collaborative filtering functions namely, user based, item based and matrix factorisation.
#' @import dplyr
#' @import tidyr
#' @export long_to_wide
#' @name long_to_wide
#' @return The output dataset is a wide matrix that contains each unique user as rownames and each unique item as column
#'   names with rating values populating the matrix values
#'
#' @param ratings_long The input dataset contains 3 columns for User ID, Item
#' @param fill Missing values are populated either with zeros (fill = 0) or NAs (fill = NA)
#'
long_to_wide <- function(ratings_long, fill){

  # reshape the data from long to wide format
  ratings_wide <- ratings_long %>%
    spread(key = colnames(ratings_long[2]), value = colnames(ratings_long[3]), fill = fill) # pivot the data, items become columns

  # convert to matrix; convert first column to rownames
  users <- as.character(unlist(ratings_wide[,1]))  # save the Users
  ratings_wide <- as.matrix(ratings_wide[,-1])     # convert to matrix and remove the UserID column
  row.names(ratings_wide) <- users                 # replace the rownames with User IDs

  return(ratings_wide)

}
