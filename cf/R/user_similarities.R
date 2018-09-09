#' @title Cosine Similarity between Users
#' @description Calculates cosine similarity scores between all User pairs
#' @details The input dataset is a wide matrix which contains unique User ID's
#'   as row names and unique Item ID's as column names with ratings as the
#'   values in the matrix. The output dataset is a wide matrix that contains
#'   similarity scores between each pair of Users. Whilst there are many
#'   functions in other packages that can perform the cosine similarity
#'   calculation, such as lsa::cosine, text2vec::sim2, 1-stylo::dist.cosine or
#'   one could write a function using the crossprod function, calculating cosine
#'   similarities can quickly become computationally expensive and slow to run.
#'   The cf package therefore imports a package called \code{coop} which
#'   optimises computational performance and builds on the coop::cosine and
#'   coop::tcosine functions. For more details, please refer to the
#'   documentation in the coop package.
#' @import coop
#' @export user_similarities
#' @name user_similarities
#' @return The output dataset is a wide matrix that contains cosine similarity
#'   scores between each User and Item combination. A cosine similarity
#'   score of 1 indicates that Users are similar to each other and a
#'   cosine similarity score of 0 indicates that Users are dissimilar (different
#'   from each other) with numbers between 0 and 1 depicting relative similarity of Users.
#' @examples
#' user_based <- user_similarities(book_ratings)
#'
#' @param ratings_wide The input dataset is a wide matrix which contains unique
#'   User ID's as row names and unique Item ID's as column names with ratings as
#'   the values in the matrix.
#'
#'
user_similarities <- function(ratings_wide){
  user_similarities <- coop::tcosine(ratings_wide)  # the tcosine function calculates cosine similarity between rows (users)
  diag(user_similarities) <- 0                      # set diagonal to zero to avoid returning similarity to self
  return(user_similarities)
}
