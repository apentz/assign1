#' @title Cosine Similarity between Items
#' @description Calculates cosine similarity scores between all Item pairs
#' @details The input dataset is a wide matrix which contains unique User ID's
#'   as row names and unique Item ID's as column names with ratings as the
#'   values in the matrix. The output dataset is a wide matrix that contains
#'   similarity scores between each pair of Items. Whilst there are many
#'   functions in other packages that can perform the cosine similarity
#'   calculation, such as lsa::cosine, text2vec::sim2, 1-stylo::dist.cosine or
#'   one could write a function using the crossprod function, calculating cosine
#'   similarities can quickly become computationally expensive and slow to run.
#'   The cf package therefore imports a package called \code{coop} which
#'   optimises computational performance and builds on the coop::cosine and
#'   coop::tcosine functions. For more details, please refer to the
#'   documentation in the coop package.
#' @import coop
#' @export item_similarities
#' @name item_similarities
#' @return The output dataset is a wide matrix that contains similarity scores
#'   between each pair of Items. A cosine similarity score of 1 indicates that
#'   Items are similar to each other and a cosine similarity score of 0
#'   indicates that Items are dissimilar (different from each other) with
#'   numbers between 0 and 1 depicting relative similarity of Items.
#' @examples
#' user_based <- similarities_items(book_ratings)
#'
#' @param ratings_wide The input dataset is a wide matrix which contains unique
#'   User ID's as row names and unique Item ID's as column names with ratings as
#'   the values in the matrix.
#'
#'
item_similarities <- function(ratings_wide){
  item_similarities <- coop::cosine(ratings_wide)  # the cosine function calculates cosine similarity between columns (items)
  diag(item_similarities) <- 0                     # set diagonal to zero to avoid returning similarity to self
  return(item_similarities)
}
