#' @title Calculation of Cosine Similarity Scores
#' @description Calculates cosine similarities between either users eg. readers
#'   of the books that were rated or people who watched the movies or items eg.
#'   books or movies.
#' @details The input dataset is a wide matrix which contains unique User ID's
#'   as row names and unique Item ID's as column names with ratings as the
#'   values in the matrix. The output dataset is a wide matrix that contains
#'   similarity scores between each unique User and Item combination.
#'   Calculating cosine similarities can quickly become computationally
#'   intensive so a package called coop was imported for this purpose because it
#'   optimises computational performance (reduces computation time). For more
#'   details, please refer to the documentation in the coop package
#' @import coop
#' @import dplyr
#' @export cosine_similarities
#' @name cosine_similarities
#' @return The output dataset is a wide matrix that contains cosine similarity
#'   scores between each unique User and Item combination. A cosine similarity
#'   score of 1 indicated that users or items are similar to each other and a
#'   cosine similarity score of 0 indicates that users or items are differnt
#'   from each other.
#' @examples
#' cosine_similarities_user <- cosine_similarities(book_ratings, method = "user")
#' cosine_similarities_item <- cosine_similarities(book_ratings, method = "item")
#'
#' @param ratings_wide The input dataset is a wide matrix which contains unique
#'   User ID's as row names and unique Item ID's as column names with ratings as
#'   the values in the matrix.
#' @param method Set method = "user" to return cosine similarities between rows
#'   (users) eg. readers who gave similar books similar ratings would have
#'   cosine similarities closer to 1 and readers who give different ratings to
#'   similar books or who rated a different set of books have a cosine
#'   similarity closer to zero. Set method = "item" to return cosine
#'   similarities between columns (items) eg. users that gave movies similar
#'   ratings are considered similar and cosine similarity is closer to 1 and
#'   users that gave movies different ratings are considered different from each
#'   other and cosine similarity is closer to 0
#'
#'
cosine_similarities <- function(ratings_wide, method){

  if(method == "user"){
    # function if method = "user"
    user_similarities <- ratings_wide %>%
      coop::tcosine() %>%         # the tcosine function calculates cosine similarity between rows (users)
      base::diag() <- 0 %>%       # set diagonal to zero to avoid returning similarity to self
        return(user_similarities)
  } else if (method == "item") {
    # function if method = "item"
    item_similarities <- ratings_wide %>%
      coop::cosine() %>%          # the cosine function calculates cosine similarity between columns (items)
      base::diag() <- 0 %>%       # set diagonal to zero to avoid returning similarity to self
        return(item_similarities)
  } else {
    # return an error
    print(" Error. Set method = 'user' or method = 'item' ")
  }

}
