


#' @title Remove Zero and Missing Values (blanks)
#' @description Takes a long dataset and removes implicit (zero) ratings and blanks if applicable
#'
#' @param ratings_long The input dataset contains 3 columns for User ID, Item ID
#'   and Rating respectively
#'
#' @return A long dataset without zero and missing ratings
#' @export
#'
#' @examples
#'
remove_zeros_blanks <- function(ratings_long){

  ratings_long <- ratings_long %>%
    filter(ratings_long[,3] %in% c(1:10)) %>%

    return(ratings_long)

}
