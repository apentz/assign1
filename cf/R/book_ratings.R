#' @title Book Ratings Data
#' @description Ratings (on a scale of 0 to 10) of 150 books from 10,000 users.
#' @details This is a modified version of the “Book-Crossings” dataset. The .RData
#'   file contains the following three objects:
#'   \itemize{
#'   \item{book_ratings:}
#'   {data frame containing unique ID variable for identifying readers
#'   (\code{User.ID}, a unique ID variable for identifying books (\code{ISBN}), and the book
#'   ratings for each reader-book pair (\code{Book.Rating}). Ratings are either
#'   integers (0 – 10) or NA. You can assume an NA means the book has not been
#'   read}
#'   \item{book_info:} {data frame containing the title (\code{Book.Title}) and
#'   author (Book.Author) for each ISBN}
#'   \item{user_info:} {data frame
#'   containing additional demographic information (\code{Age}) for some users. You
#'   don't need this data to build the recommender but if you want to go a bit
#'   further you can try including this information}
#'   }
#' @name book_ratings
#' @source \href{http://www2.informatik.uni-freiburg.de/~cziegler/BX/}
#' @docType data
#' @usage data(book_ratings)
#' @format 3 dataframes
#'
#'
# load data
load("./data/book_ratings.RData")
# save as .RData
save(book_ratings, book_info, user_info, file = "data/book_ratings.RData")
