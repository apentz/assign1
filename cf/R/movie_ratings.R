#' @title Movie Ratings Data
#' @description 20 movies seen or not seen by 15 movie-goers
#' @details This is a modified version of the "Movie Lens" dataset. The .RData file
#'   contains a small subset of 15 movie-goers (Users) and 20 movies (Items) and whether they have
#'   seen (rating=1) or not seen (rating=0) each movie. The full dataset has
#'   ratings on 40 000 movies by 260 000 users, some 24 million ratings in all.
#'   There are four csv files: `links.csv`, `movies.csv`, `ratings.csv`, and
#'   `tags.csv`.
#' @name movie_ratings
#' @docType data
#' @usage movie_ratings
#' @format tibble
#'
#'
# load data
load("./data/movie_ratings.RData")
# save as .RData
save(movie_ratings, file = "data/movie_ratings.RData")
