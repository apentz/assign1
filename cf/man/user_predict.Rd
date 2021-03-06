% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/user_predict.R
\name{user_predict}
\alias{user_predict}
\title{Generate Predictions for 1 User}
\usage{
user_predict(user, user_similarities, ratings_wide, predict_n)
}
\arguments{
\item{user}{An existing or new user denoted by their unique User.ID}

\item{user_similarities}{A wide matrix that contains cosine similarity scores
between each User and Item combination. A cosine similarity score of 1
indicates that Users are similar to each other and a cosine similarity
score of 0 indicates that Users are dissimilar (different from each other)
with numbers between 0 and 1 depicting relative similarity of Users.}

\item{ratings_wide}{A wide matrix which contains unique User ID's as row
names and unique Item ID's as column names with ratings as the values in
the matrix.}

\item{predict_n}{The number of predictions to generate for the specified
user.}
}
\value{
The output dataset is a wide matrix that contains cosine similarity
  scores between each User and Item combination. A cosine similarity score of
  1 indicates that Users are similar to each other and a cosine similarity
  score of 0 indicates that Users are dissimilar (different from each other)
  with numbers between 0 and 1 depicting relative similarity of Users.
}
\description{
Generate Predictions for 1 User
}
