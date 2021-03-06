% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_to_matrix.R
\name{convert_to_matrix}
\alias{convert_to_matrix}
\title{Convert to Matrix of Ratings}
\usage{
convert_to_matrix(ratings_wide)
}
\arguments{
\item{ratings_wide}{The input dataset is a wide matrix that contains User ID's in the
first column and 1 Item ID in each column from column 2 onwards.}
}
\value{
The output dataset is a wide matrix that contains each unique user as
  rownames and each unique item as column names with rating values populating
  the matrix values
}
\description{
Takes a wide dataset and transforms it into a matrix of ratings
}
\details{
The input dataset is a wide matrix that contains User ID's in the
  first column and 1 Item ID in each column from column 2 onwards. The output
  dataset is a wide matrix. The User ID column has been removed from the matrix to prepare for
  the format required by the collaborative filtering functions namely, user
  based, item based and matrix factorisation.
}
