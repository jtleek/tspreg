\name{calculateTspairs}
\alias{calculateTspairs}
\title{Calculate pairwise comparison matrix for a set of top-scoring pairs}
\usage{
  calculateTspairs(dat, index)
}
\arguments{
  \item{dat}{An input data set}

  \item{index}{An index output from the function
  \code{tspreg}}
}
\value{
  pairMat A matrix with pairwise comparisons in rows.

  probeName The names of the probes (rows) being compared
  in the data set
}
\description{
  Create comparison variables between all rows of a data
  matrix that participate in a top-scoring pair
}

