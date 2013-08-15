\docType{package}
\name{tspreg}
\alias{tspreg}
\alias{tspreg-package}
\title{tspreg}
\usage{
  tspreg(dat, outcome, npair = 10, nvars = 10000)
}
\arguments{
  \item{dat}{An input data set}

  \item{outcome}{The outcome that you are trying to predict
  (may be factor or survival)}

  \item{npair}{The desired number of top scoring pairs to
  calculate}

  \item{nvars}{The algorithm first screens for marginal
  association between outcome and dat and selects the top
  nvars to use to create candidate pairs}
}
\value{
  An index vector for the top-scoring pairs

  The test statistics for the association between each pair
  and the outcome
}
\description{
  tspreg

  This function calculates a top-scoring pairs regression
  model which can be used for prediction in
  high-dimensional problems.
}

