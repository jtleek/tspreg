\name{fstats}
\alias{fstats}
\title{Calculate f-statistics quickly}
\usage{
  fstats(dat, mod, mod0)
}
\arguments{
  \item{dat}{An input data set}

  \item{mod}{The alternative model to be fit}

  \item{mod0}{The null model to be fit}
}
\value{
  fstats A vector of f-statistics
}
\description{
  Using matrix algebbra, quickly calculate f statistics for
  comparing nested models
}

