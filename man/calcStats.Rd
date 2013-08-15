\name{calcStats}
\alias{calcStats}
\title{Fast calculation of marginal stats}
\usage{
  calcStats(X, pheno, type = "factor")
}
\arguments{
  \item{X}{An input data set}

  \item{pheno}{A factor or survival outcome}

  \item{The}{type of analysis to be performed}
}
\value{
  Test statistics (F-stats or Wald Stats from coxph model)
}
\description{
  Rapidly calculate statistics for the relationship between
  the rows of X and the phenotype (factor or survival)
}

