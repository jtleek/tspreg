#' 
#' Peform top scoring pairs regression
#' 
#' This function calculates a top-scoring pairs regression model
#' which can be used for prediction in high-dimensional problems. 
#' 
#' @param dat An input data set
#' @param outcome The outcome that you are trying to predict (may be factor or survival)
#' @param npair The desired number of top scoring pairs to calculate
#' @param nvars The algorithm first screens for marginal association
#'  between outcome and dat and selects the top nvars to use to create candidate pairs
#' 
#' @export
#' 
#' @return An index vector for the top-scoring pairs
#' @return The test statistics for the association between each pair and the outcome 

tspreg <- function(dat,outcome,npair=10,nvars=1e4){
  m <- dim(dat)[1]; n <- dim(dat)[2]; ind <- 1:m
  type <- class(outcome)
  if(nvars < m){
    tmpStats <- abs(calcStats(dat,outcome,type))
    ind <- ind[rank(-tmpStats) <= nvars]
    dat <- dat[ind,]
    m <- dim(dat)[1]
  }
  pairMat <- calcPairs(dat)
  
  ind1 <- rep(1:m,m); ind2 <- rep(1:m,each=m)
  index <- paste(rep(ind,m),rep(ind,each=m),sep="<")
  index <- index[ind1 < ind2]
  
  stats <- calcStats(pairMat[ind1 < ind2,],outcome,type=type)
  oo <- order(-abs(stats))[1:npair]
  return(list(index=index[oo],stats=stats[oo]))
}






