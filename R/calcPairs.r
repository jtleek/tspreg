#' 
#' Calculate pair matrix
#' 
#' Create comparison variables between all rows of
#' a data matrix
#' 
#' @param dat An input data set
#' 
#' @export
#' 
#' @return pairMat A matrix with pairwise comparisons in rows.
#' 


calcPairs <- function(dat){
  m <- dim(dat)[1]; n <- dim(dat)[2]; ind <- 1:m
  pairMat <- matrix(NA,nrow=m*m,ncol=n)
  for(i in 1:m){
    pairMat[(1:m + ((i-1)*m)),] <- t(t(dat) <= dat[i,])
  }
  return(pairMat)
}