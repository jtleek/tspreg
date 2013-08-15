#' 
#' Calculate pairwise comparison matrix for a set of top-scoring pairs
#' 
#' Create comparison variables between all rows of
#' a data matrix that participate in a top-scoring pair
#' 
#' 
#' @param dat An input data set
#' @param index An index output from the function \code{tspreg}
#' 
#' @export
#' 
#' @return pairMat A matrix with pairwise comparisons in rows.
#' @return probeName The names of the probes (rows) being compared in the data set
#' 

calculateTspairs <- function(dat,index){
  
  pairMat <- matrix(NA,ncol=length(index),nrow=ncol(dat))
  probeName <- rep(NA,length(index))
  colnames(pairMat) <- rep(1,length(index))
  indMat <- t(sapply(index,function(x){as.numeric(strsplit(x,"<")[[1]])}))
  
  for(i in 1:dim(pairMat)[2]){
    pairMat[,i] <- (dat[indMat[i,1],] <= dat[indMat[i,2],])
    colnames(pairMat)[i] <- paste0("Pair.",i)
    probeName[i] <-  paste0("Pair.",i,": probe ",rownames(dat)[indMat[i,1]]," less than probe ",rownames(dat)[indMat[i,2]])
  }
  
  return(list(pairMat=as.data.frame(pairMat*1),probeName=probeName))
}
