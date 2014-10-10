#' 
#' Calculate f-statistics quickly
#' 
#' Using matrix algebbra, quickly calculate
#' f statistics for comparing nested models
#' 
#' @param dat An input data set
#' @param mod The model matrix for the alternative model to be fit
#' @param mod0 The model matrix for the null model to be fit. If NULL, the null model
#' is the model with only an intercept. 
#' 
#' @export
#' 
#' @return fstats A vector of f-statistics
#' 


fstats <- function(dat,mod,mod0=NULL){
  
  n <- dim(dat)[2]
  m <- dim(dat)[1]
  
  if(is.null(mod0)){
    mod0 <- cbind(rep(1,n))
  }
  
  df1 <- dim(mod)[2]
  df0 <- dim(mod0)[2]
  p <- rep(0,m)
  Id <- diag(n)
  
  resid <- dat %*% (Id - mod %*% solve(t(mod) %*% mod) %*% t(mod))
  rss1 <- rowSums(resid*resid)
  rm(resid)
  gc()
  
  resid0 <- dat %*% (Id - mod0 %*% solve(t(mod0) %*% mod0) %*% t(mod0))
  rss0 <- rowSums(resid0*resid0)
  rm(resid0)
  gc()
  
  fstats <- ((rss0 - rss1)/(df1-df0))/(rss1/(n-df1))
  rm(rss0,rss1)
  gc()
  return(fstats)
}