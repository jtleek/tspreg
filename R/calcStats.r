#' Fast calculation of marginal stats
#' 
#' Rapidly calculate statistics for the relationship
#' between the rows of X and the phenotype (factor or survival)
#' 
#' 
#' @param X An input data set
#' @param pheno A factor or survival outcome
#' @param The type of analysis to be performed
#' 
#' @export
#' 
#' @return Test statistics (F-stats or Wald Stats from coxph model)
#'  

calcStats <- function(X,pheno,type="factor"){
  if(type=="factor"){
    stats <- genefilter::rowFtests(X,as.factor(pheno))$statistic
  }
  if(type=="Surv"){
    tmp <- survHD::rowCoxTests(X,pheno)
    stats <- tmp$coef/tmp$se.coef
    stats[!is.finite(stats)] <- 0
  }
  return(stats)
}

