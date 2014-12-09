#'
#' Generate a knitr report for model-building using tspreg
#'
#' @param data A matrix of continuous data with features in rows
#' and samples in columns (m x n) to train a model. Rownames must be provided.
#' @param outcome A binary outcome vector of length n
#' @param covar A matrix of covariates to adjust for (n x p)
#' @param val A matrix similar to data on which to validate the trained model.
#' @param val_outcome The binary outcome vector for val
#' @param val_covar The covariate matrix for val
#' @param npair The number of pairs desired in the final model
#' @filepath A character string of where the report should be generated.
#' Default NULL, which generates a file called "{curtime}_output.Rmd" in the
#' working directory.
#'
#' @export
#' @result A report at filepath containing the model-building results

library(knitr)

tspreg_report <- function(data, outcome, covar=NULL, val=NULL, val_outcome=NULL, val_covar=NULL, npair=5, filepath=NULL){
	if(is.null(filepath)){
		filepath <- paste0(getwd(),"/",format(Sys.time(), "%y_%m_%d_%H%M%S"), "_output")
	} else {
		filepath <- paste0(getwd(), "/", filepath)
	}

	knit2html(paste0(system.file("exec", package="tspreg"),"/template.Rmd"), filepath)
}

#tspreg_report(data=exprs(glas_eset), outcome=pData(glas_eset)$FiveYearRecurrence, val=exprs(buyse_eset), val_outcome=pData(buyse_eset)$FiveYearRecurrence, npair=5, filepath="output")