#'
#' Generate a knitr report for model-building using tspreg
#'
#' @param data A matrix of continuous data with features in rows
#' and samples in columns
#' @param outcome A binary outcome vector with length = ncol(data)
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
		filepath <- paste0(format(Sys.time(), "%y_%m_%d_%H%M%S"), "_output")
	}

	knit2html("template.Rmd", filepath)
}

#tspreg_report(data=exprs(glas_eset), outcome=pData(glas_eset)$FiveYearRecurrence, val=exprs(buyse_eset), val_outcome=pData(buyse_eset)$FiveYearRecurrence, npair=5, filepath="output")