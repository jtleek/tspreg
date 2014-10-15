#' 
#' Regression-based feature selection
#' 
#' Leverages the fact that the t-statistic for y~x and
#' x~y is the same to quickly choose pairs that add conditional
#' value for predicting an outcome. 
#' 
#' @param pairmat An m x n matrix of pairwise features with rows=features, columns=samples
#' @param outcome A vector of outcomes of length n
#' @param npair The number of paris we wish to select
#' 
#' @export
#' 
#' @return pairs A vector with the index for each chosen pair
#' 

reg_fs <- function(pairmat, outcome, npair=5){	
	tpm <- pairmat # temp copy
	pairs <- vector("integer", npair)
	# Initial step is for best overall pair
	mod0 <- cbind(rep(1, length(outcome)))
	mod <- cbind(mod0, outcome)
	idx <- 1:nrow(tpm)
	
	for(i in 1:npair){
		fs <- fstats(tpm, mod, mod0)
		cur <- which.max(fs)
		mod0 <- cbind(mod0, tpm[cur,])
		mod <- cbind(mod, tpm[cur,])
		pairs[i] <- idx[cur]
		# Need to dump this and all identical rows
		all_cur <- which(apply(tpm, 1, function(x){all(x == tpm[cur,])}))
		tpm <- tpm[-all_cur,]
		idx <- idx[-all_cur]		
	}

	pairs	
}
