#' 
#' Build individual pairs based on rownames
#' 
#' A convenience function to quickly build a small matrix of pairs. Used
#' when we want to predict on a validation set.
#'
#' @param pair A single pairname "A<B", where "A" and "B" are valid rownames.
#' @param mat A matrix with rownames that contain all "A<B" pairs.
#' 
#' @return A vector of 0's and 1's corresponding to the pair "A<B" we want
#' 

single_pairs <- function(pair, mat){
	names <- unlist(strsplit(pair, "<"))
	if(!(names[1] %in% rownames(mat)) | !(names[2] %in% rownames(mat))){
		rep(NA, ncol(mat))	
	} else {
		ifelse(mat[names[1],] - mat[names[2],] < 0, 1, 0)
	}
}