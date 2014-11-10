#'
#' Make pairs out of two vectors of rownames (convenience function
#' to be used with empirical_controls.R)
#'
#' This function creates a pair matrix using specifically-provided row names.
#'
#' @param rn_min Rownames for group 1
#' @param rn_max Rownames for group 2
#' @param mat Expression matrix (NOTE: must have rownames)
#' @param n Length of rn_min and rn_max
#'
#' @return A matrix of dimension n*n with pairs generated for
#' every comparision in rn_min to rn_max.

make_pairs <- function(rn_min, rn_max, mat, n){
        tmp <- matrix(NA, n*n, ncol(mat))
        rn_tmp <- vector("character", n*n)
        for(j in 1:length(rn_max)){
                for(k in 1:length(rn_min)){
                        tmp[(n*(j-1) + k),] <- mat[rn_min[k],] < mat[rn_max[j],]
                        rn_tmp[(n*(j-1) + k)] <- paste0(rn_min[k], "<", rn_max[j])
                }
        }
        tmp <- ifelse(tmp, 1, 0)
        rownames(tmp) <- rn_tmp
        tmp
}