#' 
#' Empirical controls calculation function
#' 
#' This function calculates pairs for a data
#' set based on the empirical controls concept.
#' 
#' @param exprs An input matrix of gene expression values
#' @param n In each quantile bin, n genes are picked from the top and bottom
#' 
#' @export
#' 
#' @return A matrix with pairs in rows and samples in columns

empirical_controls = function(exprs, n=25){
  gm <- cbind(rowMeans(exprs), rowSds(exprs))
  qt <- quantile(gm[,1], na.rm=T) # Get cut points
  all_pairs <- all_min <- all_max <- c()
  
  for(i in 1:(length(qt)-1)){
    # Get the ith quantile subset and operate within it
    sub <- gm[which(gm[,1] >= qt[i] & gm[,1] < qt[i+1]),]
    rn <- rownames(sub)
    sd_ord <- order(sub[,2], sub[,1])
    rn_min <- rn[head(sd_ord, n)]
    rn_max <- rn[tail(sd_ord, n)]
    
    # These are features for all low-high and high-high variance pairs
    # all_pairs <- rbind(all_pairs, make_pairs(rn_min, rn_max, exprs, n), make_pairs(rn_max, rn_max, exprs, n))
    
    # These are just the low-high variance pairs
    all_pairs <- rbind(all_pairs, make_pairs(rn_min, rn_max, exprs, n))
  }	
  
  all_pairs
}