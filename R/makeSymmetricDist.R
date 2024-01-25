
#' makeSymmetricDist
#'
#' @description Utility function to convert a matrix of non-symmetrical distances (A->B != B->A) into a symmetrical one.
#' A method of aggregating the non-symmetrical distances can be specified. The default aggregation method is
#' \code{mean}.
#'
#' @param mat A non-symmetrical matrix of distances.
#' @param FUN function applied to the non-symmetrical distance pairs to aggregate into a symmetrical distance.
#'
#' @return A symmetrical distance matrix.
#'
#' @examples
#'
#' # Start with a non-symmetrical distance matrix.
#' ns_dist.mat <- matrix( nrow = 3, ncol = 3,
#'                        data = c( NA, -70, -47,
#'                                  -63, NA, -10,
#'                                  -53, -17, NA ) )
#'
#' # Calculate a symmetric distance matrix using 'mean'
#' mean_dist.mat <- makeSymmetricDist( ns_dist.mat, FUN = mean )
#'
#' # minimum or max can also be used:
#' min_dist.mat <- makeSymmetricDist( ns_dist.mat, FUN = min )
#'
#' @export
#'
makeSymmetricDist <- function( mat, FUN = mean ){
  mat.symm <- matrix( nrow = nrow(mat), ncol = ncol(mat), dimnames = dimnames(mat) )
  for( j in 1:(ncol(mat)-1) ){
    for( i in (j+1):nrow(mat)){
      agg <- FUN(c(mat[i,j],mat[j,i]))
      mat.symm[i,j] <- agg
      mat.symm[j,i] <- agg
    }
  }
  mat.symm
}

