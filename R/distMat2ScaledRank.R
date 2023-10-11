
#' distMat2ScaledRank
#'
#' @description Convert a symmetrical numerical matrix of distances to a matrix of scaled ranks (from 0 to 1).
#' Note: Only the lower side of the matrix is used. Data on the upper right are assumed to be redundant.
#'
#' @param mat A numerical matrix containing distances.
#' @param lower_is_closer Logical indicating that lower is closer.
#'
#' @return A symmetric matrix of ranks. The matrix has the attribute 'lower_is_closer' set to
#' 'TRUE'.
#'
#' @examples
#' \dontrun{
#' mat.dist <- matrix( c( NA, -400, -600, NA, NA, -120, NA, NA, NA ), nrow = 3, ncol = 3 )
#' mat.ranks <- distMat2ScaledRank(mat.dist)
#' }
#' @seealso \code{\link{distMat2Rank}()}
distMat2ScaledRank <- function( mat, lower_is_closer = TRUE ){
  if( "dist" %in% class( mat ) ){
    mat <- as.matrix(mat)
  }
  if( nrow( mat ) != ncol( mat ) ){
    stop( "Error: matrix is not symmetrical. Number of rows differs from number of columns." )
  }
  mat.cp <- matrix( ncol = ncol(mat), nrow = nrow( mat ), dimnames = dimnames( mat ))
  for( j in 1:(ncol(mat)-1) ){ # Cop[ies bottom left half of the distance matrix
    for( i in (j+1):nrow(mat) ){
      mat.cp[i,j] <- mat[i,j]
      #mat.cp[j,i] <- NA
    }
  }
  mat_dist_scaled_rank <- matrix(rank( (2*lower_is_closer - 1) * mat.cp, na.last = "keep" ) / (nrow(mat.cp) * (ncol(mat.cp) - 1) / 2 ),
                                 nrow = nrow(mat.cp), ncol = ncol(mat.cp), dimnames = dimnames( mat.cp ) )
  attr( x = mat_dist_scaled_rank, which = 'lower_is_closer' ) <- TRUE

  # Restore the missing upper right side of matrix
  for( j in 1:(ncol(mat_dist_scaled_rank)-1) ){ # Cop[ies bottom left half of the distance matrix
    for( i in (j+1):nrow(mat_dist_scaled_rank) ){
      mat_dist_scaled_rank[j,i] <- mat_dist_scaled_rank[i,j]
    }
  }
  mat_dist_scaled_rank
}

