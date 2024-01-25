
#' distMat2UnitNormRank negDistMat2UnitNormRank
#'
#' @description Convert a symmetrical numerical matrix of distances to a matrix of scaled ranks (from 0 to 1).
#' Note: Only the lower side of the matrix is used. Data on the upper right are assumed to be redundant. These
#' functions are intended to convert a matrix of distance or similarity values into a proper form for applying
#' hierarchical clustering with the \code{gsnPareNetGenericHierarchic()} function.
#'
#' @param mat A numerical matrix containing distances.
#' @param lower_is_closer Logical indicating that lower is closer.
#'
#' @return A symmetric matrix of ranks. The matrix has the attribute 'lower_is_closer' set to
#' 'TRUE'.
#'
#' @details The difference between \code{distMat2UnitNormRank()} and \code{negDistMat2UnitNormRank()} is that
#' \code{negDistMat2UnitNormRank()} takes only the \code{mat} argument, and negates it prior to calling
#' \code{distMat2UnitNormRank()}.
#'
#' @examples
#'
#' # For log Fisher values, lower is closer and more significant.
#' mat.dist <- matrix( c( NA, -400, -600, NA, NA, -120, NA, NA, NA ), nrow = 3, ncol = 3 )
#' mat.scaledranks <- distMat2UnitNormRank(mat.dist)
#'
#' # With metrics for which higher is closer/more similar, use
#' # negDistMat2UnitNormRank():
#' mat.jaccard <- matrix( c( NA, 0.2, 0.3, NA, NA, 0.1, NA, NA, NA ), nrow = 3, ncol = 3 )
#' mat.srjaccard <- negDistMat2UnitNormRank(mat.jaccard)
#'
#' # This also works:
#' mat.srjaccard <- distMat2UnitNormRank(mat.jaccard, lower_is_closer=FALSE)
#'
#' @seealso \code{\link{distMat2Rank}()}
#' @export
distMat2UnitNormRank <- function( mat, lower_is_closer = TRUE ){
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

#' negDistMat2UnitNormRank
#' @describeIn distMat2UnitNormRank Takes the same parameter distMat2UnitNormRank, but multiplies the distance by -1 first.
#' @export
#'
negDistMat2UnitNormRank <-  function( mat ) distMat2UnitNormRank( mat = - mat )


#' complement
#'
#' @description This function returns the complement of a numeric or integer vector or matrix. This may be suitable as the
#' \code{matrix_scaling_fun()} argument for \code{gsnPareNetGenericHierarchic()} when being used with such distance metrics
#' as the Jaccard Index or Szymkiewicz–Simpson Overlap Coefficients to transform them into something more approximating a
#' distance in behavior.
#'
#' @param x A numeric vector or matrix.
#'
#' @returns The complement of the x argument, equal to \eqn{1 - x}.
#'
#' @details
#' This function also sets matrix or vector attributes appropriately for negation of the input.
#'
#' @export
complement <- function( x ){
  if( isTRUE( attr( x, which = "lower_is_closer") ) ){
    warning( "lower_is_closer attribute already set as TRUE for input matrix." )
  }
  if( max( x, na.rm = TRUE ) > 1 )
    warning( "Some input values are greater than 1. Complement will have negative values." )

  out <- 1 - x
  attr( x = out, which = "distance_type" ) <- NULL
  if( !is.null( attr( x = x, which = "lower_is_closer" ) ) )
    attr( x = out, which = "lower_is_closer" ) <- !attr( x = x, which = "lower_is_closer")
  if( !is.null( attr( x = x, which = "distance" ) ) )
  attr( x = out, which = "distance" ) <- paste0( "complement_", attr( x = x, which = "distance" ) )
  # Remove double complement.
  attr( x = out, which = "distance" ) <- gsub( x = attr( x = out, which = "distance" ),
                                               pattern = "^complement_complement_",
                                               replacement = "" )
  out
}


#' negative
#'
#' @description This function returns the negative of a numeric or integer vector or matrix. This may be suitable as the
#' \code{matrix_scaling_fun()} argument for \code{gsnPareNetGenericHierarchic()} when being used with such distance metrics
#' as the Jaccard Index or Szymkiewicz–Simpson Overlap Coefficients to transform them into something more approximating a
#' distance in behavior.
#'
#' @param x A numeric vector or matrix.
#'
#' @returns The negative of the x argument, equal to \eqn{- x}.
#'
#' @details
#' This function also sets matrix or vector attributes appropriately for negation of the input.
#'
#' @export
negative <- function( x ){
  if( isTRUE( attr( x, which = "lower_is_closer") ) ){
    warning( "lower_is_closer attribute already set as TRUE for input matrix." )
  }
  out <- - x
  attr( x = out, which = "distance_type" ) <- NULL
  if( ! is.null( attr( x = x, which = "lower_is_closer" ) ) )
    attr( x = out, which = "lower_is_closer" ) <- ! attr( x = x, which = "lower_is_closer")
  if( !is.null( attr( x = x, which = "distance" ) ) )
    attr( x = out, which = "distance" ) <- paste0( "negative_", attr( x = x, which = "distance" ) )
  # Remove double negative.
  attr( x = out, which = "distance" ) <- gsub( x = attr( x = out, which = "distance" ),
                                               pattern = "^negative_negative_",
                                               replacement = "" )

  out
}

