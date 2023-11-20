#' combineRGBMatrices
#'
#' @description Given 2 different matrices of colors, combine the colors numerically. This is used in \code{makeTwoColorEncodeFunction()}.
#'
#' @param c1.mat A numeric matrix with three columns corresponding to red, green, blue, with numerical
#' values ranging from 0 to 255 to be combined numerically with \code{c2.mat}.
#' @param c2.mat A numeric matrix with three columns corresponding to red, green, blue, with numerical
#' values ranging from 0 to 255 to be combined numerically with \code{c1.mat}.
#' @param combine_method Method of combining colors, can be \code{'scaled_geomean'},
#' \code{'standard'}/\code{'euclidean'}, \code{'negative_euclidean'}, \code{'mean'}, \code{'scaled_geomean'}
#' (default = \code{'scaled_geomean'})
#'
#' @param max_per_channel Maximal color value per RGB channel (default 255).
#'
#' @return A 3-column RGB matrix of combined colors.
#'
#' @export
#'
#' @examples
#'
#' c1.mat <- matrix( c(255, 100, 0 ), ncol = 3 )
#' c2.mat <- matrix( c( 0, 50, 255 ), ncol = 3 )
#' c12.mat <- combineRGBMatrices( c1.mat, c2.mat, "euclidean" )
#'
#' @seealso [makeTwoColorEncodeFunction()]
#'
combineRGBMatrices <- function( c1.mat, c2.mat, combine_method = "scaled_geomean", max_per_channel = 255 ){
  if( combine_method == "standard" || combine_method == "euclidean" ){
    c12.mat <- round(sqrt(c1.mat**2 + c2.mat**2))
    c12.mat[c12.mat>max_per_channel] <- max_per_channel
    c12.mat[c12.mat<0] <- 0
  } else if(combine_method == "negative_euclidean"){
    c12.mat <- max_per_channel - round(sqrt((max_per_channel - c1.mat)**2 + (max_per_channel - c2.mat)**2))
    c12.mat[c12.mat>max_per_channel] <- max_per_channel
    c12.mat[c12.mat<0] <- 0
  } else if( combine_method == "mean" ){
    c12.mat <- round( (c1.mat + c2.mat) / 2 )
  } else if( combine_method == "additive" ){
    c12.mat <- (c1.mat + c2.mat)
    c12.mat[c12.mat>max_per_channel] <- max_per_channel
    c12.mat[c12.mat<0] <- 0
  }else if(  combine_method == "scaled_geomean" || combine_method == "default" ){ # scaled_geomean
    c12.mat <- round(max_per_channel * sqrt(c1.mat/max_per_channel) * sqrt(c2.mat/max_per_channel))
  }
  c12.mat
}
