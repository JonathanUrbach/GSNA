#' Title
#'
#' @param c1.mat
#' @param c2.mat
#' @param combine_method
#' @param max_per_channel
#'
#' @return
#' @export
#'
#' @examples
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
    c12.mat <- (c1.mat + c2.mat) / 2
  } else if( combine_method == "additive" ){
    c12.mat <- (c1.mat + c2.mat)
    c12.mat[c12.mat>max_per_channel] <- max_per_channel
    c12.mat[c12.mat<0] <- 0
  }else if(  combine_method == "scaled_geomean" || combine_method == "default" ){ # scaled_geomean
    c12.mat <- round(max_per_channel * sqrt(c1.mat/max_per_channel) * sqrt(c2.mat/max_per_channel))
  }
  c12.mat
}
