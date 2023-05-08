
#' Title
#'
#' @param numbers.1
#' @param numbers.2
#' @param twoColorEncode.fun
#' @param n
#'
#' @return
#' @export
#'
#' @examples
make2ColorLegendStack <- function( numbers.1,
                                   numbers.2,
                                   twoColorEncode.fun,
                                   n = 100
){
  numbers.1.domain <- range( numbers.1, na.rm = TRUE )
  numbers.2.domain <- range( numbers.2, na.rm = TRUE )

  ln1.mat <- matrix( nrow = n+1, ncol = n+1 )
  ln2.mat <- matrix( nrow = n+1, ncol = n+1 )

  # Both axes go from 0 to n
  seq1.scale <- seq( from = numbers.1.domain[1], to = numbers.1.domain[2], length.out = n + 1 )
  seq2.scale <- seq( from = numbers.2.domain[1], to = numbers.2.domain[2], length.out = n + 1 )

  for( i in 1:length(seq1.scale) ){
    ln1.mat[i,] <- seq1.scale[i]
    ln2.mat[,i] <- seq2.scale[i]
  }

  # Linearize ln1.mat and ln2.mat and apply twoColorEncode.fun
  lc12.mat <- twoColorEncode.fun( numbers.1 = as.vector( ln1.mat ), numbers.2 = as.vector( ln2.mat ), output_as = "array" )

  # Split the channels into 3 matrices (RGB) with n+1 rows and n+1 columns
  r.mat <- apply( X = matrix( data = round(lc12.mat[,1]), nrow = n+1, ncol = n+1 ), MARGIN = 2, FUN = rev )
  g.mat <- apply( X = matrix( data = round(lc12.mat[,2]), nrow = n+1, ncol = n+1 ), MARGIN = 2, FUN = rev )
  b.mat <- apply( X = matrix( data = round(lc12.mat[,3]), nrow = n+1, ncol = n+1 ), MARGIN = 2, FUN = rev )

  rgb.stack <- raster::stack( list( raster::raster( r.mat ) , raster::raster( g.mat ), raster::raster(b.mat) ) )

  raster::extent(rgb.stack) <- raster::extent( numbers.2.domain[1], numbers.2.domain[2],
                                               numbers.1.domain[1], numbers.1.domain[2] )

  rgb.stack
}
