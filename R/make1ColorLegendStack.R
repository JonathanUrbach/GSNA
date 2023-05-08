
make1ColorLegendStack <- function( numbers,
                                   oneColorEncode.fun,
                                   n = 100,
                                   legend_thickness = 15
){
  numbers.domain <- range( numbers, na.rm = TRUE )

  ln1.mat <- matrix( nrow = n+1, ncol = legend_thickness )

  # Both axes go from 0 to n
  seq1.scale <- seq( from = numbers.domain[1], to = numbers.domain[2], length.out = n + 1 )

  for( i in 1:length(seq1.scale) ){
    ln1.mat[i,] <- seq1.scale[i]
  }

  # Linearize ln1.mat and ln2.mat and apply oneColorEncode.fun
  lc12.mat <- oneColorEncode.fun( numbers = as.vector( ln1.mat ), output_as = "array" )

  # Split the channels into 3 matrices (RGB) with n+1 rows and n+1 columns
  r.mat <- apply( X = matrix( data = round(lc12.mat[,1]), nrow = n+1, ncol = legend_thickness ), MARGIN = 2, FUN = rev )
  g.mat <- apply( X = matrix( data = round(lc12.mat[,2]), nrow = n+1, ncol = legend_thickness ), MARGIN = 2, FUN = rev )
  b.mat <- apply( X = matrix( data = round(lc12.mat[,3]), nrow = n+1, ncol = legend_thickness ), MARGIN = 2, FUN = rev )

  rgb.stack <- raster::stack( list( raster::raster( r.mat ) , raster::raster( g.mat ), raster::raster(b.mat) ) )

  raster::extent(rgb.stack) <- raster::extent( 1, legend_thickness, numbers.domain[1], numbers.domain[2] )

  rgb.stack
}
