
# This sizing function maker can be used with both leaf cex's and network vortices. The range of sizes will have different
# values, but the method for making the sizing function is the same.

make_lv_sizing_function <- function( numbers, size_range = c( 0.5, 1.5), log_scale = NULL, log10threshold = 1.0 ){
  numbers.domain <- range( numbers )
  force( size_range )
  force( log_scale )

  if( is.null( log_scale ) )
    log_scale <- guessLogScale( numeric_values = numbers, log10threshold = log10threshold )

  if( log_scale ){
    lnum_range <- log10( numbers.domain )
    return( function( x, return_log_scale_bool = FALSE ){
      if( return_log_scale_bool ) return( log_scale )
      (log10( x ) - lnum_range[1]) * (size_range[2] - size_range[1]) / (lnum_range[2] - lnum_range[1]) + size_range[1]
    } )
  } else {
    return( function( x, return_log_scale_bool = FALSE ){
      if( return_log_scale_bool ) return(  log_scale )
      ( x - numbers.domain[1] ) * (size_range[2] - size_range[1]) / (numbers.domain[2] - numbers.domain[1]) + size_range[1]
    } )
  }
}
